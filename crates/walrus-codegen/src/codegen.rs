#![allow(clippy::cast_possible_truncation)]

use arena::ArenaMap;
use either::Either;
use inkwell::{
    builder::Builder,
    context::Context,
    memory_buffer::MemoryBuffer,
    module::Module,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};
use std::ops::Index;
use walrus_semantics::{
    builtins::Builtin,
    hir::{
        self, ArithmeticBinop, Binop, CmpBinop, Expr, ExprId, Field, FnDefId, LazyBinop, Lit,
        Param, PatId, StructExprField, Unop, VarId,
    },
    scopes::{self, Denotation},
    ty,
    ty::{Ctor, FnType, Type},
};

use crate::free_vars::FreeVars;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirModule {
    pub hir: hir::Module,
    pub types: ty::InferenceResult,
    pub scopes: scopes::Scopes,
}

pub struct Compiler<'ctx> {
    pub llvm: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,

    pub hir: hir::ModuleData,
    pub scopes: scopes::Scopes,
    pub types: ty::InferenceResult,

    loop_result: Value<'ctx>,
}

#[derive(Debug, Clone, Default)]
pub struct Vars<'a> {
    locals: ArenaMap<PatId, PointerValue<'a>>,
    fns: ArenaMap<FnDefId, FunctionValue<'a>>,
}

impl<'a> Index<PatId> for Vars<'a> {
    type Output = PointerValue<'a>;
    fn index(&self, id: PatId) -> &Self::Output { &self.locals[id] }
}
impl<'a> Index<FnDefId> for Vars<'a> {
    type Output = FunctionValue<'a>;
    fn index(&self, id: FnDefId) -> &Self::Output { &self.fns[id] }
}

type Value<'ctx> = Option<BasicValueEnum<'ctx>>;

impl<'ctx> Compiler<'ctx> {
    fn void_ptr_type(&self) -> BasicTypeEnum<'ctx> {
        self.llvm.i8_type().ptr_type(AddressSpace::Generic).into()
    }

    fn value_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        let (ctor, params) = match ty {
            Type::App { ctor, params } => (ctor, params),
            _ => unreachable!(format!("{ty:?}")),
        };
        match ctor {
            ty::Ctor::Bool => self.llvm.bool_type().into(),
            ty::Ctor::Int | ty::Ctor::Char => self.llvm.i32_type().into(),
            ty::Ctor::Float => self.llvm.f32_type().into(),
            ty::Ctor::Tuple => {
                let field_types = params
                    .iter()
                    .map(|ty| self.value_type(ty))
                    .collect::<Vec<_>>();
                self.llvm.struct_type(&field_types, false).into()
            }
            ty::Ctor::Fn => self.closure_type(&ty.as_fn().unwrap()),
            ty::Ctor::Struct(id) => {
                let struct_def = &self.hir[*id];
                let field_types = struct_def
                    .fields
                    .iter()
                    .map(|field| {
                        let field_type = &self.types[field.ty];
                        self.value_type(field_type)
                    })
                    .collect::<Vec<_>>();
                self.llvm.struct_type(&field_types, false).into()
            }
            ty::Ctor::Never => unreachable!(),
        }
    }

    fn fn_type(&self, ty: &FnType) -> FunctionType<'ctx> {
        let FnType { params, ret } = ty;
        if ret == &Type::NEVER {
            self.llvm.void_type().fn_type(
                &std::iter::once(self.void_ptr_type())
                    .chain(params.iter().map(|ty| self.value_type(ty)))
                    .collect::<Vec<_>>(),
                false,
            )
        } else {
            self.value_type(ret).fn_type(
                &std::iter::once(self.void_ptr_type())
                    .chain(params.iter().map(|ty| self.value_type(ty)))
                    .collect::<Vec<_>>(),
                false,
            )
        }
    }

    fn closure_type(&self, ty: &FnType) -> BasicTypeEnum<'ctx> {
        let struct_type = self.llvm.struct_type(
            &[
                self.fn_type(ty).ptr_type(AddressSpace::Generic).into(),
                self.void_ptr_type(),
            ],
            false,
        );
        struct_type.into()
    }

    fn tuple_type(&self, tys: &[Type]) -> StructType<'ctx> {
        self.llvm.struct_type(
            &tys.iter().map(|ty| self.value_type(ty)).collect::<Vec<_>>(),
            false,
        )
    }

    fn codegen_module(self) -> Module<'ctx> {
        let builtins_source = include_str!("builtins.ll");
        let builtins =
            MemoryBuffer::create_from_memory_range_copy(builtins_source.as_bytes(), "builtins");
        let builtins = self
            .llvm
            .create_module_from_ir(builtins)
            .map_err(|e| eprintln!("{}", e.to_string()))
            .unwrap();
        self.module.link_in_module(builtins).unwrap();
        let mut vars = Vars::default();

        for (id, func) in self.hir.fn_defs.iter() {
            let fn_type = self.fn_type(&self.types[id]);
            let name = self.hir[func.name].as_str();
            let llvm_fn = self.module.add_function(name, fn_type, None);
            vars.fns.insert(id, llvm_fn);
        }

        for (id, _) in self.hir.fn_defs.iter() {
            self.codegen_fn(&mut vars, id)
        }

        match self.module.verify() {
            Err(e) => {
                eprintln!("{}", self.module.print_to_string().to_string());
                eprintln!("{}", e.to_string());
                panic!()
            }
            Ok(_) => {}
        }

        self.module
    }

    fn codegen_fn(&self, vars: &mut Vars<'ctx>, id: FnDefId) {
        let llvm_fn = vars[id];
        let fn_def = &self.hir[id];
        let name = &self.hir[fn_def.name];
        let bb = self
            .llvm
            .append_basic_block(llvm_fn, &format!("{name}.entry"));
        self.builder.position_at_end(bb);

        llvm_fn
            .get_nth_param(0)
            .unwrap()
            .set_name(&format!("{name}.env"));

        llvm_fn
            .get_param_iter()
            .skip(1)
            .zip(&fn_def.params)
            .enumerate()
            .for_each(|(idx, (llvm_param, hir_param))| {
                llvm_param.set_name(&format!("{name}.params.{idx}"));
                self.codegen_local_var(vars, hir_param.pat, llvm_param)
            });

        if let Some(value) = self.codegen_expr(vars, fn_def.expr) {
            self.builder.build_return(Some(&value));
        }
    }

    fn codegen_local_var(&self, vars: &mut Vars<'ctx>, id: PatId, val: BasicValueEnum) {
        let pat = &self.hir[id];
        let pat_type = &self.types[id];
        match pat {
            hir::Pat::Var(var) => {
                let name = format!("{}.alloca", self.hir[*var]);
                let alloca = self.builder.build_alloca(self.value_type(pat_type), &name);
                vars.locals.insert(id, alloca);
                self.builder.build_store(vars[id], val);
            }
            hir::Pat::Ignore => {
                // `Pat::Ignore` still evaluates its arguments, for side effects
                // eg `let _ = print(5);`
                let alloca = self
                    .builder
                    .build_alloca(self.value_type(pat_type), "_.alloca");
                vars.locals.insert(id, alloca);
                self.builder.build_store(vars[id], val);
            }
            hir::Pat::Tuple(pats) => pats.iter().enumerate().for_each(|(idx, id)| {
                let val = self
                    .builder
                    .build_extract_value(
                        val.into_struct_value(),
                        idx as u32,
                        &format!("tuple.{idx}"),
                    )
                    .unwrap();
                self.codegen_local_var(vars, *id, val)
            }),
        }
    }

    fn codegen_expr(&self, vars: &mut Vars<'ctx>, id: ExprId) -> Value {
        let expr = &self.hir[id];
        match expr {
            Expr::Lit(lit) => Some(self.codegen_lit(*lit)),
            Expr::Var(var) => Some(self.codegen_var(vars, id, *var)),
            Expr::Tuple(exprs) => self.codegen_tuple(vars, id, exprs),
            Expr::Struct { fields, .. } => self.codegen_struct(vars, id, fields),
            Expr::Field { expr, field } => self.codegen_field(vars, *expr, *field),
            Expr::If {
                test,
                then_branch,
                else_branch,
            } => self.codegen_if(vars, *test, *then_branch, *else_branch),
            Expr::Loop(expr) => self.codegen_loop(vars, *expr),
            Expr::Break(_) => todo!(),
            Expr::Continue => todo!(),
            Expr::Return(expr) => self.codegen_return(vars, *expr),
            Expr::Call { func, args } => self.codegen_call(vars, *func, args),
            Expr::Lambda { params, expr } => Some(self.codegen_lambda(vars, id, params, *expr)),
            Expr::Unop { op, expr } => self.codegen_unop(vars, *op, *expr),
            Expr::Binop { lhs, op, rhs } => self.codegen_binop(vars, *lhs, *op, *rhs),
            Expr::Block { stmts, expr } => {
                for stmt in stmts {
                    match stmt {
                        hir::Stmt::Expr(expr) => {
                            self.codegen_expr(vars, *expr)?;
                        }
                        hir::Stmt::Let { pat, expr, .. } => {
                            let val = self.codegen_expr(vars, *expr)?;
                            self.codegen_local_var(vars, *pat, val);
                        }
                    }
                }
                match expr {
                    Some(expr) => self.codegen_expr(vars, *expr),
                    None => Some(self.codegen_unit()),
                }
            }
        }
    }

    fn codegen_unit(&self) -> BasicValueEnum { self.llvm.const_struct(&[], false).into() }

    fn codegen_undef(&self) -> BasicValueEnum { self.llvm.i8_type().get_undef().into() }

    fn codegen_lit(&self, lit: Lit) -> BasicValueEnum {
        match lit {
            Lit::Bool(false) => self.llvm.bool_type().const_int(0, false).into(),
            Lit::Bool(true) => self.llvm.bool_type().const_int(1, false).into(),
            Lit::Int(val) => self.llvm.i32_type().const_int(val.into(), false).into(),
            Lit::Float(val) => self.llvm.f32_type().const_float(val.0.into()).into(),
            Lit::Char(val) => self.llvm.i32_type().const_int(val.into(), false).into(),
        }
    }

    fn codegen_var(&self, vars: &Vars<'ctx>, expr: ExprId, var: VarId) -> BasicValueEnum {
        let var = &self.hir[var];
        let denotation = self.scopes.lookup_expr(expr, var);
        match denotation {
            Some(Denotation::Local(id)) => self.builder.build_load(vars[id], var.as_str()),
            Some(Denotation::Fn(id)) => {
                let fn_def = &self.hir[id];
                let fn_name = &self.hir[fn_def.name].as_str();
                let fn_type = self.types[expr].as_fn().unwrap();
                let fn_value = vars[id];
                self.codegen_fn_value(fn_name, fn_value, fn_type)
            }
            Some(Denotation::Builtin(b)) => self.codegen_builtin(b),
            _ => unreachable!(),
        }
    }

    fn codegen_fn_value(
        &self,
        fn_name: &str,
        fn_value: FunctionValue,
        fn_type: FnType,
    ) -> BasicValueEnum {
        let code_ptr = fn_value.as_global_value().as_pointer_value();
        let closure_alloca = self.builder.build_alloca(
            self.closure_type(&fn_type),
            &format!("{fn_name}.closure.alloca"),
        );

        let code_gep = self
            .builder
            .build_struct_gep(closure_alloca, 0, &format!("{fn_name}.closure.code"))
            .unwrap();
        self.builder.build_store(code_gep, code_ptr);

        let env_gep = self
            .builder
            .build_struct_gep(closure_alloca, 1, &format!("{fn_name}.closure.env"))
            .unwrap();
        let null_ptr = self.void_ptr_type().const_zero();
        self.builder.build_store(env_gep, null_ptr);
        self.builder.build_load(closure_alloca, fn_name)
    }

    fn codegen_builtin(&self, builtin: Builtin) -> BasicValueEnum {
        match builtin {
            Builtin::Bool | Builtin::Int | Builtin::Float | Builtin::Char | Builtin::Never => {
                unreachable!()
            }
            Builtin::Exit => {
                let exit_wrapper_fn = self.module.get_function("builtins.exit.wrapper").unwrap();
                self.codegen_fn_value("exit", exit_wrapper_fn, builtin.ty().as_fn().unwrap())
            }
            Builtin::PutChar => {
                let wrapper_fn = self
                    .module
                    .get_function("builtins.putchar.wrapper")
                    .unwrap();
                self.codegen_fn_value("putchar", wrapper_fn, builtin.ty().as_fn().unwrap())
            }
        }
    }

    fn codegen_tuple(&self, vars: &mut Vars<'ctx>, expr: ExprId, exprs: &[ExprId]) -> Value {
        let types = self.types[expr].as_tuple().unwrap();
        let tuple_type = self.tuple_type(types);
        let tuple_alloca = self.builder.build_alloca(tuple_type, "tuple.alloca");
        for (idx, expr) in exprs.iter().enumerate() {
            let value = self.codegen_expr(vars, *expr)?;
            let gep = self
                .builder
                .build_struct_gep(tuple_alloca, idx as u32, &format!("tuple.{idx}"))
                .unwrap();
            self.builder.build_store(gep, value);
        }
        Some(self.builder.build_load(tuple_alloca, "tuple"))
    }

    fn codegen_struct(
        &self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        fields: &[StructExprField],
    ) -> Value {
        let struct_id = self.types[expr].as_struct().unwrap();
        let struct_def = &self.hir[struct_id];
        let struct_name = &self.hir[struct_def.name];

        let struct_type = &self.types[expr];
        let struct_type = self.value_type(struct_type);
        let init_exprs = fields
            .iter()
            .map(|field| (&self.hir[field.name], self.codegen_expr(vars, field.val)))
            .collect::<Vec<_>>();
        let struct_alloca = self
            .builder
            .build_alloca(struct_type, &format!("{struct_name}.alloca"));
        for (idx, field) in struct_def.fields.iter().enumerate() {
            let field_name = &self.hir[field.name];
            let gep = self
                .builder
                .build_struct_gep(
                    struct_alloca,
                    idx as u32,
                    &format!("{struct_name}.{field_name}"),
                )
                .unwrap();
            let (_, value) = init_exprs
                .iter()
                .find(|(name, _)| name == &field_name)
                .unwrap();
            let value = value;
            self.builder.build_store(gep, (*value)?);
        }
        Some(self.builder.build_load(struct_alloca, struct_name.as_str()))
    }

    fn codegen_field(&self, vars: &mut Vars<'ctx>, expr: ExprId, field: Field) -> Value {
        let base_value = self.codegen_expr(vars, expr)?;
        let value = match field {
            Field::Tuple(idx) => self
                .builder
                .build_extract_value(base_value.into_struct_value(), idx, &format!("tuple.{idx}"))
                .unwrap(),
            Field::Named(name) => {
                let name = &self.hir[name];
                let struct_id = self.types[expr].as_struct().unwrap();
                let struct_def = &self.hir[struct_id];
                let struct_name = &self.hir[struct_def.name];
                let idx = struct_def
                    .fields
                    .iter()
                    .position(|field| &self.hir[field.name] == name)
                    .unwrap();
                self.builder
                    .build_extract_value(
                        base_value.into_struct_value(),
                        idx as u32,
                        &format!("{struct_name}.{name}"),
                    )
                    .unwrap()
            }
        };
        Some(value)
    }

    fn codegen_if(
        &self,
        vars: &mut Vars<'ctx>,
        test: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    ) -> Value {
        let bb = self.builder.get_insert_block().unwrap();
        let end_bb = self.llvm.insert_basic_block_after(bb, "if.end");
        let else_bb = self.llvm.insert_basic_block_after(bb, "if.else");
        let then_bb = self.llvm.insert_basic_block_after(bb, "if.then");
        let test_value = self.codegen_expr(vars, test)?;
        self.builder
            .build_conditional_branch(test_value.into_int_value(), then_bb, else_bb);

        // then branch
        self.builder.position_at_end(then_bb);
        let then_value = match else_branch {
            Some(_) => self
                .codegen_expr(vars, then_branch)
                .unwrap_or_else(|| self.codegen_undef()),
            None => {
                self.codegen_expr(vars, then_branch);
                self.codegen_unit()
            }
        };
        self.builder.build_unconditional_branch(end_bb);

        // else branch
        self.builder.position_at_end(else_bb);
        let else_value = match else_branch {
            Some(else_branch) => self
                .codegen_expr(vars, else_branch)
                .unwrap_or_else(|| self.codegen_undef()),
            None => self.codegen_unit(),
        };
        self.builder.build_unconditional_branch(end_bb);

        // merge the 2 branches
        self.builder.position_at_end(end_bb);
        let ty = match else_branch {
            Some(_) => self.value_type(&self.types[then_branch]),
            None => self.tuple_type(&[]).into(),
        };
        let phi = self.builder.build_phi(ty, "if.merge");
        phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);
        Some(phi.as_basic_value())
    }

    fn codegen_loop(&self, vars: &mut Vars<'ctx>, expr: ExprId) -> Value {
        let old_bb = self.builder.get_insert_block().unwrap();
        let loop_bb = self.llvm.insert_basic_block_after(old_bb, "loop");
        self.builder.build_unconditional_branch(loop_bb);

        self.builder.position_at_end(loop_bb);
        let body = self.codegen_expr(vars, expr);
        self.builder.build_unconditional_branch(loop_bb);

        self.builder.position_at_end(old_bb);

        self.loop_result
    }

    fn codegen_call(&self, vars: &mut Vars<'ctx>, func: ExprId, args: &[ExprId]) -> Value {
        let closure_value = self.codegen_expr(vars, func)?.into_struct_value();
        let code_ptr = self
            .builder
            .build_extract_value(closure_value, 0, "closure.code")
            .unwrap()
            .into_pointer_value();
        let env_ptr = self
            .builder
            .build_extract_value(closure_value, 1, "closure.env")
            .unwrap();
        let args = &std::iter::once(Some(env_ptr))
            .chain(args.iter().map(|arg| self.codegen_expr(vars, *arg)))
            .collect::<Option<Vec<_>>>()?;
        match self
            .builder
            .build_call(code_ptr, args, "call")
            .try_as_basic_value()
        {
            Either::Left(value) => Some(value),
            Either::Right(_) => {
                self.builder.build_unreachable();
                None
            }
        }
    }

    fn codegen_lambda(
        &self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        params: &[Param],
        body: ExprId,
    ) -> BasicValueEnum {
        let free_vars = self.free_vars(expr);

        let code_ptr = self
            .codegen_lambda_body(
                // this clone is necessary. Dont delete it!
                &mut vars.clone(),
                &free_vars,
                expr,
                params,
                body,
            )
            .as_global_value()
            .as_pointer_value();

        let closure_type = self.types[expr].as_fn().unwrap();
        let closure_alloca = self
            .builder
            .build_alloca(self.closure_type(&closure_type), "closure.alloca");
        let code_gep = self
            .builder
            .build_struct_gep(closure_alloca, 0, "closure.code")
            .unwrap();
        self.builder.build_store(code_gep, code_ptr);

        let env_gep = self
            .builder
            .build_struct_gep(closure_alloca, 1, "closure.env")
            .unwrap();

        // store free variables
        let free_vars_type = self.tuple_type(
            &free_vars
                .iter()
                .map(|(pat, _)| self.types[pat].clone())
                .collect::<Vec<_>>(),
        );
        let env_alloca = self.builder.build_alloca(free_vars_type, "env.alloca");
        for (idx, (free_var, ())) in free_vars.iter().enumerate() {
            let gep = self
                .builder
                .build_struct_gep(env_alloca, idx as u32, &format!("env.{idx}.gep"))
                .unwrap();
            let val = self
                .builder
                .build_load(vars[free_var], &format!("env.{idx}"));
            self.builder.build_store(gep, val);
        }
        let env_alloca = self
            .builder
            .build_bitcast(env_alloca, self.void_ptr_type(), "env");
        self.builder.build_store(env_gep, env_alloca);
        self.builder.build_load(closure_alloca, "closure")
    }

    fn codegen_lambda_body(
        &self,
        vars: &mut Vars<'ctx>,
        free_vars: &FreeVars,
        expr: ExprId,
        params: &[Param],
        body: ExprId,
    ) -> FunctionValue {
        let fn_type = self.fn_type(&self.types[expr].as_fn().unwrap());
        let llvm_fn = self.module.add_function("lambda", fn_type, None);
        let old_bb = self.builder.get_insert_block().unwrap();

        let bb = self.llvm.append_basic_block(llvm_fn, "lambda.entry");
        self.builder.position_at_end(bb);

        // load free vars
        let free_vars_type = self.tuple_type(
            &free_vars
                .iter()
                .map(|(pat, _)| self.types[pat].clone())
                .collect::<Vec<_>>(),
        );

        let env_param = llvm_fn.get_nth_param(0).unwrap();
        env_param.set_name("env_ptr");
        let env_ptr = self.builder.build_bitcast(
            env_param,
            free_vars_type.ptr_type(AddressSpace::Generic),
            "env_ptr",
        );
        let env = self.builder.build_load(env_ptr.into_pointer_value(), "env");
        for (idx, (free_var, ())) in free_vars.iter().enumerate() {
            let val = self
                .builder
                .build_extract_value(env.into_struct_value(), idx as u32, &format!("env.{idx}"))
                .unwrap();
            self.codegen_local_var(vars, free_var, val)
        }

        // load params
        llvm_fn
            .get_param_iter()
            .skip(1)
            .zip(params)
            .enumerate()
            .for_each(|(idx, (llvm_param, hir_param))| {
                llvm_param.set_name(&format!("params.{idx}"));
                self.codegen_local_var(vars, hir_param.pat, llvm_param)
            });

        if let Some(value) = self.codegen_expr(vars, body) {
            self.builder.build_return(Some(&value));
        }

        self.builder.position_at_end(old_bb);

        llvm_fn
    }

    fn codegen_return(&self, vars: &mut Vars<'ctx>, expr: Option<ExprId>) -> Value {
        let value = match expr {
            Some(expr) => self.codegen_expr(vars, expr)?,
            None => self.codegen_unit(),
        };
        self.builder.build_return(Some(&value));
        None
    }

    fn codegen_unop(&self, vars: &mut Vars<'ctx>, op: Unop, expr: ExprId) -> Value {
        let value = self.codegen_expr(vars, expr)?;
        let value = match op {
            Unop::Not => self
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    value.into_int_value(),
                    self.llvm.bool_type().const_int(0, false),
                    "unary_not",
                )
                .into(),
            Unop::Sub => self
                .builder
                .build_int_neg(value.into_int_value(), "unary_neg")
                .into(),
            Unop::Add => value,
        };
        Some(value)
    }

    fn codegen_lazy_binop(
        &self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        op: LazyBinop,
        rhs: ExprId,
    ) -> Value {
        let bool_type = self.llvm.bool_type();
        let bb = self.builder.get_insert_block().unwrap();
        let end_bb = self.llvm.insert_basic_block_after(bb, &format!("{op}.end"));
        let else_bb = self
            .llvm
            .insert_basic_block_after(bb, &format!("{op}.else"));
        let then_bb = self
            .llvm
            .insert_basic_block_after(bb, &format!("{op}.then"));
        let lhs_value = self.codegen_expr(vars, lhs)?;
        self.builder
            .build_conditional_branch(lhs_value.into_int_value(), then_bb, else_bb);

        // then branch
        self.builder.position_at_end(then_bb);
        self.builder.build_unconditional_branch(end_bb);

        // else branch
        self.builder.position_at_end(else_bb);
        let rhs_value = self.codegen_expr(vars, rhs)?;
        self.builder.build_unconditional_branch(end_bb);

        // merge the 2 branches
        self.builder.position_at_end(end_bb);
        let phi = self.builder.build_phi(bool_type, &format!("{op}.merge"));
        match op {
            LazyBinop::Or => phi.add_incoming(&[(&lhs_value, then_bb), (&rhs_value, else_bb)]),
            LazyBinop::And => phi.add_incoming(&[(&rhs_value, then_bb), (&lhs_value, else_bb)]),
        }
        Some(phi.as_basic_value())
    }

    fn codegen_arithmetic_binop(
        &self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        op: ArithmeticBinop,
        rhs: ExprId,
    ) -> Value {
        let lhs_value = self.codegen_expr(vars, lhs)?;
        let rhs_value = self.codegen_expr(vars, rhs)?;

        let lhs_ty = &self.types[lhs];
        let rhs_ty = &self.types[rhs];

        let ctor = self.types[lhs].ctor().unwrap();

        #[rustfmt::skip]
        macro_rules! int_op {
            ($op:ident) => {self.builder .$op(lhs_value.into_int_value(), rhs_value.into_int_value(), "") .into()};
        }

        #[rustfmt::skip]
        macro_rules! float_op {
            ($op:ident) => {self.builder.$op(lhs_value.into_float_value(), rhs_value.into_float_value(), "") .into()};
        }

        let value = match (ctor, op) {
            (Ctor::Int, ArithmeticBinop::Add) => int_op!(build_int_add),
            (Ctor::Int, ArithmeticBinop::Sub) => int_op!(build_int_sub),
            (Ctor::Int, ArithmeticBinop::Mul) => int_op!(build_int_mul),
            (Ctor::Int, ArithmeticBinop::Div) => int_op!(build_int_signed_div),
            (Ctor::Float, ArithmeticBinop::Add) => float_op!(build_float_add),
            (Ctor::Float, ArithmeticBinop::Sub) => float_op!(build_float_sub),
            (Ctor::Float, ArithmeticBinop::Mul) => float_op!(build_float_mul),
            (Ctor::Float, ArithmeticBinop::Div) => float_op!(build_float_div),

            _ => unreachable!(format!("cannot perform binop {lhs_ty:?} {op} {rhs_ty:?}")),
        };
        Some(value)
    }

    fn codegen_cmp_binop(
        &self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        op: CmpBinop,
        rhs: ExprId,
    ) -> Value {
        let lhs_value = self.codegen_expr(vars, lhs)?;
        let rhs_value = self.codegen_expr(vars, rhs)?;

        let lhs_ty = &self.types[lhs];
        let rhs_ty = &self.types[rhs];

        let ctor = self.types[lhs].ctor().unwrap();

        #[rustfmt::skip]
        macro_rules! int_cmp {
            ($op:expr) => {self.builder.build_int_compare($op,lhs_value.into_int_value(), rhs_value.into_int_value(), "") .into()};
        }

        #[rustfmt::skip]
        macro_rules! float_cmp {
            ($op:expr) => {self.builder.build_float_compare($op,lhs_value.into_float_value(), rhs_value.into_float_value(), "") .into()};
        }

        let value = match (ctor, op) {
            (Ctor::Bool | Ctor::Int | Ctor::Char, CmpBinop::Eq) => {
                int_cmp!(IntPredicate::EQ)
            }
            (Ctor::Bool | Ctor::Int | Ctor::Char, CmpBinop::NotEq) => {
                int_cmp!(IntPredicate::NE)
            }
            (Ctor::Bool | Ctor::Int | Ctor::Char, CmpBinop::Less) => {
                int_cmp!(IntPredicate::SLT)
            }
            (Ctor::Bool | Ctor::Int | Ctor::Char, CmpBinop::LessEq) => {
                int_cmp!(IntPredicate::SLE)
            }
            (Ctor::Bool | Ctor::Int | Ctor::Char, CmpBinop::Greater) => {
                int_cmp!(IntPredicate::SGT)
            }
            (Ctor::Bool | Ctor::Int | Ctor::Char, CmpBinop::GreaterEq) => {
                int_cmp!(IntPredicate::SGE)
            }

            (Ctor::Float, CmpBinop::Eq) => {
                float_cmp!(FloatPredicate::OEQ)
            }
            (Ctor::Float, CmpBinop::NotEq) => {
                float_cmp!(FloatPredicate::ONE)
            }
            (Ctor::Float, CmpBinop::Less) => {
                float_cmp!(FloatPredicate::OLT)
            }
            (Ctor::Float, CmpBinop::LessEq) => {
                float_cmp!(FloatPredicate::OLE)
            }
            (Ctor::Float, CmpBinop::Greater) => {
                float_cmp!(FloatPredicate::OGT)
            }
            (Ctor::Float, CmpBinop::GreaterEq) => {
                float_cmp!(FloatPredicate::OGE)
            }

            _ => unreachable!(format!("cannot perform binop {lhs_ty:?} {op} {rhs_ty:?}")),
        };
        Some(value)
    }

    fn codegen_binop(&self, vars: &mut Vars<'ctx>, lhs: ExprId, op: Binop, rhs: ExprId) -> Value {
        match op {
            Binop::Lazy(op) => self.codegen_lazy_binop(vars, lhs, op, rhs),
            Binop::Arithmetic(op) => self.codegen_arithmetic_binop(vars, lhs, op, rhs),
            Binop::Cmp(op) => self.codegen_cmp_binop(vars, lhs, op, rhs),
            Binop::Assign => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::OptimizationLevel;
    use insta::*;

    macro_rules! test_codegen_and_run {
        ($name:ident, $src:expr, $expected:expr) => {
            #[test]
            fn $name() { test_codegen_and_run($src, $expected); }
        };
    }

    #[track_caller]
    fn test_codegen_and_run<T>(src: &str, expected: T)
    where
        T: PartialEq + std::fmt::Debug,
    {
        let syntax = walrus_parser::parse(src);
        let hir = walrus_semantics::hir::lower(&syntax);
        let scopes = walrus_semantics::scopes::scopes(&hir);
        let types = walrus_semantics::ty::infer(hir.clone(), scopes.clone());
        dbg!(&syntax);
        dbg!(&hir);
        dbg!(&scopes);
        dbg!(&types);

        let llvm = Context::create();
        let builder = llvm.create_builder();
        let module = llvm.create_module("module");

        let llvm_module = {
            let compiler = Compiler {
                llvm: &llvm,
                module,
                builder,

                hir: hir.data,
                scopes,
                types,

                loop_result: None,
            };
            compiler.codegen_module()
        };

        let exec_engine = llvm_module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let f = unsafe { exec_engine.get_function::<unsafe extern "C" fn() -> T>("main") }.unwrap();
        assert_eq!(unsafe { f.call() }, expected);

        let mut settings = insta::Settings::new();
        settings.set_snapshot_path("../snapshots");
        settings.set_prepend_module_to_snapshot(false);
        settings.bind(|| assert_display_snapshot!(llvm_module.print_to_string().to_string()));
    }

    test_codegen_and_run!(empty_fn, r#"fn main() -> _ {}"#, ());
    test_codegen_and_run!(lit_true, r#"fn main() -> _ { true }"#, true);
    test_codegen_and_run!(lit_false, r#"fn main() -> _ { false }"#, false);
    test_codegen_and_run!(lit_int, r#"fn main() -> _ { 1 }"#, 1_i32);
    test_codegen_and_run!(lit_float, r#"fn main() -> _ { 1.234 }"#, 1.234_f32);
    test_codegen_and_run!(lit_char, r#"fn main() -> _ { 'a' }"#, 'a');

    test_codegen_and_run!(tuple0, r#"fn main() -> _ { () }"#, ());
    test_codegen_and_run!(tuple1, r#"fn main() -> _ { (1,) }"#, (1_i32,));

    // TODO: fails
    #[cfg(FALSE)]
    test_codegen_and_run!(tuple2, r#"fn main() -> _ { (1,2) }"#, (1_i32, 2_i32));

    #[cfg(FALSE)]
    test_codegen_and_run!(
        tuple3,
        r#"fn main() -> _ { (1,2,3) }"#,
        (1_i32, 2_i32, 3_i32)
    );

    #[cfg(FALSE)]
    test_codegen_and_run!(
        tuple4,
        r#"fn main() -> _ { (1,2,3,4) }"#,
        (1_i32, 2_i32, 3_i32, 4_i32)
    );

    test_codegen_and_run!(
        if_then_else_true,
        r#"fn main() -> _ { if true {1} else {0} }"#,
        1_i32
    );
    test_codegen_and_run!(
        if_then_else_false,
        r#"fn main() -> _ { if false {1} else {0} }"#,
        0_i32
    );

    test_codegen_and_run!(if_then_true, r#"fn main() -> _ { if true {1} }"#, ());
    test_codegen_and_run!(if_then_false, r#"fn main() -> _ { if false {1} }"#, ());

    test_codegen_and_run!(let_var, r#"fn main() -> _ { let x = 5; x }"#, 5_i32);
    test_codegen_and_run!(
        let_tuple2,
        r#"fn main() -> _ { let (x, y) = (5, 6); x }"#,
        5_i32
    );
    test_codegen_and_run!(
        let_tuple4,
        r#"
fn main() -> _ {
    let (a, b, c, d) = (9, 8, 7, 6);
    let (x,(y, z), w) = (d, (c, b), a);
    x
}"#,
        6_i32
    );

    test_codegen_and_run!(
        id_fn,
        r#"
fn main() -> _ { id(5) }
fn id(x: _) -> _ {x}
"#,
        5_i32
    );

    test_codegen_and_run!(
        get_five_fn,
        r#"
fn main() -> _ {get_five()}
fn get_five() -> _ {5}
"#,
        5_i64
    );

    test_codegen_and_run!(
        early_return,
        r#"
fn main() -> _ {
    return 5;
    6
}
"#,
        5_i64
    );

    test_codegen_and_run!(
        multiple_return,
        r#"
fn main() -> _ {
    return 5;
    return 6;
}
"#,
        5_i64
    );

    // TODO
    #[cfg(FALSE)]
    test_codegen_and_run!(
        if_then_return,
        r#"
fn main() -> _ {
    if true {return 1}
    else {0}
}
"#,
        1
    );

    test_codegen_and_run!(multi_expr_return, r#"fn main() -> _ { 1 + (return 2) }"#, 2);

    test_codegen_and_run!(
        struct_construtor,
        r#"
struct Foo {x: Int}
fn main() -> _ {
    let foo = Foo {x:5};
    foo
}
"#,
        5_i32
    );

    test_codegen_and_run!(
        struct_field,
        r#"
struct Foo {x: Int,y:Int}
fn main() -> _ {
    let foo = Foo {y:6, x:5};
    foo.y
}
"#,
        6_i32
    );

    test_codegen_and_run!(binop_or, r#"fn main() -> _  {false || true}"#, true);
    test_codegen_and_run!(binop_and, r#"fn main() -> _ {true && false}"#, false);

    test_codegen_and_run!(
        int_add,
        r#"fn main() -> _ {let x = 1; let y = 1; x + y}"#,
        2_i32
    );
    test_codegen_and_run!(
        int_sub,
        r#"fn main() -> _ {let x = 1; let y = 1; x - y}"#,
        0_i32
    );
    test_codegen_and_run!(
        int_mul,
        r#"fn main() -> _ {let x = 1; let y = 2; x * y}"#,
        2_i32
    );
    test_codegen_and_run!(
        int_div,
        r#"fn main() -> _ {let x = 5; let y = 2; x / y}"#,
        2_i32
    );

    test_codegen_and_run!(
        float_add,
        r#"fn main() -> _ {let x = 1.0; let y = 1.0; x + y}"#,
        2.0_f32
    );
    test_codegen_and_run!(
        float_sub,
        r#"fn main() -> _ {let x = 1.0; let y = 1.0; x - y}"#,
        0.0_f32
    );
    test_codegen_and_run!(
        float_mul,
        r#"fn main() -> _ {let x = 1.0; let y = 2.0; x * y}"#,
        2.0_f32
    );
    test_codegen_and_run!(
        float_div,
        r#"fn main() -> _ {let x = 5.0; let y = 2.0; x / y}"#,
        2.5_f32
    );

    test_codegen_and_run!(
        int_eq,
        r#"fn main() -> _ {let x = 1; let y = 1; x == y}"#,
        true
    );
    test_codegen_and_run!(
        int_neq,
        r#"fn main() -> _ {let x = 1; let y = 1; x != y}"#,
        false
    );
    test_codegen_and_run!(
        int_less,
        r#"fn main() -> _ {let x = 1; let y = 1; x < y}"#,
        false
    );
    test_codegen_and_run!(
        int_less_eq,
        r#"fn main() -> _ {let x = 1; let y = 1; x <= y}"#,
        true
    );
    test_codegen_and_run!(
        int_greater,
        r#"fn main() -> _ {let x = 1; let y = 1; x > y}"#,
        false
    );
    test_codegen_and_run!(
        int_greater_eq,
        r#"fn main() -> _ {let x = 1; let y = 1; x >= y}"#,
        true
    );

    test_codegen_and_run!(
        lambda_no_free_vars,
        r#"fn main() -> _ {
let get_five = () => 5;
get_five()
}"#,
        5_i32
    );

    test_codegen_and_run!(
        lambda_args,
        r#"fn main() -> _ {
let add = (x,y) => x+y;
add(2,3)
}"#,
        5_i32
    );

    test_codegen_and_run!(
        lambda_free_vars,
        r#"fn main() -> _ {
let x = 5;
let capture_x = () => x;
capture_x()
}"#,
        5_i32
    );

    test_codegen_and_run!(
        return_lambda,
        r#"
fn const_(k) -> _ { (_) => k }

fn main() -> _ {
    let const_five = const_(5);
    const_five(0)
}
"#,
        5_i32
    );

    // TODO: this sucessfully exits, which makes the test fail!
    #[cfg(FALSE)]
    test_codegen_and_run!(
        builtin_exit,
        r#"
fn main() -> Int {exit(1)}
"#,
        0_i32
    );

    test_codegen_and_run!(
        builtin_putchar,
        r#"
fn main() -> () {
    putchar('h');
    putchar('e');
    putchar('l');
    putchar('l');
    putchar('o');
    putchar(' ');
    putchar('w');
    putchar('o');
    putchar('r');
    putchar('l');
    putchar('d');
}
"#,
        ()
    );

    // TODO: this successfully loops forever, which makes the test never complete!
    #[cfg(FALSE)]
    test_codegen_and_run!(
        loop_forever,
        r#"
fn main() -> () {
    loop {}
}
"#,
        ()
    );
}
