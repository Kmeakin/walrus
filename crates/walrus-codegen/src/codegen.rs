#![allow(clippy::cast_possible_truncation)]

use arena::ArenaMap;
use either::{self, *};
pub use inkwell::context::Context;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    memory_buffer::MemoryBuffer,
    module::Module,
    targets::TargetData,
    types::{AnyType, BasicType, BasicTypeEnum, FunctionType, IntType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};
use std::{collections::HashMap, ops::Index};
use walrus_semantics::{
    builtins::Builtin,
    hir::{
        self, ArithmeticBinop, Binop, CmpBinop, EnumDefId, Expr, ExprId, Field, FieldInit, FnDefId,
        LazyBinop, Lit, MatchCase, Param, Pat, PatId, StructDefId, StructField, Unop, VarId,
    },
    scopes::{self, Denotation},
    ty,
    ty::{FnType, PrimitiveType, Type},
};

use crate::free_vars::FreeVars;

mod types;
mod util;

pub struct Compiler<'ctx> {
    pub llvm: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,

    pub hir: hir::HirData,
    pub scopes: scopes::Scopes,
    pub types: ty::InferenceResult,
}

#[derive(Debug, Clone)]
pub struct Loop<'ctx> {
    body_bb: BasicBlock<'ctx>,
    exit_bb: BasicBlock<'ctx>,
    result_alloca: PointerValue<'ctx>,

    does_continue: bool,
    does_break: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Vars<'a> {
    locals: ArenaMap<VarId, PointerValue<'a>>,
    fns: ArenaMap<FnDefId, FunctionValue<'a>>,
    types: HashMap<Either<StructDefId, EnumDefId>, StructType<'a>>,
    current_loop: Option<Loop<'a>>,
}

impl<'a> Index<VarId> for Vars<'a> {
    type Output = PointerValue<'a>;
    fn index(&self, id: VarId) -> &Self::Output { &self.locals[id] }
}
impl<'a> Index<FnDefId> for Vars<'a> {
    type Output = FunctionValue<'a>;
    fn index(&self, id: FnDefId) -> &Self::Output { &self.fns[id] }
}

type Value<'ctx> = Option<BasicValueEnum<'ctx>>;

impl<'ctx> Compiler<'ctx> {
    pub fn codegen_module(self) -> String {
        let this: &'static Compiler<'static> = unsafe { std::mem::transmute(&self) };

        let builtins_source = include_str!("builtins.ll");
        let builtins =
            MemoryBuffer::create_from_memory_range_copy(builtins_source.as_bytes(), "builtins");
        let builtins = this
            .llvm
            .create_module_from_ir(builtins)
            .map_err(|e| eprintln!("{}", e.to_string()))
            .unwrap();
        this.module.link_in_module(builtins).unwrap();
        let mut vars = Vars::default();

        for (id, func) in this.hir.fn_defs.iter() {
            let fn_type = this.fn_type(&mut vars, &this.types[id]);
            let name = this.hir[func.name].as_str();
            let llvm_fn = this.module.add_function(name, fn_type, None);
            vars.fns.insert(id, llvm_fn);
        }

        for (id, _) in this.hir.fn_defs.iter() {
            this.codegen_fn(&mut vars, id)
        }

        if let Err(e) = this.module.verify() {
            eprintln!("{}", this.module.print_to_string().to_string());
            eprintln!("{}", e.to_string());
            panic!()
        }

        this.module.print_to_string().to_string()
    }

    fn codegen_fn(&'ctx self, vars: &mut Vars<'ctx>, id: FnDefId) {
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
                self.codegen_local_pat(vars, hir_param.pat, llvm_param)
            });

        if let Some(value) = self.codegen_expr(vars, fn_def.expr) {
            self.builder.build_return(Some(&value));
        }
    }

    fn codegen_local_var(&self, vars: &mut Vars<'ctx>, var_id: VarId, val: BasicValueEnum) {
        let var_type = &self.types[var_id];
        let name = format!("{}.alloca", self.hir[var_id]);
        let alloca = self
            .builder
            .build_alloca(self.value_type(vars, var_type), &name);
        vars.locals.insert(var_id, alloca);
        self.builder.build_store(alloca, val);
    }

    fn codegen_local_pat(&self, vars: &mut Vars<'ctx>, id: PatId, val: BasicValueEnum<'ctx>) {
        let pat = &self.hir[id];
        assert!(pat.is_infalliable(&self.hir));
        match pat {
            hir::Pat::Ignore => {}
            hir::Pat::Var(var) => self.codegen_local_var(vars, *var, val),
            hir::Pat::Tuple(pats) => pats.iter().enumerate().for_each(|(idx, id)| {
                let val = self.get_tuple_field(val, idx);
                self.codegen_local_pat(vars, *id, val)
            }),
            hir::Pat::Struct { fields, .. } => {
                let struct_id = self.types[id].as_struct().unwrap();
                for field in fields {
                    let val = self.get_struct_field(struct_id, val, field.name);
                    match field.pat {
                        None => self.codegen_local_var(vars, field.name, val),
                        Some(pat) => self.codegen_local_pat(vars, pat, val),
                    }
                }
            }
            hir::Pat::Lit(_) | hir::Pat::Enum { .. } => {
                unreachable!("Attempt to codegen falliable pattern: {:#?}", pat)
            }
        }
    }

    fn codegen_expr(&'ctx self, vars: &mut Vars<'ctx>, id: ExprId) -> Value<'ctx> {
        let expr = &self.hir[id];
        match expr {
            Expr::Lit(lit) => Some(self.codegen_lit(*lit)),
            Expr::Var(var) => Some(self.codegen_var(vars, *var)),
            Expr::Tuple(exprs) => self.codegen_tuple(vars, id, exprs),
            Expr::Struct { fields, .. } => self.codegen_struct(vars, id, fields),
            Expr::Enum {
                variant, fields, ..
            } => self.codegen_enum(vars, id, *variant, fields),
            Expr::Field { expr, field } => self.codegen_field(vars, *expr, *field),
            Expr::If {
                test,
                then_branch,
                else_branch,
            } => self.codegen_if(vars, *test, *then_branch, *else_branch),
            Expr::Match { test, cases } => self.codegen_match(vars, id, *test, cases),
            Expr::Loop(body) => self.codegen_loop(vars, id, *body),
            Expr::Break(expr) => self.codegen_break(vars, *expr),
            Expr::Continue => self.codegen_continue(vars),
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
                            self.codegen_local_pat(vars, *pat, val);
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

    fn codegen_lvalue(&self, vars: &mut Vars<'ctx>, id: ExprId) -> Option<PointerValue> {
        let expr = &self.hir[id];

        match expr {
            Expr::Var(var_id) => {
                let var = &self.hir[*var_id];
                let denotation = self.scopes.lookup_var(*var_id, var);
                match denotation {
                    Some(Denotation::Local(id)) => Some(vars[id]),
                    _ => unreachable!("Attempt to assign non-local variable: {:#?}", var),
                }
            }
            Expr::Field { expr, field } => {
                let base_value = self.codegen_lvalue(vars, *expr)?;

                let struct_id = self.types[*expr].as_struct().unwrap();
                let struct_def = &self.hir[struct_id];
                let struct_name = &self.hir[struct_def.name];

                let value = match field {
                    Field::Tuple(idx) => self
                        .builder
                        .build_struct_gep(base_value, *idx as u32, &format!("{struct_name}.{idx}"))
                        .unwrap(),
                    Field::Named(name) => {
                        let name = &self.hir[*name];
                        let idx = struct_def
                            .fields
                            .iter()
                            .position(|field| &self.hir[field.name] == name)
                            .unwrap();
                        self.builder
                            .build_struct_gep(
                                base_value,
                                idx as u32,
                                &format!("{struct_name}.{name}"),
                            )
                            .unwrap()
                    }
                };
                Some(value)
            }
            _ => unreachable!("Attempt to assign non-lvalue: {:#?}", expr),
        }
    }

    fn codegen_unit(&self) -> BasicValueEnum { self.llvm.const_struct(&[], false).into() }

    fn codegen_undef(&self) -> BasicValueEnum { self.llvm.i8_type().get_undef().into() }

    fn codegen_lit(&self, lit: Lit) -> BasicValueEnum<'ctx> {
        match lit {
            Lit::Bool(false) => self.llvm.bool_type().const_int(0, false).into(),
            Lit::Bool(true) => self.llvm.bool_type().const_int(1, false).into(),
            Lit::Int(val) => self.llvm.i32_type().const_int(val.into(), false).into(),
            Lit::Float(val) => self.llvm.f32_type().const_float(val.0.into()).into(),
            Lit::Char(val) => self.llvm.i32_type().const_int(val.into(), false).into(),
        }
    }

    fn codegen_var(&self, vars: &mut Vars<'ctx>, id: VarId) -> BasicValueEnum {
        let var = &self.hir[id];
        let denotation = self.scopes.lookup_var(id, var);
        match denotation {
            Some(Denotation::Local(id)) => self.builder.build_load(vars[id], var.as_str()),
            Some(Denotation::Fn(id)) => {
                let fn_def = &self.hir[id];
                let fn_name = &self.hir[fn_def.name].as_str();
                let fn_type = &self.types[id];
                let fn_value = vars[id];
                self.codegen_fn_value(vars, fn_name, fn_value, fn_type)
            }
            Some(Denotation::Builtin(b)) => self.codegen_builtin(vars, b),
            _ => unreachable!("Local variable not bound to a value: {:#?}", var),
        }
    }

    fn codegen_fn_value(
        &self,
        vars: &mut Vars<'ctx>,
        fn_name: &str,
        fn_value: FunctionValue,
        fn_type: &FnType,
    ) -> BasicValueEnum {
        let code_ptr = fn_value.as_global_value().as_pointer_value();
        let closure_alloca = self.builder.build_alloca(
            self.closure_type(vars, fn_type),
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

    fn codegen_builtin(&self, vars: &mut Vars<'ctx>, builtin: Builtin) -> BasicValueEnum {
        match builtin {
            Builtin::Exit => {
                let exit_wrapper_fn = self.module.get_function("builtins.exit.wrapper").unwrap();
                self.codegen_fn_value(vars, "exit", exit_wrapper_fn, builtin.ty().as_fn().unwrap())
            }
            Builtin::PutChar => {
                let wrapper_fn = self
                    .module
                    .get_function("builtins.putchar.wrapper")
                    .unwrap();
                self.codegen_fn_value(vars, "putchar", wrapper_fn, builtin.ty().as_fn().unwrap())
            }
            Builtin::Bool | Builtin::Int | Builtin::Float | Builtin::Char | Builtin::Never => {
                unreachable!("Attempt to codegen non-value builtin: {:#?}", builtin)
            }
        }
    }

    fn codegen_tuple(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        exprs: &[ExprId],
    ) -> Value<'ctx> {
        let ty = &self.types[expr];
        let value_type = self.value_type(vars, ty);
        let tuple_alloca = self.builder.build_alloca(value_type, "tuple.alloca");
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
        &'ctx self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        inits: &[FieldInit],
    ) -> Value<'ctx> {
        let struct_id = self.types[expr].as_struct().unwrap();
        let struct_name = self.struct_name(struct_id);
        let struct_type = self.struct_type(vars, struct_id);
        let struct_alloca = self
            .builder
            .build_alloca(struct_type, &format!("{struct_name}.alloca"));

        for init in inits {
            let value = self.codegen_expr(vars, init.val)?;
            self.set_struct_field(vars, struct_id, struct_alloca, init.name, value);
        }

        Some(self.builder.build_load(struct_alloca, struct_name.as_str()))
    }

    fn codegen_enum(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        variant: VarId,
        inits: &[FieldInit],
    ) -> Value<'ctx> {
        let enum_id = self.types[expr].as_enum().unwrap();
        let enum_def = &self.hir[enum_id];
        let enum_name = &self.hir[enum_def.name];

        let enum_ty = self.enum_type(vars, enum_id);

        let variant_name = self.hir[variant].as_str();
        let (variant_idx, variant) = enum_def.find_variant(&self.hir, variant).unwrap();
        let discriminant_value = match self.discriminant_type(enum_def.variants.len()) {
            None => self.codegen_unit(),
            Some(int_type) => int_type.const_int(variant_idx as u64, false).into(),
        };

        let alloca = self
            .builder
            .build_alloca(enum_ty, &format!("{enum_name}::{variant_name}.alloca"));

        let discriminant_gep = self
            .builder
            .build_struct_gep(alloca, 0, &format!("{enum_name}.discriminant.gep"))
            .unwrap();
        self.builder
            .build_store(discriminant_gep, discriminant_value);

        for init in inits.iter() {
            let value = self.codegen_expr(vars, init.val)?;
            let (idx, field) = variant.lookup_field(&self.hir, init.name).unwrap();
            let field_name = self.hir[field.name].as_str();
            let payload_gep = self
                .builder
                .build_struct_gep(alloca, 1, "payload.gep")
                .unwrap();
            let field_gep = self
                .builder
                .build_struct_gep(
                    payload_gep,
                    idx as u32,
                    &format!("{enum_name}::{variant_name}.{field_name}.gep"),
                )
                .unwrap();
            self.builder.build_store(field_gep, value);
        }

        let casted = self
            .builder
            .build_bitcast(alloca, enum_ty.ptr_type(AddressSpace::Generic), "")
            .into_pointer_value();
        let load = self
            .builder
            .build_load(casted, &format!("{enum_name}::{variant_name}"));
        Some(load)
    }

    fn codegen_field(&'ctx self, vars: &mut Vars<'ctx>, expr: ExprId, field: Field) -> Value<'ctx> {
        let struct_value = self.codegen_expr(vars, expr)?;
        let value = match field {
            Field::Tuple(idx) => self.get_tuple_field(struct_value, idx as usize),
            Field::Named(name) => {
                let struct_id = self.types[expr].as_struct().unwrap();
                self.get_struct_field(struct_id, struct_value, name)
            }
        };
        Some(value)
    }

    fn codegen_if(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        test: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    ) -> Value<'ctx> {
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
            Some(_) => self.value_type(vars, &self.types[then_branch]),
            None => self.unit_type().into(),
        };
        let phi = self.builder.build_phi(ty, "if.merge");
        phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);
        Some(phi.as_basic_value())
    }

    fn codegen_loop(&'ctx self, vars: &mut Vars<'ctx>, expr: ExprId, body: ExprId) -> Value<'ctx> {
        let old_bb = self.builder.get_insert_block().unwrap();
        let exit_bb = self.llvm.insert_basic_block_after(old_bb, "loop.exit");
        let body_bb = self.llvm.insert_basic_block_after(old_bb, "loop.body");

        let result_type = &self.types[expr];
        let terminates = result_type != &Type::NEVER;
        let result_type = if terminates {
            self.value_type(vars, result_type)
        } else {
            self.unit_type().into()
        };
        let result_alloca = self.builder.build_alloca(result_type, "loop.result.alloca");
        self.builder.build_unconditional_branch(body_bb);

        self.builder.position_at_end(body_bb);
        let old_loop = vars.current_loop.clone();
        let new_loop = Loop {
            result_alloca,
            body_bb,
            exit_bb,
            does_continue: false,
            does_break: false,
        };
        vars.current_loop = Some(new_loop);
        let _body = self.codegen_expr(vars, body);
        let Loop {
            does_continue,
            does_break,
            ..
        } = vars.current_loop.as_ref().unwrap();

        let ret = match dbg!((does_continue, does_break)) {
            (false, false) => {
                self.builder.build_unconditional_branch(body_bb);
                self.builder.position_at_end(exit_bb);
                self.builder.build_unreachable();
                None
            }
            (true, false) => {
                self.builder.position_at_end(exit_bb);
                self.builder.build_unreachable();
                None
            }
            (_, true) => {
                self.builder.position_at_end(exit_bb);
                let result = self.builder.build_load(result_alloca, "loop.result");
                Some(result)
            }
        };

        vars.current_loop = old_loop;

        ret
    }

    fn codegen_break(&'ctx self, vars: &mut Vars<'ctx>, expr: Option<ExprId>) -> Value<'ctx> {
        let value = match expr {
            None => self.codegen_unit(),
            Some(expr) => self.codegen_expr(vars, expr)?,
        };

        let Loop {
            result_alloca,
            exit_bb,
            ref mut does_break,
            ..
        } = vars.current_loop.as_mut().unwrap();
        *does_break = true;
        self.builder.build_store(*result_alloca, value);
        self.builder.build_unconditional_branch(*exit_bb);
        None
    }

    fn codegen_continue(&self, vars: &mut Vars<'ctx>) -> Value {
        let Loop {
            body_bb,
            ref mut does_continue,
            ..
        } = vars.current_loop.as_mut().unwrap();
        *does_continue = true;
        self.builder.build_unconditional_branch(*body_bb);
        None
    }

    fn codegen_call(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        func: ExprId,
        args: &[ExprId],
    ) -> Value<'ctx> {
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
            Left(value) => Some(value),
            Right(_) => {
                self.builder.build_unreachable();
                None
            }
        }
    }

    fn codegen_lambda(
        &'ctx self,
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
            .build_alloca(self.closure_type(vars, closure_type), "closure.alloca");
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
            vars,
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
        &'ctx self,
        vars: &mut Vars<'ctx>,
        free_vars: &FreeVars,
        expr: ExprId,
        params: &[Param],
        body: ExprId,
    ) -> FunctionValue<'ctx> {
        let fn_type = self.fn_type(vars, self.types[expr].as_fn().unwrap());
        let llvm_fn = self.module.add_function("lambda", fn_type, None);
        let old_bb = self.builder.get_insert_block().unwrap();

        let bb = self.llvm.append_basic_block(llvm_fn, "lambda.entry");
        self.builder.position_at_end(bb);

        // load free vars
        let free_vars_type = self.tuple_type(
            vars,
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
                self.codegen_local_pat(vars, hir_param.pat, llvm_param)
            });

        if let Some(value) = self.codegen_expr(vars, body) {
            self.builder.build_return(Some(&value));
        }

        self.builder.position_at_end(old_bb);

        llvm_fn
    }

    fn codegen_return(&'ctx self, vars: &mut Vars<'ctx>, expr: Option<ExprId>) -> Value<'ctx> {
        let value = match expr {
            Some(expr) => self.codegen_expr(vars, expr)?,
            None => self.codegen_unit(),
        };
        self.builder.build_return(Some(&value));
        None
    }

    fn codegen_unop(&'ctx self, vars: &mut Vars<'ctx>, op: Unop, expr: ExprId) -> Value<'ctx> {
        let ty = &self.types[expr];
        let value = self.codegen_expr(vars, expr)?;
        let value = match op {
            Unop::Not if ty == &Type::BOOL => self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    value.into_int_value(),
                    self.llvm.bool_type().const_zero(),
                    "",
                )
                .into(),
            Unop::Sub if ty == &Type::INT => self
                .builder
                .build_int_neg(value.into_int_value(), "")
                .into(),

            Unop::Sub if ty == &Type::FLOAT => self
                .builder
                .build_float_neg(value.into_float_value(), "")
                .into(),

            Unop::Add if ty == &Type::INT || ty == &Type::FLOAT => value,
            _ => unreachable!("Attempt to codegen unop {} {:?}", op, ty),
        };
        Some(value)
    }

    fn codegen_binop(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        op: Binop,
        rhs: ExprId,
    ) -> Value<'ctx> {
        match op {
            Binop::Lazy(op) => self.codegen_lazy_binop(vars, lhs, op, rhs),
            Binop::Arithmetic(op) => self.codegen_arithmetic_binop(vars, lhs, op, rhs),
            Binop::Cmp(op) => self.codegen_cmp_binop(vars, lhs, op, rhs),
            Binop::Assign => self.codegen_assign(vars, lhs, rhs),
        }
    }

    fn codegen_lazy_binop(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        op: LazyBinop,
        rhs: ExprId,
    ) -> Value<'ctx> {
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
        &'ctx self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        op: ArithmeticBinop,
        rhs: ExprId,
    ) -> Value<'ctx> {
        let lhs_value = self.codegen_expr(vars, lhs)?;
        let rhs_value = self.codegen_expr(vars, rhs)?;

        let lhs_ty = &self.types[lhs];
        let rhs_ty = &self.types[rhs];

        let ty = &self.types[lhs];

        #[rustfmt::skip]
        macro_rules! int_op {
            ($op:ident) => {self.builder .$op(lhs_value.into_int_value(), rhs_value.into_int_value(), "") .into()};
        }

        #[rustfmt::skip]
        macro_rules! float_op {
            ($op:ident) => {self.builder.$op(lhs_value.into_float_value(), rhs_value.into_float_value(), "") .into()};
        }

        let value = match op {
            ArithmeticBinop::Add if ty.is_int() => int_op!(build_int_add),
            ArithmeticBinop::Sub if ty.is_int() => int_op!(build_int_sub),
            ArithmeticBinop::Mul if ty.is_int() => int_op!(build_int_mul),
            ArithmeticBinop::Div if ty.is_int() => int_op!(build_int_signed_div),

            ArithmeticBinop::Add if ty.is_float() => float_op!(build_float_add),
            ArithmeticBinop::Sub if ty.is_float() => float_op!(build_float_sub),
            ArithmeticBinop::Mul if ty.is_float() => float_op!(build_float_mul),
            ArithmeticBinop::Div if ty.is_float() => float_op!(build_float_div),

            _ => unreachable!(format!("cannot perform binop {lhs_ty:?} {op} {rhs_ty:?}")),
        };
        Some(value)
    }

    fn codegen_cmp_binop(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        op: CmpBinop,
        rhs: ExprId,
    ) -> Value<'ctx> {
        let lhs_value = self.codegen_expr(vars, lhs)?;
        let rhs_value = self.codegen_expr(vars, rhs)?;

        let lhs_ty = &self.types[lhs];
        let rhs_ty = &self.types[rhs];

        let ty = &self.types[lhs];

        #[rustfmt::skip]
        macro_rules! int_cmp {
            ($op:expr) => {self.builder.build_int_compare($op,lhs_value.into_int_value(), rhs_value.into_int_value(), "") .into()};
        }

        #[rustfmt::skip]
        macro_rules! float_cmp {
            ($op:expr) => {self.builder.build_float_compare($op,lhs_value.into_float_value(), rhs_value.into_float_value(), "") .into()};
        }

        let value = match op {
            CmpBinop::Eq if ty.is_integral() => int_cmp!(IntPredicate::EQ),
            CmpBinop::NotEq if ty.is_integral() => int_cmp!(IntPredicate::NE),
            CmpBinop::Less if ty.is_integral() => int_cmp!(IntPredicate::SLT),
            CmpBinop::LessEq if ty.is_integral() => int_cmp!(IntPredicate::SLE),
            CmpBinop::Greater if ty.is_integral() => int_cmp!(IntPredicate::SGT),
            CmpBinop::GreaterEq if ty.is_integral() => int_cmp!(IntPredicate::SGE),

            CmpBinop::Eq if ty.is_floating() => float_cmp!(FloatPredicate::OEQ),
            CmpBinop::NotEq if ty.is_floating() => float_cmp!(FloatPredicate::ONE),
            CmpBinop::Less if ty.is_floating() => float_cmp!(FloatPredicate::OLT),
            CmpBinop::LessEq if ty.is_floating() => float_cmp!(FloatPredicate::OLE),
            CmpBinop::Greater if ty.is_floating() => float_cmp!(FloatPredicate::OGT),
            CmpBinop::GreaterEq if ty.is_floating() => float_cmp!(FloatPredicate::OGE),

            _ => unreachable!(format!("cannot perform binop {lhs_ty:?} {op} {rhs_ty:?}")),
        };
        Some(value)
    }

    fn codegen_assign(&'ctx self, vars: &mut Vars<'ctx>, lhs: ExprId, rhs: ExprId) -> Value<'ctx> {
        let lhs = self.codegen_lvalue(vars, lhs)?;
        let rhs = self.codegen_expr(vars, rhs)?;
        self.builder.build_store(lhs, rhs);
        Some(self.codegen_unit())
    }
}

impl<'ctx> Compiler<'ctx> {
    fn codegen_match(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        parent: ExprId,
        test: ExprId,
        cases: &[MatchCase],
    ) -> Value<'ctx> {
        let test_value = self.codegen_expr(vars, test)?;

        let bb = self.builder.get_insert_block().unwrap();
        let end_bb = self.llvm.insert_basic_block_after(bb, "match.end");
        let mut next_bb = self.llvm.insert_basic_block_after(bb, "match.fail");

        // failure to match is UB
        self.builder.position_at_end(next_bb);
        self.builder.build_unreachable();

        let mut incomings = Vec::new();
        for (idx, case) in cases.iter().enumerate().rev() {
            let then_bb = self
                .llvm
                .insert_basic_block_after(bb, &format!("match.case{idx}.then"));
            let test_bb = self
                .llvm
                .insert_basic_block_after(bb, &format!("match.case{idx}.test"));

            // test agaisnt the pattern
            self.builder.position_at_end(test_bb);
            let matched = self.codegen_match_attempt(idx, test_value, case.pat);
            self.builder
                .build_conditional_branch(matched, then_bb, next_bb);

            // the then block
            self.builder.position_at_end(then_bb);
            self.capture_variables_from_pattern(vars, test_value, case.pat);
            let val = self
                .codegen_expr(vars, case.expr)
                .unwrap_or_else(|| self.codegen_undef());
            let val: Box<dyn BasicValue> = Box::new(val);
            self.builder.build_unconditional_branch(end_bb);

            incomings.push((val, then_bb));

            next_bb = test_bb;
        }
        self.builder.position_at_end(bb);
        self.builder.build_unconditional_branch(next_bb);

        // merge the branches
        self.builder.position_at_end(end_bb);
        let expr_type = self.value_type(vars, &self.types[parent]);
        let phi = self.builder.build_phi(expr_type, "match.phi");
        phi.add_incoming(
            incomings
                .iter()
                .map(|(val, bb)| (val.as_ref(), *bb))
                .collect::<Vec<_>>()
                .as_slice(),
        );
        Some(phi.as_basic_value())
    }

    fn codegen_match_attempt(
        &self,
        case_idx: usize,
        test: BasicValueEnum<'ctx>,
        pat_id: PatId,
    ) -> IntValue<'ctx> {
        let pat = &self.hir[pat_id];

        match pat {
            Pat::Lit(_) => todo!(),
            Pat::Var(_) | Pat::Ignore => self.codegen_true(),
            Pat::Tuple(pats) => self.codegen_all(pats.iter().enumerate().map(|(idx, pat)| {
                let val = self.get_tuple_field(test, idx);
                self.codegen_match_attempt(idx, val, *pat)
            })),
            Pat::Struct { fields, .. } => {
                let struct_id = self.types[pat_id].as_struct().unwrap();
                self.codegen_all(fields.iter().map(|field| {
                    let val = self.get_struct_field(struct_id, test, field.name);
                    match field.pat {
                        None => self.codegen_true(),
                        Some(pat) => self.codegen_match_attempt(case_idx, val, pat),
                    }
                }))
            }
            Pat::Enum {
                variant, fields, ..
            } => {
                let enum_id = self.types[pat_id].as_enum().unwrap();
                let enum_def = &self.hir[enum_id];
                let enum_name = &self.hir[enum_def.name];

                let (variant_idx, enum_variant) =
                    enum_def.find_variant(&self.hir, *variant).unwrap();
                let variant_name = &self.hir[enum_variant.name];

                let tag_matched = match self.discriminant_type(enum_def.variants.len()) {
                    None => self.codegen_true(),
                    Some(int_type) => {
                        let tag = self.get_enum_discriminant(test);
                        self.builder.build_int_compare(
                            IntPredicate::EQ,
                            tag,
                            int_type.const_int(variant_idx as u64, false),
                            "",
                        )
                    }
                };
                let bb = self.builder.get_insert_block().unwrap();
                let end_bb = self.llvm.insert_basic_block_after(
                    bb,
                    &format!("match.case{case_idx}.{enum_name}::{variant_name}.end"),
                );
                let else_bb = self.llvm.insert_basic_block_after(
                    bb,
                    &format!("match.case{case_idx}.{enum_name}::{variant_name}.else"),
                );
                let then_bb = self.llvm.insert_basic_block_after(
                    bb,
                    &format!("match.case{case_idx}.{enum_name}::{variant_name}.then"),
                );

                self.builder
                    .build_conditional_branch(tag_matched, then_bb, else_bb);

                // else branch
                self.builder.position_at_end(else_bb);
                let else_value = self.codegen_false();
                self.builder.build_unconditional_branch(end_bb);

                // then branch
                self.builder.position_at_end(then_bb);
                let (payload, variant) = self.get_enum_payload(enum_id, *variant, test);
                let then_value = self.codegen_all(fields.iter().map(|field| {
                    let val = self.get_variant_field(&variant, payload, field.name);
                    match field.pat {
                        None => self.codegen_true(),
                        Some(pat) => self.codegen_match_attempt(case_idx, val, pat),
                    }
                }));
                self.builder.build_unconditional_branch(end_bb);

                // merge the 2 branches
                self.builder.position_at_end(end_bb);
                let phi = self.builder.build_phi(
                    self.llvm.bool_type(),
                    &format!("match.case{case_idx}.{enum_name}::{variant_name}.phi"),
                );
                phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);
                phi.as_basic_value().into_int_value()
            }
        }
    }

    fn capture_variables_from_pattern(
        &self,
        vars: &mut Vars<'ctx>,
        test: BasicValueEnum<'ctx>,
        pat_id: PatId,
    ) {
        let pat = &self.hir[pat_id];

        match pat {
            Pat::Lit(_) | Pat::Ignore => {}
            Pat::Var(var_id) => self.codegen_local_var(vars, *var_id, test),
            Pat::Tuple(pats) => pats.iter().enumerate().for_each(|(idx, pat)| {
                let val = self.get_tuple_field(test, idx);
                self.capture_variables_from_pattern(vars, val, *pat)
            }),
            Pat::Struct { fields, .. } => {
                let struct_id = self.types[pat_id].as_struct().unwrap();
                fields.iter().for_each(|field| {
                    let val = self.get_struct_field(struct_id, test, field.name);
                    match field.pat {
                        None => self.codegen_local_var(vars, field.name, val),
                        Some(pat) => self.capture_variables_from_pattern(vars, val, pat),
                    }
                })
            }
            Pat::Enum {
                variant, fields, ..
            } => {
                let enum_id = self.types[pat_id].as_enum().unwrap();

                let (payload, variant) = self.get_enum_payload(enum_id, *variant, test);
                fields.iter().for_each(|field| {
                    let val = self.get_variant_field(&variant, payload, field.name);
                    match field.pat {
                        None => self.codegen_local_var(vars, field.name, val),
                        Some(pat) => self.capture_variables_from_pattern(vars, val, pat),
                    }
                })
            }
        }
    }

    fn codegen_true(&self) -> IntValue<'ctx> { self.llvm.bool_type().const_int(true as _, false) }
    fn codegen_false(&self) -> IntValue<'ctx> { self.llvm.bool_type().const_int(false as _, false) }

    fn codegen_all(&self, bools: impl Iterator<Item = IntValue<'ctx>>) -> IntValue<'ctx> {
        bools.fold(self.codegen_true(), |acc, e| {
            self.builder.build_and(acc, e, "")
        })
    }
}
