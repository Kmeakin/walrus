#![allow(clippy::cast_possible_truncation)]

use arena::ArenaMap;
use either::Either;
pub use inkwell::context::Context;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    memory_buffer::MemoryBuffer,
    module::Module,
    targets::TargetData,
    types::{AnyType, BasicType, BasicTypeEnum, FunctionType, IntType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};
use std::ops::Index;
use walrus_semantics::{
    builtins::Builtin,
    hir::{
        self, ArithmeticBinop, Binop, CmpBinop, EnumDefId, Expr, ExprId, Field, FieldInit, FnDefId,
        LazyBinop, Lit, Param, PatId, StructDefId, StructField, Unop, VarId,
    },
    scopes::{self, Denotation},
    ty,
    ty::{FnType, PrimitiveType, Type},
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
    locals: ArenaMap<PatId, PointerValue<'a>>,
    fns: ArenaMap<FnDefId, FunctionValue<'a>>,
    current_loop: Option<Loop<'a>>,
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

    fn discriminant_type(&self, n: usize) -> Option<IntType> {
        const I0: usize = 1;
        const I8: usize = 2_usize.pow(8);
        const I16: usize = 2_usize.pow(16);
        const I32: usize = 2_usize.pow(32);

        let ty = match n {
            0..=I0 => return None,
            0..=I8 => self.llvm.i8_type(),
            0..=I16 => self.llvm.i16_type(),
            0..=I32 => self.llvm.i32_type(),
            _ => self.llvm.i64_type(),
        };
        Some(ty)
    }

    fn value_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Fn(func) => self.closure_type(func),
            Type::Struct(id) => self.struct_type(*id).into(),
            Type::Enum(id) => self.enum_type(*id).into(),
            Type::Tuple(tys) => self.tuple_type(tys).into(),
            Type::Primitive(PrimitiveType::Bool) => self.llvm.bool_type().into(),
            Type::Primitive(PrimitiveType::Int | PrimitiveType::Char) => {
                self.llvm.i32_type().into()
            }
            Type::Primitive(PrimitiveType::Float) => self.llvm.f32_type().into(),
            Type::Primitive(PrimitiveType::Never) | Type::Infer(_) | Type::Unknown => {
                unreachable!()
            }
        }
    }

    fn fn_type(&self, ty: &FnType) -> FunctionType<'ctx> {
        let FnType { params, ret } = ty;
        if ret.as_ref() == &Type::NEVER {
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

    fn unit_type(&self) -> StructType<'ctx> { self.tuple_type(&[]) }

    fn aggregate_type(&self, types: &[StructField]) -> StructType<'ctx> {
        self.llvm.struct_type(
            &types
                .iter()
                .map(|field| self.value_type(&self.types[field.ty]))
                .collect::<Vec<_>>(),
            false,
        )
    }

    fn struct_type(&self, id: StructDefId) -> StructType<'ctx> {
        let struct_def = &self.hir[id];
        self.aggregate_type(&struct_def.fields)
    }

    fn enum_type(&self, id: EnumDefId) -> StructType<'ctx> {
        let enum_def = &self.hir[id];

        let discriminant = self
            .discriminant_type(enum_def.variants.len())
            .map_or_else(|| self.unit_type().into(), Into::into);

        let target_triple = self.module.get_triple();
        let target_str = target_triple.as_str().to_str().unwrap();
        let layout = TargetData::create(target_str);
        let largest_payload = enum_def
            .variants
            .iter()
            .map(|variant| self.aggregate_type(&variant.fields))
            .max_by_key(|ty| layout.get_bit_size(&ty.as_any_type_enum()))
            .unwrap_or_else(|| self.unit_type());

        self.llvm
            .struct_type(&[discriminant, largest_payload.into()], false)
    }

    pub fn codegen_module(self) -> Module<'ctx> {
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

        if let Err(e) = self.module.verify() {
            eprintln!("{}", self.module.print_to_string().to_string());
            eprintln!("{}", e.to_string());
            panic!()
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
            Expr::Enum {
                variant, fields, ..
            } => self.codegen_enum(vars, id, *variant, fields),
            Expr::Field { expr, field } => self.codegen_field(vars, *expr, *field),
            Expr::If {
                test,
                then_branch,
                else_branch,
            } => self.codegen_if(vars, *test, *then_branch, *else_branch),
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

    fn codegen_lvalue(&self, vars: &mut Vars<'ctx>, id: ExprId) -> Option<PointerValue> {
        let expr = &self.hir[id];
        match expr {
            Expr::Var(var) => {
                let var = &self.hir[*var];
                let denotation = self.scopes.lookup_expr(id, var);
                match denotation {
                    Some(Denotation::Local(id)) => Some(vars[id]),
                    _ => unreachable!(),
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
            _ => unreachable!(),
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
        fn_type: &FnType,
    ) -> BasicValueEnum {
        let code_ptr = fn_value.as_global_value().as_pointer_value();
        let closure_alloca = self.builder.build_alloca(
            self.closure_type(fn_type),
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
        let ty = &self.types[expr];
        let value_type = self.value_type(ty);
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

    fn codegen_struct(&self, vars: &mut Vars<'ctx>, expr: ExprId, fields: &[FieldInit]) -> Value {
        let struct_id = self.types[expr].as_struct().unwrap();
        let struct_def = &self.hir[struct_id];
        let struct_name = &self.hir[struct_def.name];

        let ty = &self.types[expr];
        let value_type = self.value_type(ty);
        let init_exprs = fields
            .iter()
            .map(|field| (&self.hir[field.name], self.codegen_expr(vars, field.val)))
            .collect::<Vec<_>>();
        let struct_alloca = self
            .builder
            .build_alloca(value_type, &format!("{struct_name}.alloca"));
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

    fn codegen_enum(
        &self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        variant: VarId,
        inits: &[FieldInit],
    ) -> Value {
        let enum_id = self.types[expr].as_enum().unwrap();
        let enum_def = &self.hir[enum_id];
        let enum_name = &self.hir[enum_def.name];

        let variant_name = self.hir[variant].as_str();
        let (variant_idx, variant) = enum_def
            .variants
            .iter()
            .enumerate()
            .find(|(_, var)| self.hir[var.name] == self.hir[variant])
            .unwrap();

        let (discriminant_type, discriminant_value) =
            match self.discriminant_type(enum_def.variants.len()) {
                None => (self.unit_type().into(), self.codegen_unit()),
                Some(int_type) => (
                    int_type.into(),
                    int_type.const_int(variant_idx as u64, false).into(),
                ),
            };

        let ty = self.llvm.struct_type(
            &[
                discriminant_type,
                self.aggregate_type(&variant.fields).into(),
            ],
            false,
        );

        let alloca = self
            .builder
            .build_alloca(ty, &format!("{enum_name}::{variant_name}.alloca"));

        dbg!(alloca);
        let discriminant_gep = self
            .builder
            .build_struct_gep(alloca, 0, &format!("{enum_name}.discriminant.gep"))
            .unwrap();
        self.builder
            .build_store(discriminant_gep, discriminant_value);

        for init in inits.iter() {
            let value = self.codegen_expr(vars, init.val)?;
            let (idx, field) = variant
                .fields
                .iter()
                .enumerate()
                .find(|(_, field)| self.hir[field.name] == self.hir[init.name])
                .unwrap();
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

        let load = self
            .builder
            .build_load(alloca, &format!("{enum_name}::{variant_name}"));
        Some(load)
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
            None => self.unit_type().into(),
        };
        let phi = self.builder.build_phi(ty, "if.merge");
        phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);
        Some(phi.as_basic_value())
    }

    fn codegen_loop(&self, vars: &mut Vars<'ctx>, expr: ExprId, body: ExprId) -> Value {
        let old_bb = self.builder.get_insert_block().unwrap();
        let exit_bb = self.llvm.insert_basic_block_after(old_bb, "loop.exit");
        let body_bb = self.llvm.insert_basic_block_after(old_bb, "loop.body");

        let result_type = &self.types[expr];
        let terminates = result_type != &Type::NEVER;
        let result_type = if terminates {
            self.value_type(result_type)
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

    fn codegen_break(&self, vars: &mut Vars<'ctx>, expr: Option<ExprId>) -> Value {
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
            .build_alloca(self.closure_type(closure_type), "closure.alloca");
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
        let fn_type = self.fn_type(self.types[expr].as_fn().unwrap());
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

            Unop::Sub | Unop::Not => unreachable!(),
            Unop::Add => value,
        };
        Some(value)
    }

    fn codegen_binop(&self, vars: &mut Vars<'ctx>, lhs: ExprId, op: Binop, rhs: ExprId) -> Value {
        match op {
            Binop::Lazy(op) => self.codegen_lazy_binop(vars, lhs, op, rhs),
            Binop::Arithmetic(op) => self.codegen_arithmetic_binop(vars, lhs, op, rhs),
            Binop::Cmp(op) => self.codegen_cmp_binop(vars, lhs, op, rhs),
            Binop::Assign => self.codegen_assign(vars, lhs, rhs),
        }
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

    fn codegen_assign(&self, vars: &mut Vars<'ctx>, lhs: ExprId, rhs: ExprId) -> Value {
        let lhs = self.codegen_lvalue(vars, lhs)?;
        let rhs = self.codegen_expr(vars, rhs)?;
        self.builder.build_store(lhs, rhs);
        Some(self.codegen_unit())
    }
}
