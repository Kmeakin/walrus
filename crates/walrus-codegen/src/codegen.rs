use arena::ArenaMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{
        BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntMathValue, IntValue, PointerValue,
    },
    AddressSpace, FloatPredicate, IntPredicate,
};
use std::{cell::RefCell, collections::HashMap, ops::Index};
use walrus_semantics::{
    hir::{self, Binop, Expr, ExprId, Field, FnDefId, Lit, PatId, StructExprField, Unop, VarId},
    scopes::{self, Denotation},
    ty,
    ty::{Ctor, FnType, Type},
};

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
    pub types: ty::InferenceResult,
    pub scopes: scopes::Scopes,
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

impl<'ctx> Compiler<'ctx> {
    fn void_ptr_type(&self) -> BasicTypeEnum {
        self.llvm.i8_type().ptr_type(AddressSpace::Generic).into()
    }

    fn value_type(&self, ty: &Type) -> BasicTypeEnum {
        let (ctor, params) = match ty {
            Type::App { ctor, params } => (ctor, params),
            _ => unreachable!(),
        };
        match ctor {
            ty::Ctor::Bool => self.llvm.bool_type().into(),
            ty::Ctor::Int => self.llvm.i32_type().into(),
            ty::Ctor::Float => self.llvm.f32_type().into(),
            ty::Ctor::Char => self.llvm.i32_type().into(),
            ty::Ctor::Tuple => {
                let field_types = params
                    .iter()
                    .map(|ty| self.value_type(&ty))
                    .collect::<Vec<_>>();
                self.llvm.struct_type(&field_types, false).into()
            }
            ty::Ctor::Fn => self.closure_type(ty.as_fn().unwrap()),
            ty::Ctor::Never => todo!(),
            ty::Ctor::Struct(id) => {
                let struct_def = &self.hir[*id];
                let field_types = struct_def
                    .fields
                    .iter()
                    .map(|field| {
                        let field_type = &self.types[field.ty];
                        self.value_type(&field_type)
                    })
                    .collect::<Vec<_>>();
                self.llvm.struct_type(&field_types, false).into()
            }
        }
    }

    fn fn_type(&self, ty: &FnType) -> FunctionType {
        let FnType { params, ret } = ty;
        self.value_type(ret)
            .fn_type(
                &std::iter::once(self.void_ptr_type())
                    .chain(params.iter().map(|ty| self.value_type(ty)))
                    .collect::<Vec<_>>(),
                false,
            )
            .clone()
    }

    fn closure_type(&self, ty: FnType) -> BasicTypeEnum {
        let struct_type = self.llvm.struct_type(
            &[
                self.fn_type(&ty).ptr_type(AddressSpace::Generic).into(),
                self.void_ptr_type(),
            ],
            false,
        );
        struct_type.into()
    }

    fn tuple_type(&self, tys: &[Type]) -> StructType {
        self.llvm.struct_type(
            &tys.iter().map(|ty| self.value_type(ty)).collect::<Vec<_>>(),
            true,
        )
    }

    fn codegen_module(&'ctx self, module: &HirModule) -> &Module<'ctx> {
        let mut vars = Vars::default();

        for (id, func) in module.hir.data.fn_defs.iter() {
            let fn_type = self.fn_type(&module.types[id]);
            let name = module.hir.data[func.name].as_str();
            let llvm_fn = self.module.add_function(name, fn_type, None);
        }

        for (id, func) in module.hir.data.fn_defs.iter() {
            self.codegen_fn(module, &mut vars, id)
        }

        &self.module
    }

    fn codegen_fn(&'ctx self, module: &HirModule, vars: &mut Vars<'ctx>, id: FnDefId) {
        let llvm_fn = vars[id];
        let fn_def = &module.hir.data[id];
        let name = &module.hir.data[fn_def.name];
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
                self.codegen_local_var(module, vars, hir_param.pat, llvm_param)
            });

        let body = self.codegen_expr(module, vars, fn_def.expr);
        self.builder.build_return(Some(&body));
    }

    fn codegen_local_var(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars,
        id: PatId,
        val: BasicValueEnum,
    ) {
        let pat = &module.hir.data[id];
        match pat {
            hir::Pat::Ignore | hir::Pat::Var(_) => {
                // `Pat::Ignore` still evaluates its arguments, for side effects
                // eg `let _ = print(5);`
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
                self.codegen_local_var(module, vars, *id, val)
            }),
        }
    }

    fn codegen_expr(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        id: ExprId,
    ) -> BasicValueEnum {
        let expr = &module.hir.data[id];
        match expr {
            Expr::Lit(lit) => self.codegen_lit(*lit),
            Expr::Var(var) => self.codegen_var(module, vars, id, *var),
            Expr::Tuple(exprs) => self.codegen_tuple(module, vars, id, exprs),
            Expr::Struct { fields, .. } => self.codegen_struct(module, vars, id, fields),
            Expr::Field { expr, field } => self.codegen_field(module, vars, *expr, *field),
            Expr::If {
                test,
                then_branch,
                else_branch,
            } => self.codegen_if(module, vars, *test, *then_branch, *else_branch),
            Expr::Loop(_) => todo!(),
            Expr::Break(_) => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Continue => todo!(),
            Expr::Call { func, args } => self.codegen_call(module, vars, *func, args),
            Expr::Lambda { params, expr } => todo!(),
            Expr::Unop { op, expr } => self.codegen_unop(module, vars, *op, *expr),
            Expr::Binop { lhs, op, rhs } => self.codegen_binop(module, vars, *lhs, *rhs, *op),
            Expr::Block { stmts, expr } => {
                for stmt in stmts {
                    match stmt {
                        hir::Stmt::Expr(expr) => {
                            self.codegen_expr(module, vars, *expr);
                        }
                        hir::Stmt::Let { pat, expr, .. } => {
                            let val = self.codegen_expr(module, vars, *expr);
                            self.codegen_local_var(module, vars, *pat, val);
                        }
                    }
                }
                match expr {
                    Some(expr) => self.codegen_expr(module, vars, *expr),
                    None => self.codegen_unit(),
                }
            }
        }
    }

    fn codegen_unit(&'ctx self) -> BasicValueEnum { self.llvm.const_struct(&[], true).into() }

    fn codegen_lit(&'ctx self, lit: Lit) -> BasicValueEnum {
        match lit {
            Lit::Bool(false) => self.llvm.bool_type().const_int(0, false).into(),
            Lit::Bool(true) => self.llvm.bool_type().const_int(1, false).into(),
            Lit::Int(val) => self.llvm.i32_type().const_int(val as u64, false).into(),
            Lit::Float(val) => self.llvm.f32_type().const_float(val.0 as f64).into(),
            Lit::Char(val) => self.llvm.i32_type().const_int(val as u64, false).into(),
        }
    }

    fn codegen_var(
        &'ctx self,
        module: &HirModule,
        vars: &Vars<'ctx>,
        expr: ExprId,
        var: VarId,
    ) -> BasicValueEnum {
        let var = &module.hir.data[var];
        let scope = module.scopes.scope_of_expr(expr);
        let denotation = module.scopes.lookup_in_scope(scope, &var);
        match denotation {
            Some(Denotation::Local(id)) => {
                self.builder.build_load(vars[id], &format!("{var}.load"))
            }
            Some(Denotation::Fn(id)) => todo!(),
            Some(Denotation::Builtin(b)) => todo!(),
            _ => unreachable!(),
        }
    }

    fn codegen_tuple(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        exprs: &[ExprId],
    ) -> BasicValueEnum {
        let types = module.types[expr].as_tuple().unwrap();
        let tuple_type = self.tuple_type(types);
        let tuple_alloca = self.builder.build_alloca(tuple_type, "tuple.alloca");
        for (idx, expr) in exprs.iter().enumerate() {
            let value = self.codegen_expr(module, vars, *expr);
            let gep = self
                .builder
                .build_struct_gep(tuple_alloca, idx as u32, &format!("tuple.{idx}.gep"))
                .unwrap();
            self.builder.build_store(gep, value);
        }
        self.builder.build_load(tuple_alloca, "tuple.load")
    }

    fn codegen_struct(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        fields: &[StructExprField],
    ) -> BasicValueEnum {
        let struct_id = self.types[expr].as_struct().unwrap();
        let struct_def = &self.hir[struct_id];
        let struct_name = &self.hir[struct_def.name];

        let struct_type = &self.types[expr];
        let struct_type = self.value_type(&struct_type);
        let init_exprs = fields
            .iter()
            .map(|field| {
                (
                    &self.hir[field.name],
                    self.codegen_expr(module, vars, field.val),
                )
            })
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
                    &format!("{struct_name}.{field_name}.gep"),
                )
                .unwrap();
            let value = init_exprs
                .iter()
                .find(|(name, _)| name == &field_name)
                .map(|(_, value)| value)
                .unwrap();
            self.builder.build_store(gep, *value);
        }
        self.builder
            .build_load(struct_alloca, &format!("{struct_name}.load"))
    }

    fn codegen_field(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        field: Field,
    ) -> BasicValueEnum {
        let base_value = self.codegen_expr(module, vars, expr);
        match field {
            Field::Tuple(idx) => self
                .builder
                .build_extract_value(base_value.into_struct_value(), idx, &format!("tuple.{idx}"))
                .unwrap(),
            Field::Named(name) => {
                let name = &module.hir.data[name];
                let struct_id = module.types[expr].as_struct().unwrap();
                let struct_def = &module.hir.data[struct_id];
                let struct_name = &module.hir.data[struct_def.name];
                let idx = struct_def
                    .fields
                    .iter()
                    .position(|field| &module.hir.data[field.name] == name)
                    .unwrap();
                self.builder
                    .build_extract_value(
                        base_value.into_struct_value(),
                        idx as u32,
                        &format!("{struct_name}.{name}"),
                    )
                    .unwrap()
            }
        }
    }

    fn codegen_if(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        test: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    ) -> BasicValueEnum {
        match else_branch {
            Some(else_branch) => {
                let then_type = &module.types[then_branch];
                let else_type = &module.types[else_branch];
                assert_eq!(then_type, else_type);

                let bb = self.builder.get_insert_block().unwrap();
                let end_bb = self.llvm.insert_basic_block_after(bb, "if.end");
                let then_bb = self.llvm.insert_basic_block_after(bb, "if.then");
                let else_bb = self.llvm.insert_basic_block_after(bb, "if.else");
                let test_value = self.codegen_expr(module, vars, test);
                self.builder.build_conditional_branch(
                    test_value.into_int_value(),
                    then_bb,
                    else_bb,
                );

                // then branch
                self.builder.position_at_end(then_bb);
                let then_value = self.codegen_expr(module, vars, then_branch);
                self.builder.build_unconditional_branch(end_bb);

                // else branch
                self.builder.position_at_end(else_bb);
                let else_value = self.codegen_expr(module, vars, else_branch);
                self.builder.build_unconditional_branch(end_bb);

                // merge the 2 branches
                self.builder.position_at_end(end_bb);
                let phi = self
                    .builder
                    .build_phi(self.value_type(&then_type), "if.merge");
                phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);
                phi.as_basic_value()
            }
            None => {
                let bb = self.builder.get_insert_block().unwrap();
                let end_bb = self.llvm.insert_basic_block_after(bb, "if.end");
                let then_bb = self.llvm.insert_basic_block_after(bb, "if.then");
                let test_value = self.codegen_expr(module, vars, test);
                self.builder
                    .build_conditional_branch(test_value.into_int_value(), then_bb, end_bb);

                // then branch
                self.builder.position_at_end(then_bb);
                let then_value = self.codegen_expr(module, vars, then_branch);
                self.builder.build_unconditional_branch(end_bb);

                // epilogue
                self.builder.position_at_end(end_bb);
                self.codegen_unit()
            }
        }
    }

    fn codegen_call(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        func: ExprId,
        args: &[ExprId],
    ) -> BasicValueEnum {
        let closure_value = self.codegen_expr(module, vars, func).into_struct_value();
        let code_ptr = self
            .builder
            .build_extract_value(closure_value, 0, "closure.code")
            .unwrap()
            .into_pointer_value();
        let env_ptr = self
            .builder
            .build_extract_value(closure_value, 1, "closure.env")
            .unwrap();
        let args = std::iter::once(env_ptr)
            .chain(args.iter().map(|arg| self.codegen_expr(module, vars, *arg)))
            .collect::<Vec<_>>();
        self.builder
            .build_call(code_ptr, &args, "call")
            .try_as_basic_value()
            .unwrap_left()
    }

    fn codegen_unop(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        op: Unop,
        expr: ExprId,
    ) -> BasicValueEnum {
        let value = self.codegen_expr(module, vars, expr);
        match op {
            Unop::Not => self
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    value.into_int_value(),
                    self.llvm.bool_type().const_int(0, false),
                    "unary_not",
                )
                .into(),
            Unop::Add => value,
            Unop::Sub => self
                .builder
                .build_int_neg(value.into_int_value(), "unary_neg")
                .into(),
        }
    }

    fn codegen_binop(
        &'ctx self,
        m: &HirModule,
        vars: &mut Vars<'ctx>,
        l: ExprId,
        r: ExprId,
        op: Binop,
    ) -> BasicValueEnum {
        let (ctor, params) = match &m.types[l] {
            Type::App { ctor, params } => (ctor, params),
            Type::Unknown | Type::Infer(_) => unreachable!(),
        };

        match (op, ctor) {
            (Binop::Add, Ctor::Int) => self.int_op(m, vars, l, r, Builder::build_int_add),
            (Binop::Add, Ctor::Float) => self.float_op(m, vars, l, r, Builder::build_float_add),
            (Binop::Add, _) => unreachable!("`+` only allowed on Int or Float"),

            (Binop::Sub, Ctor::Int) => self.int_op(m, vars, l, r, Builder::build_int_sub),
            (Binop::Sub, Ctor::Float) => self.float_op(m, vars, l, r, Builder::build_float_sub),
            (Binop::Sub, _) => unreachable!("`-` only allowed on Int or Float"),

            (Binop::Mul, Ctor::Int) => self.int_op(m, vars, l, r, Builder::build_int_mul),
            (Binop::Mul, Ctor::Float) => self.float_op(m, vars, l, r, Builder::build_float_mul),
            (Binop::Mul, _) => unreachable!("`*` only allowed on Int or Float"),

            (Binop::Div, Ctor::Int) => self.int_op(m, vars, l, r, Builder::build_int_signed_div),
            (Binop::Div, Ctor::Float) => self.float_op(m, vars, l, r, Builder::build_float_div),
            (Binop::Div, _) => unreachable!("`/` only allowed on Int or Float"),

            (Binop::Less, Ctor::Int) => self.int_cmp(m, vars, l, r, IntPredicate::SLT),
            (Binop::Less, Ctor::Float) => self.float_cmp(m, vars, l, r, FloatPredicate::OLT),
            (Binop::Less, _) => unreachable!("`<` only allowed on Int or Float"),

            (Binop::LessEq, Ctor::Int) => self.int_cmp(m, vars, l, r, IntPredicate::SLE),
            (Binop::LessEq, Ctor::Float) => self.float_cmp(m, vars, l, r, FloatPredicate::OLE),
            (Binop::LessEq, _) => unreachable!("`<=` only allowed on Int or Float"),

            (Binop::Greater, Ctor::Int) => self.int_cmp(m, vars, l, r, IntPredicate::SGT),
            (Binop::Greater, Ctor::Float) => self.float_cmp(m, vars, l, r, FloatPredicate::OGT),
            (Binop::Greater, _) => unreachable!("`>` only allowed on Int or Float"),

            (Binop::GreaterEq, Ctor::Int) => self.int_cmp(m, vars, l, r, IntPredicate::SGE),
            (Binop::GreaterEq, Ctor::Float) => self.float_cmp(m, vars, l, r, FloatPredicate::OGE),
            (Binop::GreaterEq, _) => unreachable!("`>=` only allowed on Int or Float"),
            _ => todo!(),
        }
    }

    fn int_op(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        rhs: ExprId,
        op: fn(&Builder<'ctx>, IntValue<'ctx>, IntValue<'ctx>, &str) -> IntValue<'ctx>,
    ) -> BasicValueEnum {
        let lhs = self.codegen_expr(module, vars, lhs);
        let rhs = self.codegen_expr(module, vars, rhs);
        op(
            &self.builder,
            lhs.into_int_value(),
            rhs.into_int_value(),
            "",
        )
        .into()
    }

    fn float_op(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        rhs: ExprId,
        op: fn(&Builder<'ctx>, FloatValue<'ctx>, FloatValue<'ctx>, &str) -> FloatValue<'ctx>,
    ) -> BasicValueEnum {
        let lhs = self.codegen_expr(module, vars, lhs);
        let rhs = self.codegen_expr(module, vars, rhs);
        op(
            &self.builder,
            lhs.into_float_value(),
            rhs.into_float_value(),
            "",
        )
        .into()
    }

    fn int_cmp(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        rhs: ExprId,
        cmp: IntPredicate,
    ) -> BasicValueEnum {
        let lhs = self.codegen_expr(module, vars, lhs);
        let rhs = self.codegen_expr(module, vars, rhs);
        self.builder
            .build_int_compare(cmp, lhs.into_int_value(), rhs.into_int_value(), "")
            .into()
    }

    fn float_cmp(
        &'ctx self,
        module: &HirModule,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        rhs: ExprId,
        cmp: FloatPredicate,
    ) -> BasicValueEnum {
        let lhs = self.codegen_expr(module, vars, lhs);
        let rhs = self.codegen_expr(module, vars, rhs);
        self.builder
            .build_float_compare(cmp, lhs.into_float_value(), rhs.into_float_value(), "")
            .into()
    }
}
