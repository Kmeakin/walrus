use super::{unify::InferenceTable, InferType, Type};
use crate::{
    diagnostic::Diagnostic,
    hir::{
        self, BinOp, Decl, Expr, ExprId, FnDefId, Lit, Module, Param, Pat, PatId, Stmt, TypeId,
        UnOp, Var,
    },
    scopes::{Binding, Scopes},
};
use la_arena::ArenaMap;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct InferenceResult {
    pub type_of_expr: ArenaMap<ExprId, Type>,
    pub type_of_pat: ArenaMap<PatId, Type>,
    pub type_of_fn: ArenaMap<FnDefId, FnType>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    params: Vec<Type>,
    ret: Type,
}

impl From<FnType> for Type {
    fn from(func: FnType) -> Type { Type::function(func.params, func.ret) }
}

struct Ctx {
    scopes: Scopes,
    module: Module,
    result: InferenceResult,
    table: InferenceTable,
    fn_type: Option<FnType>,
    loop_type: Option<Type>,
}

impl Ctx {
    fn finish(mut self) -> InferenceResult {
        let result = std::mem::take(&mut self.result);
        for (expr, ty) in result.type_of_expr.iter() {
            let was_unknown = ty == &Type::Unknown;
            let ty = self.propagate_type_completely(&ty);
            if !was_unknown && ty == Type::Unknown {
                todo!("Unable to infer type")
            }
        }

        for (pat, ty) in result.type_of_pat.iter() {
            let was_unknown = ty == &Type::Unknown;
            let ty = self.propagate_type_completely(&ty);
            if !was_unknown && ty == Type::Unknown {
                todo!("Unable to infer type")
            }
        }

        result
    }

    fn with_fn_type<T>(&mut self, ty: FnType, f: impl Fn(&mut Self) -> T) -> T {
        let old_fn_type = self.fn_type.clone();
        self.fn_type = Some(ty);
        let ret = f(self);
        self.fn_type = old_fn_type;
        ret
    }

    fn with_loop_type<T>(&mut self, ty: Type, f: impl Fn(&mut Self) -> T) -> T {
        let old_loop_type = self.loop_type.clone();
        self.loop_type = Some(ty);
        let ret = f(self);
        self.loop_type = old_loop_type;
        ret
    }

    fn set_expr_ty(&mut self, expr: ExprId, ty: Type) { self.result.type_of_expr.insert(expr, ty); }
    fn set_pat_ty(&mut self, pat: PatId, ty: Type) { self.result.type_of_pat.insert(pat, ty); }
    fn set_fn_ty(&mut self, fn_def: FnDefId, ty: FnType) {
        self.result.type_of_fn.insert(fn_def, ty);
    }

    fn new_type_var(&mut self) -> Type { Type::Infer(InferType::Var(self.table.new_type_var())) }

    fn resolve_type(&mut self, id: TypeId) -> Type {
        let ty = self.module.data[id].clone();
        match ty {
            hir::Type::Infer => self.new_type_var(),
            hir::Type::Tuple(tys) => Type::tuple(tys.iter().map(|ty| self.resolve_type(*ty))),
            hir::Type::Fn { params, ret } => Type::function(
                params
                    .iter()
                    .map(|ty| self.resolve_type(*ty))
                    .collect::<Vec<_>>(),
                self.resolve_type(ret),
            ),
            hir::Type::Var(var) => {
                let scope = self.scopes.scope_of_type(id);
                let binding = self.scopes.lookup_in_scope(scope, &var);
                match binding {
                    Some(Binding::BuiltinType(ty)) => Type::from(*ty),
                    Some(_) => todo!("Not a type"),
                    None => todo!("Undefined type"),
                }
            }
        }
    }

    /// Propagates the type as far as currently possible, replacing type
    /// variables by their known types. All types returned by the `infer_*`
    /// functions should be resolved as far as possible, i.e. contain no
    /// type variables with known type.
    fn propagate_type_as_far_as_possible(&mut self, ty: &Type) -> Type {
        self.table.propagate_type_as_far_as_possible(ty)
    }

    /// Propagates the type completely; type variables without known type are
    /// replaced by `Type::Unknown`.
    fn propagate_type_completely(&mut self, ty: &Type) -> Type {
        self.table
            .propagate_type_completely_inner(&mut Vec::new(), ty)
    }

    fn propagate_fn_type_completely(&mut self, fn_type: &FnType) -> FnType {
        FnType {
            params: fn_type
                .params
                .iter()
                .map(|param| self.propagate_type_completely(param))
                .collect(),
            ret: self.propagate_type_completely(&fn_type.ret),
        }
    }

    fn try_to_unify(&mut self, expected: &Type, got: &Type) {
        if self.unify(got, expected) {
            todo!("Type mismatch")
        }
    }

    fn try_to_unify_and_propagate_as_far_as_possible(
        &mut self,
        expected: &Type,
        got: &Type,
    ) -> Type {
        self.try_to_unify(expected, got);
        self.propagate_type_as_far_as_possible(got)
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> bool { self.table.unify(t1, t2) }
}

impl Ctx {
    fn infer_module(&mut self) {
        self.module
            .decls
            .clone()
            .iter()
            .copied()
            .for_each(|decl| self.infer_decl(decl));
        self.module
            .decls
            .clone()
            .iter()
            .copied()
            .for_each(|decl| self.infer_decl_body(decl));
    }

    fn infer_decl(&mut self, decl: Decl) {
        match decl {
            Decl::Fn(fn_id) => self.infer_fn_decl(fn_id),
        }
    }

    fn infer_fn_decl(&mut self, fn_id: FnDefId) {
        let fn_decl = self.module.data[fn_id].clone();
        let params = fn_decl
            .params
            .iter()
            .map(|param| self.infer_binding(param.pat, param.ty, None))
            .collect();
        let ret = fn_decl
            .ret_type
            .map_or(Type::UNIT, |ty| self.resolve_type(ty));
        self.set_fn_ty(fn_id, FnType { params, ret });
    }

    fn infer_decl_body(&mut self, decl: Decl) {
        match decl {
            Decl::Fn(fn_id) => self.infer_fn_body(fn_id),
        };
    }

    fn infer_fn_body(&mut self, fn_id: FnDefId) -> Type {
        let fn_decl = self.module.data[fn_id].clone();
        let fn_type = self.result.type_of_fn[fn_id].clone();
        let body_type = self.with_fn_type(fn_type.clone(), |this| {
            this.infer_expr(&Type::Unknown, fn_decl.expr)
        });
        self.try_to_unify_and_propagate_as_far_as_possible(&fn_type.ret, &body_type)
    }

    /// Common inference logic for any construct that binds a value to a pattern
    /// (ie fn params, lambda params, let bindings)
    /// FnParam { pat, ascription } => infer_binding(pat, ascription, None),
    /// LambdaParam { pat, ascription } => infer_binding(pat, ascription, None),
    /// Let { pat, ascription, val} => infer_binding(pat, ascription, val),
    fn infer_binding(&mut self, pat: PatId, ty: Option<TypeId>, expr: Option<ExprId>) -> Type {
        // type annotation takes priority over the expr, which takes priority over the
        // pattern
        // eg `let (x,y): (Bool,Bool,Bool) = (1,2,3);` will create an error pointing at
        // the expr and the pattern, not at the annotation. This is because, when the
        // user provides a type annotation, they usually want it to guide the inference
        // process on expr inference (eg `let x: Vec<_> = xs.iter().collect()`)
        let ty = match ty {
            Some(ty) => self.resolve_type(ty),
            None => self.new_type_var(),
        };
        let expr_ty = match expr {
            Some(val) => self.infer_expr(&ty, val),
            None => ty,
        };
        let expr_ty = self.propagate_type_as_far_as_possible(&expr_ty);
        self.infer_pat(&expr_ty, pat)
    }

    fn infer_pat(&mut self, expected: &Type, id: PatId) -> Type {
        let pat = self.module.data[id].clone();
        let ty = match pat {
            Pat::Var(_) | Pat::Ignore => expected.clone(),
            Pat::Tuple(pats) => {
                let expectations = expected.as_tuple().unwrap_or(&[]);
                let expectations = expectations.iter().chain(std::iter::repeat(&Type::Unknown));
                let tys = pats
                    .iter()
                    .zip(expectations)
                    .map(|(pat, expected)| self.infer_pat(expected, *pat));
                Type::tuple(tys)
            }
        };
        let ty = self.propagate_type_as_far_as_possible(&ty);
        self.set_pat_ty(id, ty.clone());
        self.try_to_unify_and_propagate_as_far_as_possible(expected, &ty)
    }

    fn infer_expr(&mut self, expected: &Type, id: ExprId) -> Type {
        let expr = self.module.data[id].clone();
        let ty = match expr {
            Expr::Lit(lit) => lit.ty(),
            Expr::Var(var) => self.infer_var_expr(&var, id),
            Expr::Tuple(exprs) => self.infer_tuple_expr(expected, &exprs),
            Expr::If {
                test,
                then_branch,
                else_branch,
            } => self.infer_if_expr(test, then_branch, else_branch),
            Expr::Lambda { params, expr } => self.infer_lambda_expr(expected, &params, expr),
            Expr::Call { func, args } => self.infer_call_expr(func, &args),
            Expr::Field { expr, var } => self.infer_field_expr(expr, var),
            Expr::Unop { op, expr } => self.infer_unop_expr(op, expr),
            Expr::Binop { lhs, op, rhs } => self.infer_binop_expr(op, lhs, rhs),
            Expr::Loop(expr) => self.infer_loop_expr(expected, expr),
            Expr::Return(expr) => self.infer_return_expr(expr),
            Expr::Break(expr) => self.infer_break_expr(expr),
            Expr::Continue => self.infer_continue_expr(),
            Expr::Block { stmts, expr } => self.infer_block_expr(expected, &stmts, expr),
        };
        let ty = self.propagate_type_as_far_as_possible(&ty);
        self.set_expr_ty(id, ty.clone());
        self.try_to_unify_and_propagate_as_far_as_possible(expected, &ty)
    }

    fn infer_var_expr(&mut self, var: &Var, expr: ExprId) -> Type {
        let scope = self.scopes.scope_of_expr(expr);
        let binding = self.scopes.lookup_in_scope(scope, &var);
        let ty = match binding {
            Some(Binding::Local(pat_id)) => Ok(self.result.type_of_pat[*pat_id].clone()),
            Some(Binding::Fn(fn_id)) => Ok(Type::from(self.result.type_of_fn[*fn_id].clone())),
            Some(binding @ Binding::BuiltinType(_)) => Err(Diagnostic::NotValue {
                var: var.clone(),
                expr,
                binding: *binding,
            }),
            None => Err(Diagnostic::UnboundVar {
                var: var.clone(),
                expr,
            }),
        };
        match ty {
            Ok(ty) => ty,
            Err(err) => {
                self.result.diagnostics.push(err);
                Type::Unknown
            }
        }
    }

    fn infer_tuple_expr(&mut self, expected: &Type, exprs: &[ExprId]) -> Type {
        let expectations = expected.as_tuple().unwrap_or(&[]);
        let expectations = expectations.iter().chain(std::iter::repeat(&Type::Unknown));
        let tys = exprs
            .iter()
            .zip(expectations)
            .map(|(expr, expected)| self.infer_expr(expected, *expr));
        Type::tuple(tys)
    }

    fn infer_if_expr(
        &mut self,
        test: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    ) -> Type {
        let _test_ty = self.infer_expr(&Type::BOOL, test);
        match else_branch {
            None => {
                self.infer_expr(&Type::Unknown, then_branch);
                Type::UNIT
            }
            Some(else_branch) => {
                let then_ty = self.infer_expr(&Type::Unknown, then_branch);
                let else_ty = self.infer_expr(&Type::Unknown, else_branch);
                if self.unify(&then_ty, &else_ty) {
                    then_ty
                } else {
                    self.result.diagnostics.push(Diagnostic::IfBranchMismatch {
                        then_branch,
                        else_branch,
                        then_ty,
                        else_ty,
                    });
                    Type::Unknown
                }
            }
        }
    }

    fn infer_lambda_expr(&mut self, expected: &Type, params: &[Param], body: ExprId) -> Type {
        let param_tys = params
            .iter()
            .map(|param| self.infer_binding(param.pat, param.ty, None))
            .collect::<Vec<_>>();
        let lambda_ty = FnType {
            params: param_tys,
            ret: expected.clone(),
        };
        self.with_fn_type(lambda_ty.clone(), |this| this.infer_expr(expected, body));
        lambda_ty.into()
    }

    fn infer_call_expr(&mut self, func: ExprId, args: &[ExprId]) -> Type {
        let func_ty = self.infer_expr(&Type::Unknown, func);
        match func_ty.as_fn() {
            None => todo!("Called non function"),
            Some((params, ret)) => {
                if args.len() != params.len() {
                    todo!("Argument count mismatch")
                }
                for (arg, param) in args.iter().zip(params.iter()) {
                    self.infer_expr(param, *arg);
                }
                ret.clone()
            }
        }
    }

    fn infer_field_expr(&mut self, base: ExprId, var: Var) -> Type { todo!() }

    fn infer_unop_expr(&mut self, op: UnOp, expr: ExprId) -> Type {
        match op {
            UnOp::Add | UnOp::Sub => self.infer_expr(&Type::INT, expr),
        }
    }

    fn infer_binop_expr(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId) -> Type {
        let lhs_ty = self.infer_expr(&Type::Unknown, lhs);
        let _rhs_ty = self.infer_expr(&lhs_ty, rhs);
        match op {
            BinOp::Assign => todo!("Assignment not yet supported"),
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                if !lhs_ty.is_num() {
                    todo!("Operator not supported on this type")
                };
                lhs_ty
            }
            BinOp::Eq | BinOp::NotEq => {
                if !lhs_ty.is_eq() {
                    todo!("Operator not supported on this type")
                };
                Type::BOOL
            }
            BinOp::Less | BinOp::LessEq | BinOp::Greater | BinOp::GreaterEq => {
                if !lhs_ty.is_cmp() {
                    todo!("Operator not supported on this type")
                };
                Type::BOOL
            }
        }
    }

    fn infer_loop_expr(&mut self, expected: &Type, expr: ExprId) -> Type {
        let expr = self.module.data[expr].clone();
        let (stmts, expr) = match expr {
            Expr::Block { stmts, expr } => (stmts, expr),
            _ => unreachable!(),
        };
        self.with_loop_type(Type::NEVER, |this| {
            this.infer_block_expr(expected, &stmts, expr);
            this.loop_type.clone().unwrap()
        })
    }

    fn infer_return_expr(&mut self, expr: Option<ExprId>) -> Type {
        let result_type = expr.map_or(Type::UNIT, |expr| self.infer_expr(&Type::Unknown, expr));
        match self.fn_type.clone() {
            None => todo!("Return outside of function"),
            Some(fn_type) => self.try_to_unify(&fn_type.ret, &result_type),
        }
        Type::NEVER
    }

    fn infer_break_expr(&mut self, expr: Option<ExprId>) -> Type {
        let result_type = expr.map_or(Type::UNIT, |expr| self.infer_expr(&Type::Unknown, expr));
        match self.loop_type.clone() {
            None => todo!("Break outside of loop"),
            Some(loop_type) => self.try_to_unify(&loop_type, &result_type),
        }
        Type::NEVER
    }

    fn infer_continue_expr(&mut self) -> Type {
        match self.loop_type.clone() {
            None => todo!("Continue outside of loop"),
            Some(_) => {}
        }
        Type::NEVER
    }

    fn infer_block_expr(&mut self, expected: &Type, stmts: &[Stmt], expr: Option<ExprId>) -> Type {
        for stmt in stmts {
            match stmt {
                Stmt::Expr(expr) => self.infer_expr(&Type::Unknown, *expr),
                Stmt::Let { pat, ty, expr } => self.infer_binding(*pat, ty.clone(), Some(*expr)),
            };
        }

        match expr {
            Some(expr) => self.infer_expr(expected, expr),
            None => Type::UNIT,
        }
    }
}

impl Lit {
    fn ty(&self) -> Type {
        match self {
            Lit::Bool(_) => Type::BOOL,
            Lit::Int(_) => Type::INT,
            Lit::Float(_) => Type::FLOAT,
            Lit::Char(_) => Type::CHAR,
        }
    }
}
