use super::*;
use crate::diagnostic::LitError;
use ordered_float::OrderedFloat;

pub fn lower(syntax: &syntax::SourceFile) -> Module {
    let mut ctx = Ctx::default();
    let decls = syntax
        .decls
        .iter()
        .map(|decl| ctx.lower_decl(decl))
        .collect();
    let Ctx {
        data,
        source,
        diagnostics,
    } = ctx;
    Module {
        decls,
        data,
        source,
        diagnostics,
    }
}

#[derive(Default)]
struct Ctx {
    data: ModuleData,
    source: ModuleSource,
    diagnostics: Vec<Diagnostic>,
}

impl Ctx {
    fn alloc_var(&mut self, syntax: syntax::Var, hir: Var) -> VarId {
        let id = self.data.vars.alloc(hir);
        self.source.vars.insert(id, syntax);
        id
    }

    fn alloc_fn_def(&mut self, syntax: syntax::FnDef, hir: FnDef) -> FnDefId {
        let id = self.data.fn_defs.alloc(hir);
        self.source.fn_defs.insert(id, syntax);
        id
    }

    fn alloc_struct_def(&mut self, syntax: syntax::StructDef, hir: StructDef) -> StructDefId {
        let id = self.data.struct_defs.alloc(hir);
        self.source.struct_defs.insert(id, syntax);
        id
    }
    fn alloc_enum_def(&mut self, syntax: syntax::EnumDef, hir: EnumDef) -> EnumDefId {
        let id = self.data.enum_defs.alloc(hir);
        self.source.enum_defs.insert(id, syntax);
        id
    }

    fn alloc_expr(&mut self, syntax: syntax::Expr, hir: Expr) -> ExprId {
        let id = self.data.exprs.alloc(hir);
        self.source.exprs.insert(id, syntax);
        id
    }

    fn alloc_type(&mut self, syntax: syntax::Type, hir: Type) -> TypeId {
        let id = self.data.types.alloc(hir);
        self.source.types.insert(id, syntax);
        id
    }

    fn alloc_pat(&mut self, syntax: syntax::Pat, hir: Pat) -> PatId {
        let id = self.data.pats.alloc(hir);
        self.source.pats.insert(id, syntax);
        id
    }
}

impl Ctx {
    fn lower_var(&mut self, syntax: syntax::Var) -> VarId {
        let hir = Var(syntax.0.text.clone());
        self.alloc_var(syntax, hir)
    }

    fn lower_decl(&mut self, syntax: &syntax::Decl) -> Decl {
        match syntax {
            syntax::Decl::Fn(syntax) => Decl::Fn(self.lower_fn_def(syntax)),
            syntax::Decl::Struct(syntax) => Decl::Struct(self.lower_struct_def(syntax)),
            syntax::Decl::Enum(syntax) => Decl::Enum(self.lower_enum_def(syntax)),
        }
    }

    fn lower_param(&mut self, syntax: &syntax::Param) -> Param {
        let syntax::Param { pat, ascription } = syntax;
        Param {
            pat: self.lower_pat(pat),
            ty: ascription.as_ref().map(|a| self.lower_type(&a.ty)),
        }
    }

    fn lower_param_list(&mut self, syntax: &syntax::ParamList) -> Vec<Param> {
        syntax
            .0
            .inner
            .iter()
            .map(|param| self.lower_param(param))
            .collect()
    }

    fn lower_fn_def(&mut self, syntax: &syntax::FnDef) -> FnDefId {
        let hir = FnDef {
            name: self.lower_var(syntax.name.clone()),
            params: self.lower_param_list(&syntax.params),
            ret_type: syntax.ret.as_ref().map(|ret| self.lower_type(&ret.ty)),
            expr: self.lower_expr(&syntax.expr),
        };
        self.alloc_fn_def(syntax.clone(), hir)
    }

    fn lower_struct_def(&mut self, syntax: &syntax::StructDef) -> StructDefId {
        let hir = StructDef {
            name: self.lower_var(syntax.name.clone()),
            fields: syntax
                .fields
                .inner
                .iter()
                .map(|field| self.lower_struct_field(field))
                .collect(),
        };
        self.alloc_struct_def(syntax.clone(), hir)
    }

    fn lower_struct_field(&mut self, syntax: &syntax::StructField) -> StructField {
        StructField {
            name: self.lower_var(syntax.name.clone()),
            ty: self.lower_type(&syntax.ty),
        }
    }

    fn lower_enum_def(&mut self, syntax: &syntax::EnumDef) -> EnumDefId {
        let hir = EnumDef {
            name: self.lower_var(syntax.name.clone()),
            variants: syntax
                .variants
                .inner
                .iter()
                .map(|variant| self.lower_enum_variant(variant))
                .collect(),
        };
        self.alloc_enum_def(syntax.clone(), hir)
    }

    fn lower_enum_variant(&mut self, syntax: &syntax::EnumVariant) -> EnumVariant {
        EnumVariant {
            name: self.lower_var(syntax.name.clone()),
            fields: syntax
                .fields
                .inner
                .iter()
                .map(|field| self.lower_struct_field(field))
                .collect(),
        }
    }

    fn lower_type(&mut self, syntax: &syntax::Type) -> TypeId {
        let hir = match syntax {
            syntax::Type::Var(var) => Type::Var(self.lower_var(var.clone())),
            syntax::Type::Infer(_) => Type::Infer,
            syntax::Type::Paren(ty) => return self.lower_type(&ty.inner),
            syntax::Type::Tuple(tys) => {
                Type::Tuple(tys.inner.iter().map(|ty| self.lower_type(ty)).collect())
            }
            syntax::Type::Fn { args, ret } => Type::Fn {
                params: args.inner.iter().map(|ty| self.lower_type(ty)).collect(),
                ret: self.lower_type(&ret.ty),
            },
        };
        self.alloc_type(syntax.clone(), hir)
    }

    fn lower_pat(&mut self, syntax: &syntax::Pat) -> PatId {
        let hir = match syntax {
            syntax::Pat::Var(var) => Pat::Var(self.lower_var(var.clone())),
            syntax::Pat::Ignore(_) => Pat::Ignore,
            syntax::Pat::Paren(pat) => return self.lower_pat(&pat.inner),
            syntax::Pat::Tuple(pats) => {
                Pat::Tuple(pats.inner.iter().map(|pat| self.lower_pat(pat)).collect())
            }
        };
        self.alloc_pat(syntax.clone(), hir)
    }

    fn lower_stmt(&mut self, syntax: &syntax::Stmt) -> Option<Stmt> {
        let stmt = match syntax {
            syntax::Stmt::Expr { expr, .. } => Stmt::Expr(self.lower_expr(expr)),
            syntax::Stmt::Let {
                pat,
                ascription,
                expr,
                ..
            } => Stmt::Let {
                pat: self.lower_pat(pat),
                ty: ascription.as_ref().map(|a| self.lower_type(&a.ty)),
                expr: self.lower_expr(expr),
            },
            syntax::Stmt::Semicolon(semicolon) => {
                self.diagnostics
                    .push(Diagnostic::UnnecessarySemicolon(*semicolon));
                return None;
            }
        };
        Some(stmt)
    }

    fn lower_expr(&mut self, syntax: &syntax::Expr) -> ExprId {
        let hir = match syntax {
            syntax::Expr::Lit(lit) => Expr::Lit(self.lower_lit(lit)),
            syntax::Expr::Var(var) => Expr::Var(self.lower_var(var.clone())),
            syntax::Expr::Paren(expr) => return self.lower_expr(&expr.inner),
            syntax::Expr::Tuple(exprs) => Expr::Tuple(
                exprs
                    .inner
                    .iter()
                    .map(|expr| self.lower_expr(expr))
                    .collect(),
            ),
            syntax::Expr::Lambda(expr) => Expr::Lambda {
                params: self.lower_param_list(&expr.params),
                expr: self.lower_expr(&expr.expr),
            },
            syntax::Expr::Unary(expr) => Expr::Unop {
                op: expr.op.into(),
                expr: self.lower_expr(&expr.expr),
            },
            syntax::Expr::Binary(expr) => Expr::Binop {
                lhs: self.lower_expr(&expr.lhs),
                op: expr.op.into(),
                rhs: self.lower_expr(&expr.rhs),
            },
            syntax::Expr::Call(expr) => Expr::Call {
                func: self.lower_expr(&expr.func),
                args: expr
                    .args
                    .0
                    .inner
                    .iter()
                    .map(|expr| self.lower_expr(expr))
                    .collect(),
            },
            syntax::Expr::Field(expr) => Expr::Field {
                expr: self.lower_expr(&expr.base),
                field: match &expr.field {
                    syntax::Field::Tuple(int) => Field::Tuple(self.lower_int(&int.text, 10)),
                    syntax::Field::Named(var) => Field::Named(self.lower_var(var.clone())),
                },
            },
            syntax::Expr::Struct(expr) => Expr::Struct {
                name: self.lower_var(expr.name.clone()),
                fields: expr
                    .fields
                    .inner
                    .iter()
                    .map(|field| StructExprField {
                        name: self.lower_var(field.name.clone()),
                        val: self.lower_expr(&field.val),
                    })
                    .collect(),
            },
            syntax::Expr::Enum(expr) => Expr::Enum {
                name: self.lower_var(expr.name.clone()),
                variant: self.lower_var(expr.variant.clone()),
                fields: expr
                    .fields
                    .inner
                    .iter()
                    .map(|field| StructExprField {
                        name: self.lower_var(field.name.clone()),
                        val: self.lower_expr(&field.val),
                    })
                    .collect(),
            },
            syntax::Expr::If(expr) => self.lower_if_expr(expr),
            syntax::Expr::Return(expr) => {
                Expr::Return(expr.expr.as_ref().map(|expr| self.lower_expr(expr)))
            }
            syntax::Expr::Break(expr) => {
                Expr::Break(expr.expr.as_ref().map(|expr| self.lower_expr(expr)))
            }
            syntax::Expr::Continue(_) => Expr::Continue,
            syntax::Expr::Loop(expr) => Expr::Loop(self.lower_expr(&expr.expr)),
            syntax::Expr::Block(block) => {
                let stmts = block
                    .stmts
                    .iter()
                    .filter_map(|stmt| self.lower_stmt(stmt))
                    .collect();
                let expr = block
                    .expr
                    .as_ref()
                    .as_ref()
                    .map(|expr| self.lower_expr(expr));
                Expr::Block { stmts, expr }
            }
        };
        self.alloc_expr(syntax.clone(), hir)
    }

    fn lower_if_expr(&mut self, syntax: &syntax::IfExpr) -> Expr {
        Expr::If {
            test: self.lower_expr(&syntax.test_expr),
            then_branch: self.lower_expr(&syntax.then_branch),
            else_branch: match &syntax.else_branch {
                None => None,
                Some(syntax::ElseExpr::ElseBlock { block, .. }) => Some(self.lower_expr(block)),
                Some(syntax::ElseExpr::ElseIf { if_expr, .. }) => Some(self.lower_expr(if_expr)),
            },
        }
    }

    fn lower_lit(&mut self, syntax: &syntax::Lit) -> Lit {
        use syntax::{BoolLit, FloatLit, IntLit, Lit::*};
        match syntax {
            Bool(BoolLit::True(_)) => Lit::Bool(true),
            Bool(BoolLit::False(_)) => Lit::Bool(false),
            Int(IntLit::Dec(int)) => Lit::Int(self.lower_int(&int.text, 10)),
            Int(IntLit::Bin(int)) => Lit::Int(self.lower_int(&int.text["0b".len()..], 2)),
            Int(IntLit::Hex(int)) => Lit::Int(self.lower_int(&int.text["0x".len()..], 16)),
            Float(FloatLit(float)) => Lit::Float(self.lower_float(&float.text)),
            Char(c) => Lit::Char(self.lower_char(c)),
        }
    }

    fn lower_int(&mut self, text: &str, radix: u32) -> u32 {
        let text = text.replace("_", "");
        match u32::from_str_radix(&text, radix) {
            Ok(x) => x,
            Err(err) => {
                self.diagnostics
                    .push(Diagnostic::BadLit(LitError::Int(err)));
                0
            }
        }
    }

    fn lower_float(&mut self, text: &str) -> OrderedFloat<f32> {
        let text = text.replace("_", "");
        match text.parse() {
            Ok(x) => OrderedFloat(x),
            Err(err) => {
                self.diagnostics
                    .push(Diagnostic::BadLit(LitError::Float(err)));
                OrderedFloat(0.0)
            }
        }
    }

    fn lower_char(&mut self, c: &syntax::CharLit) -> char {
        use syntax::CharLit::*;

        match c {
            Simple(c) => c.text.chars().nth(1).unwrap(),
            Escaped(c) => {
                let c = c.text.chars().nth(2).unwrap();
                match c {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '0' => '\0',
                    '\'' => '\'',
                    '"' => '"',
                    _ => {
                        self.diagnostics
                            .push(Diagnostic::BadLit(LitError::EscapeChar(c)));
                        '\0'
                    }
                }
            }
            Unicode(c) => {
                let text = &c.text;
                let len = text.len();
                let digits = &text["'\\u".len()..len - "'".len()];
                let val = self.lower_int(digits, 16);
                match std::char::from_u32(val) {
                    Some(c) => c,
                    None => {
                        self.diagnostics
                            .push(Diagnostic::BadLit(LitError::UnicodeChar(val)));
                        '\0'
                    }
                }
            }
        }
    }
}
