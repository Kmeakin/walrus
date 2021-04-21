use super::*;

impl<'ctx> Compiler<'ctx> {
    pub fn codegen_local_var(&self, vars: &mut Vars<'ctx>, var_id: VarId, val: BasicValueEnum) {
        let var_type = &self.types[var_id];
        let name = format!("{}.alloca", self.hir[var_id]);
        let alloca = self
            .builder
            .build_alloca(self.value_type(vars, var_type), &name);
        vars.locals.insert(var_id, alloca);
        self.builder.build_store(alloca, val);
    }

    pub fn codegen_expr(&'ctx self, vars: &mut Vars<'ctx>, id: ExprId) -> OptValue<'ctx> {
        let expr = &self.hir[id];
        match expr {
            Expr::Lit(lit) => Some(self.codegen_lit(vars, lit)),
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
            Expr::Call { func, args } => {
                let args = self.codegen_fn_args(vars, args)?;
                self.codegen_call(vars, *func, &args)
            }
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
                            self.codegen_pat(vars, *pat, val);
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
                self.codegen_fn_wrapper(vars, fn_name, fn_value, fn_type)
            }
            Some(Denotation::Builtin(b)) => self.codegen_builtin_fn_wrapper(vars, b),
            _ => unreachable!("Local variable not bound to a value: {:#?}", var),
        }
    }

    fn codegen_tuple(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        exprs: &[ExprId],
    ) -> OptValue<'ctx> {
        let ty = &self.types[expr];
        let value_type = self.value_type(vars, ty);
        let tuple_alloca = self.builder.build_alloca(value_type, "tuple.alloca");
        for (idx, expr) in exprs.iter().enumerate() {
            let value = self.codegen_expr(vars, *expr)?;
            let field_gep = self.get_tuple_field_gep(tuple_alloca, idx);
            self.builder.build_store(field_gep, value);
        }
        Some(self.builder.build_load(tuple_alloca, "tuple"))
    }

    fn codegen_struct(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        inits: &[FieldInit],
    ) -> OptValue<'ctx> {
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
    ) -> OptValue<'ctx> {
        let enum_id = self.types[expr].as_enum().unwrap();
        let enum_def = &self.hir[enum_id];
        let enum_name = self.enum_name(enum_id);
        let enum_type = self.enum_type(vars, enum_id);
        let enum_alloca = self
            .builder
            .build_alloca(enum_type, &format!("{enum_name}.alloca"));

        if !enum_def.is_empty() {
            // set the discriminant
            let (variant_idx, variant_hir) = enum_def.get_variant(&self.hir, variant).unwrap();
            let discriminant_value = self.enum_discriminant_value(enum_id, variant_idx);
            self.set_enum_discriminant(enum_id, enum_alloca, discriminant_value);

            // set the fields
            let payload_gep = self.get_enum_payload_gep(enum_id, enum_alloca);
            for init in inits {
                let field_value = self.codegen_expr(vars, init.val)?;
                self.set_variant_field(
                    vars,
                    enum_id,
                    variant_hir,
                    payload_gep,
                    init.name,
                    field_value,
                );
            }
        }

        Some(
            self.builder
                .build_load(enum_alloca, &format!("{enum_name}.load")),
        )
    }

    fn codegen_field(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        field: Field,
    ) -> OptValue<'ctx> {
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
    ) -> OptValue<'ctx> {
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

    fn codegen_match(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        parent: ExprId,
        test: ExprId,
        cases: &[MatchCase],
    ) -> OptValue<'ctx> {
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
            let matched = self.codegen_match_attempt(vars, idx, test_value, case.pat);
            self.builder
                .build_conditional_branch(matched, then_bb, next_bb);

            // the then block
            self.builder.position_at_end(then_bb);
            self.codegen_pat(vars, case.pat, test_value);
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

    fn codegen_loop(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        body: ExprId,
    ) -> OptValue<'ctx> {
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

        let ret = match (does_continue, does_break) {
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

    fn codegen_break(&'ctx self, vars: &mut Vars<'ctx>, expr: Option<ExprId>) -> OptValue<'ctx> {
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

    fn codegen_continue(&self, vars: &mut Vars<'ctx>) -> OptValue {
        let Loop {
            body_bb,
            ref mut does_continue,
            ..
        } = vars.current_loop.as_mut().unwrap();
        *does_continue = true;
        self.builder.build_unconditional_branch(*body_bb);
        None
    }

    fn codegen_return(&'ctx self, vars: &mut Vars<'ctx>, expr: Option<ExprId>) -> OptValue<'ctx> {
        let value = match expr {
            Some(expr) => self.codegen_expr(vars, expr)?,
            None => self.codegen_unit(),
        };
        self.builder.build_return(Some(&value));
        None
    }
}
