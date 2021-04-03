#![allow(nontrivial_structural_match)]

use inkwell::values::FloatValue;

use super::*;

/// Unops
impl<'ctx> Compiler<'ctx> {
    fn codegen_bool_not(&'ctx self, lhs: Value<'ctx>) -> Value<'ctx> {
        self.builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs.into_int_value(),
                self.codegen_false(),
                "Bool.neg",
            )
            .into()
    }

    fn codegen_int_neg(&'ctx self, lhs: Value<'ctx>) -> Value<'ctx> {
        self.builder
            .build_int_neg(lhs.into_int_value(), "Int.neg")
            .into()
    }

    fn codegen_float_neg(&'ctx self, lhs: Value<'ctx>) -> Value<'ctx> {
        self.builder
            .build_float_neg(lhs.into_float_value(), "Float.neg")
            .into()
    }

    pub fn codegen_unop(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        op: Unop,
        lhs: ExprId,
    ) -> OptValue<'ctx> {
        let lhs_type = &self.types[lhs];
        let lhs = self.codegen_expr(vars, lhs)?;
        let value = match (op, lhs_type) {
            (Unop::Not, &Type::BOOL) => self.codegen_bool_not(lhs),
            (Unop::Sub, &Type::INT) => self.codegen_int_neg(lhs),
            (Unop::Sub, &Type::FLOAT) => self.codegen_float_neg(lhs),
            (Unop::Add, &(Type::INT | Type::FLOAT)) => lhs,
            _ => unreachable!(
                "Attempt to codegen unop {} {}",
                op,
                lhs_type.to_string(&self.hir)
            ),
        };
        Some(value)
    }
}

/// Binops
impl<'ctx> Compiler<'ctx> {
    pub fn codegen_binop(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        op: Binop,
        rhs: ExprId,
    ) -> OptValue<'ctx> {
        match op {
            Binop::Assign => self.codegen_assign(vars, lhs, rhs),
            Binop::Lazy(LazyBinop::Or) => self.codegen_bool_or(vars, lhs, rhs),
            Binop::Lazy(LazyBinop::And) => self.codegen_bool_and(vars, lhs, rhs),
            Binop::Cmp(op) => self.codegen_cmp(vars, op, lhs, rhs),
            Binop::Arithmetic(op) => self.codegen_arithmetic(vars, op, lhs, rhs),
        }
    }
}

/// Assignment
impl<'ctx> Compiler<'ctx> {
    fn codegen_assign(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        rhs: ExprId,
    ) -> OptValue<'ctx> {
        let lhs = self.codegen_lvalue(vars, lhs)?;
        let rhs = self.codegen_expr(vars, rhs)?;
        self.builder.build_store(lhs, rhs);
        Some(self.codegen_unit())
    }

    fn codegen_lvalue(&self, vars: &mut Vars<'ctx>, id: ExprId) -> Option<PointerValue<'ctx>> {
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
                let struct_id = self.types[*expr].as_struct().unwrap();
                let struct_alloca = self.codegen_lvalue(vars, *expr)?;

                let value = match field {
                    Field::Tuple(idx) => self.get_tuple_field_gep(struct_alloca, *idx as _),
                    Field::Named(name) => {
                        self.get_struct_field_gep(struct_id, struct_alloca, *name)
                    }
                };
                Some(value)
            }
            _ => unreachable!("Attempt to assign non-lvalue: {:#?}", expr),
        }
    }
}

/// Short circuiting boolean operators
impl<'ctx> Compiler<'ctx> {
    fn codegen_bool_or(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        rhs: ExprId,
    ) -> OptValue<'ctx> {
        let bb = self.builder.get_insert_block().unwrap();
        let merge_bb = self.llvm.insert_basic_block_after(bb, "Bool.or.merge");
        let rhs_bb = self.llvm.insert_basic_block_after(bb, "Bool.or.rhs");

        // eval lhs
        let lhs = self.codegen_expr(vars, lhs)?;
        self.builder
            .build_conditional_branch(lhs.into_int_value(), merge_bb, rhs_bb);

        // eval rhs
        self.builder.position_at_end(rhs_bb);
        let rhs = self.codegen_expr(vars, rhs)?;
        self.builder.build_unconditional_branch(merge_bb);

        // merge the branches
        self.builder.position_at_end(merge_bb);
        let phi = self.builder.build_phi(self.bool_type(), "Bool.or.merge");
        phi.add_incoming(&[(&lhs, bb), (&rhs, rhs_bb)]);
        Some(phi.as_basic_value())
    }

    fn codegen_bool_and(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        lhs: ExprId,
        rhs: ExprId,
    ) -> OptValue<'ctx> {
        let bb = self.builder.get_insert_block().unwrap();
        let merge_bb = self.llvm.insert_basic_block_after(bb, "Bool.and.merge");
        let rhs_bb = self.llvm.insert_basic_block_after(bb, "Bool.and.rhs");

        // eval lhs
        let lhs = self.codegen_expr(vars, lhs)?;
        self.builder
            .build_conditional_branch(lhs.into_int_value(), rhs_bb, merge_bb);

        // eval rhs
        self.builder.position_at_end(rhs_bb);
        let rhs = self.codegen_expr(vars, rhs)?;
        self.builder.build_unconditional_branch(merge_bb);

        // merge the branches
        self.builder.position_at_end(merge_bb);
        let phi = self.builder.build_phi(self.bool_type(), "Bool.and.merge");
        phi.add_incoming(&[(&lhs, bb), (&rhs, rhs_bb)]);
        Some(phi.as_basic_value())
    }
}

/// Comparison operators
impl<'ctx> Compiler<'ctx> {
    fn codegen_cmp(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        op: CmpBinop,
        lhs: ExprId,
        rhs: ExprId,
    ) -> OptValue<'ctx> {
        let lhs_type = &self.types[lhs];
        let rhs_type = &self.types[rhs];
        let lhs = self.codegen_expr(vars, lhs)?;
        let rhs = self.codegen_expr(vars, rhs)?;
        let value = match *lhs_type {
            Type::BOOL | Type::INT | Type::CHAR => {
                self.codegen_int_cmp(lhs_type, op, lhs, rhs).into()
            }
            Type::FLOAT => self.codegen_float_cmp(op, lhs, rhs).into(),
            Type::STRING | Type::Tuple(_) | Type::Struct(_) | Type::Enum(_) => todo!(),
            _ => unreachable!(
                "Cannot apply binop {} {} {}",
                lhs_type.to_string(&self.hir),
                op,
                rhs_type.to_string(&self.hir),
            ),
        };
        Some(value)
    }

    fn codegen_int_cmp(
        &'ctx self,
        ty: &Type,
        op: CmpBinop,
        lhs: Value<'ctx>,
        rhs: Value<'ctx>,
    ) -> IntValue<'ctx> {
        let ty = ty.to_string(&self.hir);
        let predicate = match op {
            CmpBinop::Eq => IntPredicate::EQ,
            CmpBinop::NotEq => IntPredicate::NE,
            CmpBinop::Less => IntPredicate::SLT,
            CmpBinop::LessEq => IntPredicate::SLE,
            CmpBinop::Greater => IntPredicate::SGT,
            CmpBinop::GreaterEq => IntPredicate::SGE,
        };
        self.builder.build_int_compare(
            predicate,
            lhs.into_int_value(),
            rhs.into_int_value(),
            &format!("{ty}.{}", op.name()),
        )
    }

    fn codegen_float_cmp(
        &'ctx self,
        op: CmpBinop,
        lhs: Value<'ctx>,
        rhs: Value<'ctx>,
    ) -> IntValue<'ctx> {
        let predicate = match op {
            CmpBinop::Eq => FloatPredicate::OEQ,
            CmpBinop::NotEq => FloatPredicate::ONE,
            CmpBinop::Less => FloatPredicate::OLT,
            CmpBinop::LessEq => FloatPredicate::OLE,
            CmpBinop::Greater => FloatPredicate::OGT,
            CmpBinop::GreaterEq => FloatPredicate::OGE,
        };
        self.builder.build_float_compare(
            predicate,
            lhs.into_float_value(),
            rhs.into_float_value(),
            &format!("Float.{}", op.name()),
        )
    }
}

/// Arithmetic operators
impl<'ctx> Compiler<'ctx> {
    fn codegen_arithmetic(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        op: ArithmeticBinop,
        lhs: ExprId,
        rhs: ExprId,
    ) -> OptValue<'ctx> {
        let lhs_type = &self.types[lhs];
        let rhs_type = &self.types[rhs];
        let lhs = self.codegen_expr(vars, lhs)?;
        let rhs = self.codegen_expr(vars, rhs)?;
        let value = match *lhs_type {
            Type::INT => self.codegen_int_arithmetic(op, lhs, rhs).into(),
            Type::FLOAT => self.codegen_float_arithmetic(op, lhs, rhs).into(),
            Type::STRING if op == ArithmeticBinop::Add => todo!(),
            _ => unreachable!(
                "Cannot apply binop {} {} {}",
                lhs_type.to_string(&self.hir),
                op,
                rhs_type.to_string(&self.hir),
            ),
        };
        Some(value)
    }

    fn codegen_int_arithmetic(
        &self,
        op: ArithmeticBinop,
        lhs: Value<'ctx>,
        rhs: Value<'ctx>,
    ) -> IntValue<'ctx> {
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        match op {
            ArithmeticBinop::Add => self.builder.build_int_add(lhs, rhs, "Int.add"),
            ArithmeticBinop::Sub => self.builder.build_int_sub(lhs, rhs, "Int.sub"),
            ArithmeticBinop::Mul => self.builder.build_int_mul(lhs, rhs, "Int.mul"),
            ArithmeticBinop::Div => self.builder.build_int_signed_div(lhs, rhs, "Int.div"),
        }
    }

    fn codegen_float_arithmetic(
        &self,
        op: ArithmeticBinop,
        lhs: Value<'ctx>,
        rhs: Value<'ctx>,
    ) -> FloatValue<'ctx> {
        let lhs = lhs.into_float_value();
        let rhs = rhs.into_float_value();
        match op {
            ArithmeticBinop::Add => self.builder.build_float_add(lhs, rhs, "Float.add"),
            ArithmeticBinop::Sub => self.builder.build_float_sub(lhs, rhs, "Float.sub"),
            ArithmeticBinop::Mul => self.builder.build_float_mul(lhs, rhs, "Float.mul"),
            ArithmeticBinop::Div => self.builder.build_float_div(lhs, rhs, "Float.div"),
        }
    }
}
