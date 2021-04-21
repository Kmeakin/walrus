use super::*;

impl<'ctx> Compiler<'ctx> {
    pub fn codegen_pat(&self, vars: &mut Vars<'ctx>, pat_id: PatId, value: BasicValueEnum<'ctx>) {
        let pat = &self.hir[pat_id];
        match pat {
            Pat::Lit(_) | hir::Pat::Ignore => {}
            hir::Pat::Var { var, .. } => self.codegen_local_var(vars, *var, value),
            hir::Pat::Tuple(pats) => pats.iter().enumerate().for_each(|(idx, id)| {
                let val = self.get_tuple_field(value, idx);
                self.codegen_pat(vars, *id, val)
            }),
            hir::Pat::Struct { fields, .. } => {
                let struct_id = self.types[pat_id].as_struct().unwrap();
                for field in fields {
                    let val = self.get_struct_field(struct_id, value, field.name);
                    match field.pat {
                        None => self.codegen_local_var(vars, field.name, val),
                        Some(pat) => self.codegen_pat(vars, pat, val),
                    }
                }
            }
            Pat::Enum {
                variant, fields, ..
            } => {
                let enum_id = self.types[pat_id].as_enum().unwrap();
                let (payload, variant) = self.get_enum_payload(enum_id, *variant, value);
                fields.iter().for_each(|field| {
                    let val = self.get_variant_field(enum_id, &variant, payload, field.name);
                    match field.pat {
                        None => self.codegen_local_var(vars, field.name, val),
                        Some(pat) => self.codegen_pat(vars, pat, val),
                    }
                })
            }
        }
    }

    pub fn codegen_match_attempt(
        &self,
        case_idx: usize,
        test: BasicValueEnum<'ctx>,
        pat_id: PatId,
    ) -> IntValue<'ctx> {
        let pat = &self.hir[pat_id];

        match pat {
            Pat::Lit(lit) => match lit {
                Lit::Bool(b) => self.builder.build_int_compare(
                    IntPredicate::EQ,
                    test.into_int_value(),
                    self.codegen_bool(*b),
                    "Bool.eq",
                ),
                Lit::Int(i) => self.builder.build_int_compare(
                    IntPredicate::EQ,
                    test.into_int_value(),
                    self.codegen_int(*i),
                    "Int.eq",
                ),
                Lit::Char(c) => self.builder.build_int_compare(
                    IntPredicate::EQ,
                    test.into_int_value(),
                    self.codegen_char(*c),
                    "Char.eq",
                ),
                Lit::Float(f) => self.builder.build_float_compare(
                    FloatPredicate::OEQ,
                    test.into_float_value(),
                    self.codegen_float(*f),
                    "Float.eq",
                ),
                Lit::String(s) => todo!(),
            },
            Pat::Var { .. } | Pat::Ignore => self.codegen_true(),
            Pat::Tuple(pats) => self.codegen_all(pats.iter().enumerate().map(|(idx, pat)| {
                let val = self.get_tuple_field(test, idx);
                self.codegen_match_attempt(idx, val, *pat)
            })),
            Pat::Struct { fields, .. } => {
                let struct_id = self.types[pat_id].as_struct().unwrap();
                self.codegen_all(fields.iter().map(|field| match field.pat {
                    None => self.codegen_true(),
                    Some(pat) => {
                        let val = self.get_struct_field(struct_id, test, field.name);
                        self.codegen_match_attempt(case_idx, val, pat)
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
                    enum_def.get_variant(&self.hir, *variant).unwrap();
                let variant_name = &self.hir[enum_variant.name];

                let discriminant_matched = match self.enum_discriminant_type(enum_id) {
                    None => self.codegen_true(),
                    Some(int_type) => {
                        let discriminant = self.get_enum_discriminant(enum_id, test);
                        self.builder.build_int_compare(
                            IntPredicate::EQ,
                            discriminant,
                            int_type.const_int(variant_idx as u64, false),
                            &format!("{enum_name}::{variant_name}.cmp_discriminant"),
                        )
                    }
                };
                let bb = self.builder.get_insert_block().unwrap();
                let end_bb = self.llvm.insert_basic_block_after(
                    bb,
                    &format!("match.case{case_idx}.{enum_name}::{variant_name}.end"),
                );
                let then_bb = self.llvm.insert_basic_block_after(
                    bb,
                    &format!("match.case{case_idx}.{enum_name}::{variant_name}.then"),
                );

                self.builder
                    .build_conditional_branch(discriminant_matched, then_bb, end_bb);

                // then branch
                self.builder.position_at_end(then_bb);
                let (payload, variant) = self.get_enum_payload(enum_id, *variant, test);
                let then_value = self.codegen_all(fields.iter().map(|field| match field.pat {
                    None => self.codegen_true(),
                    Some(pat) => {
                        let val = self.get_variant_field(enum_id, &variant, payload, field.name);
                        self.codegen_match_attempt(case_idx, val, pat)
                    }
                }));
                self.builder.build_unconditional_branch(end_bb);

                // merge the 2 branches
                self.builder.position_at_end(end_bb);
                let phi = self.builder.build_phi(
                    self.llvm.bool_type(),
                    &format!("match.case{case_idx}.{enum_name}::{variant_name}.phi"),
                );
                phi.add_incoming(&[(&then_value, then_bb), (&self.codegen_false(), bb)]);
                phi.as_basic_value().into_int_value()
            }
        }
    }

    fn codegen_all(&self, bools: impl Iterator<Item = IntValue<'ctx>>) -> IntValue<'ctx> {
        bools.fold(self.codegen_true(), |acc, e| {
            self.builder.build_and(acc, e, "")
        })
    }
}
