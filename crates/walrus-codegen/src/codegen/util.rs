use super::*;

impl<'ctx> Compiler<'ctx> {
    pub fn get_struct_field(
        &self,
        struct_id: StructDefId,
        struct_value: BasicValueEnum<'ctx>,
        name: VarId,
    ) -> BasicValueEnum<'ctx> {
        let struct_def = &self.hir[struct_id];
        let struct_name = &self.hir[struct_def.name];

        let (field_idx, field_hir) = struct_def.lookup_field(&self.hir, name).unwrap();
        let field_name = &self.hir[field_hir.name];
        let field_ty = &self.types[field_hir.ty];

        let field = self
            .builder
            .build_extract_value(
                struct_value.into_struct_value(),
                field_idx as u32,
                &format!("{struct_name}.{field_name}"),
            )
            .unwrap();

        if field_ty.is_stack() {
            field
        } else {
            self.builder.build_load(field.into_pointer_value(), "")
        }
    }
}
