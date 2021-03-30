use super::*;
use inkwell::values::StructValue;
use walrus_semantics::hir::{EnumVariant, Var};

pub trait LLVMExt {
    fn with_name(self, name: impl AsRef<str>) -> Self;
}

macro_rules! impl_llvm_ext {
    ($t:ty) => {
        impl LLVMExt for $t {
            fn with_name(self, name: impl AsRef<str>) -> Self {
                self.set_name(name.as_ref());
                self
            }
        }
    };
}

impl_llvm_ext!(BasicValueEnum<'_>);
impl_llvm_ext!(IntValue<'_>);
impl_llvm_ext!(StructValue<'_>);

/// Tuples
impl<'ctx> Compiler<'ctx> {
    pub fn get_tuple_field(
        &self,
        tuple_value: BasicValueEnum<'ctx>,
        idx: usize,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_extract_value(
                tuple_value.into_struct_value(),
                idx as u32,
                &format!("tuple.{idx}"),
            )
            .unwrap()
    }

    pub fn get_tuple_field_gep(
        &self,
        tuple_alloca: PointerValue<'ctx>,
        idx: usize,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_struct_gep(tuple_alloca, idx as u32, &format!("tuple.{idx}.gep"))
            .unwrap()
    }
}

/// Structs
impl<'ctx> Compiler<'ctx> {
    pub fn struct_name(&self, struct_id: StructDefId) -> &Var {
        let struct_def = &self.hir[struct_id];
        &self.hir[struct_def.name]
    }

    pub fn get_struct_field(
        &self,
        struct_id: StructDefId,
        struct_value: BasicValueEnum<'ctx>,
        field: VarId,
    ) -> BasicValueEnum<'ctx> {
        let struct_def = &self.hir[struct_id];
        let struct_name = &self.hir[struct_def.name];

        let (field_idx, field_hir) = struct_def.lookup_field(&self.hir, field).unwrap();
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
            self.builder.build_load(
                field.into_pointer_value(),
                &format!("{struct_name}.{field_name}.load"),
            )
        }
    }

    pub fn get_struct_field_gep(
        &self,
        struct_id: StructDefId,
        struct_alloca: PointerValue<'ctx>,
        field: VarId,
    ) -> PointerValue<'ctx> {
        let struct_def = &self.hir[struct_id];
        let struct_name = self.struct_name(struct_id);
        let (field_idx, _) = struct_def.lookup_field(&self.hir, field).unwrap();
        let field_name = &self.hir[field];

        self.builder
            .build_struct_gep(
                struct_alloca,
                field_idx as u32,
                &format!("{struct_name}.{field_name}.gep"),
            )
            .unwrap()
    }

    pub fn set_struct_field(
        &self,
        vars: &mut Vars<'ctx>,
        struct_id: StructDefId,
        struct_alloca: PointerValue<'ctx>,
        field: VarId,
        field_value: BasicValueEnum<'ctx>,
    ) {
        let struct_name = self.struct_name(struct_id);
        let field_name = &self.hir[field];

        let struct_def = &self.hir[struct_id];
        let (_, field_hir) = struct_def.lookup_field(&self.hir, field).unwrap();
        let field_type = &self.types[field_hir.ty];

        let field_gep = self.get_struct_field_gep(struct_id, struct_alloca, field);

        let value = if field_type.is_stack() {
            field_value
        } else {
            let value_ptr = self.builder.build_alloca(
                self.value_type(vars, field_type),
                &format!("{struct_name}.{field_name}.alloca"),
            );
            self.builder.build_store(value_ptr, field_value);
            value_ptr.into()
        };
        self.builder.build_store(field_gep, value);
    }
}

/// Enums
impl<'ctx> Compiler<'ctx> {
    pub fn enum_name(&self, enum_id: EnumDefId) -> &Var {
        let enum_def = &self.hir[enum_id];
        &self.hir[enum_def.name]
    }

    pub fn enum_discriminant_type(&self, enum_id: EnumDefId) -> Option<IntType<'ctx>> {
        const I0: usize = 1;
        const I8: usize = 2_usize.pow(8);
        const I16: usize = 2_usize.pow(16);
        const I32: usize = 2_usize.pow(32);

        let enum_def = &self.hir[enum_id];

        let ty = match enum_def.len() {
            0..=I0 => return None,
            0..=I8 => self.llvm.i8_type(),
            0..=I16 => self.llvm.i16_type(),
            0..=I32 => self.llvm.i32_type(),
            _ => self.llvm.i64_type(),
        };
        Some(ty)
    }

    pub fn enum_discriminant_value(
        &'ctx self,
        enum_id: EnumDefId,
        variant_idx: usize,
    ) -> BasicValueEnum<'ctx> {
        match self.enum_discriminant_type(enum_id) {
            Some(int_type) => int_type.const_int(variant_idx as u64, false).into(),
            None => self.codegen_unit(),
        }
    }

    pub fn get_enum_discriminant(
        &self,
        enum_id: EnumDefId,
        enum_value: BasicValueEnum<'ctx>,
    ) -> IntValue<'ctx> {
        let enum_name = self.enum_name(enum_id);
        self.builder
            .build_extract_value(
                enum_value.into_struct_value(),
                0,
                &format!("{enum_name}.discriminant"),
            )
            .unwrap()
            .into_int_value()
    }

    pub fn get_enum_discriminant_gep(
        &self,
        enum_id: EnumDefId,
        enum_alloca: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let enum_name = self.enum_name(enum_id);

        self.builder
            .build_struct_gep(enum_alloca, 0, &format!("{enum_name}.discriminant.gep"))
            .unwrap()
    }

    pub fn set_enum_discriminant(
        &self,
        enum_id: EnumDefId,
        enum_alloca: PointerValue<'ctx>,
        discriminant_value: BasicValueEnum<'ctx>,
    ) {
        let gep = self.get_enum_discriminant_gep(enum_id, enum_alloca);
        self.builder.build_store(gep, discriminant_value);
    }

    pub fn get_enum_payload(
        &self,
        enum_id: EnumDefId,
        variant: VarId,
        enum_value: BasicValueEnum<'ctx>,
    ) -> (StructValue<'ctx>, EnumVariant) {
        let enum_name = self.enum_name(enum_id);
        let enum_def = &self.hir[enum_id];
        let (_, variant) = enum_def.get_variant(&self.hir, variant).unwrap();

        let val = self
            .builder
            .build_extract_value(
                enum_value.into_struct_value(),
                1,
                &format!("{enum_name}.payload"),
            )
            .unwrap()
            .into_struct_value();

        (val, variant.clone())
    }

    pub fn get_enum_payload_gep(
        &self,
        enum_id: EnumDefId,
        enum_alloca: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let enum_name = self.enum_name(enum_id);
        self.builder
            .build_struct_gep(enum_alloca, 1, &format!("{enum_name}.payload.gep"))
            .unwrap()
    }

    pub fn get_variant_field(
        &self,
        enum_id: EnumDefId,
        variant: &EnumVariant,
        payload_value: StructValue<'ctx>,
        field: VarId,
    ) -> BasicValueEnum<'ctx> {
        let enum_name = self.enum_name(enum_id);
        let (field_idx, field_hir) = variant.lookup_field(&self.hir, field).unwrap();
        let variant_name = &self.hir[variant.name];
        let field_name = &self.hir[field_hir.name];
        let field_ty = &self.types[field_hir.ty];

        let field = self
            .builder
            .build_extract_value(
                payload_value,
                field_idx as u32,
                &format!("{enum_name}::{variant_name}.{field_name}"),
            )
            .unwrap();

        if field_ty.is_stack() {
            field
        } else {
            self.builder.build_load(
                field.into_pointer_value(),
                &format!("{enum_name}::{variant_name}.{field_name}.load"),
            )
        }
    }

    pub fn get_variant_field_gep(
        &self,
        enum_id: EnumDefId,
        variant: &EnumVariant,
        payload_alloca: PointerValue<'ctx>,
        field: VarId,
    ) -> PointerValue<'ctx> {
        let enum_name = self.enum_name(enum_id);
        let (field_idx, field_hir) = variant.lookup_field(&self.hir, field).unwrap();
        let variant_name = &self.hir[variant.name];
        let field_name = &self.hir[field_hir.name];

        self.builder
            .build_struct_gep(
                payload_alloca,
                field_idx as u32,
                &format!("{enum_name}::{variant_name}.{field_name}.gep"),
            )
            .unwrap()
    }

    pub fn set_variant_field(
        &self,
        vars: &mut Vars<'ctx>,
        enum_id: EnumDefId,
        variant: &EnumVariant,
        payload_alloca: PointerValue<'ctx>,
        field: VarId,
        field_value: BasicValueEnum<'ctx>,
    ) {
        let enum_name = self.enum_name(enum_id);
        let variant_name = &self.hir[variant.name];

        let (_, field_hir) = variant.lookup_field(&self.hir, field).unwrap();
        let field_name = &self.hir[field_hir.name];
        let field_type = &self.types[field_hir.ty];

        let field_gep = self.get_variant_field_gep(enum_id, variant, payload_alloca, field);

        let value = if field_type.is_stack() {
            field_value
        } else {
            let value_ptr = self.builder.build_alloca(
                self.value_type(vars, field_type),
                &format!("{enum_name}::{variant_name}.{field_name}.alloca"),
            );
            self.builder.build_store(value_ptr, field_value);
            value_ptr.into()
        };
        self.builder.build_store(field_gep, value);
    }
}
