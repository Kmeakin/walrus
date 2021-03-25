use inkwell::values::StructValue;
use walrus_semantics::hir::{EnumVariant, Var};

use super::*;

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

    pub fn get_struct_field_gep(
        &self,
        struct_id: StructDefId,
        struct_alloca: PointerValue<'ctx>,
        field_name: VarId,
    ) -> PointerValue<'ctx> {
        let struct_def = &self.hir[struct_id];
        let struct_name = self.struct_name(struct_id);
        let (field_idx, _) = struct_def.lookup_field(&self.hir, field_name).unwrap();
        let field_name = &self.hir[field_name];

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
        field_name: VarId,
        field_val: BasicValueEnum<'ctx>,
    ) {
        let struct_def = &self.hir[struct_id];
        let (_, field) = struct_def.lookup_field(&self.hir, field_name).unwrap();
        let field_type = &self.types[field.ty];

        let field_gep = self.get_struct_field_gep(struct_id, struct_alloca, field_name);

        let value = if field_type.is_stack() {
            field_val
        } else {
            let value_ptr = self
                .builder
                .build_alloca(self.value_type(vars, field_type), "");
            self.builder.build_store(value_ptr, field_val);
            value_ptr.into()
        };
        self.builder.build_store(field_gep, value);
    }
}

impl<'ctx> Compiler<'ctx> {
    pub fn get_enum_discriminant(&self, enum_value: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        self.builder
            .build_extract_value(enum_value.into_struct_value(), 0, "")
            .unwrap()
            .into_int_value()
    }

    pub fn get_enum_payload(
        &self,
        enum_id: EnumDefId,
        variant: VarId,
        enum_value: BasicValueEnum<'ctx>,
    ) -> (StructValue<'ctx>, EnumVariant) {
        let enum_def = &self.hir[enum_id];
        let enum_name = &self.hir[enum_def.name];
        let (_, variant) = enum_def.find_variant(&self.hir, variant).unwrap();
        let variant_name = &self.hir[variant.name];

        let val = self
            .builder
            .build_extract_value(
                enum_value.into_struct_value(),
                1,
                &format!("{enum_name}::{variant_name}.payload"),
            )
            .unwrap()
            .into_struct_value();

        (val, variant.clone())
    }

    pub fn get_variant_field(
        &self,
        variant: &EnumVariant,
        payload_value: StructValue<'ctx>,
        name: VarId,
    ) -> BasicValueEnum<'ctx> {
        let (field_idx, field_hir) = variant.lookup_field(&self.hir, name).unwrap();
        let variant_name = &self.hir[variant.name];
        let field_name = &self.hir[field_hir.name];
        let field_ty = &self.types[field_hir.ty];

        let field = self
            .builder
            .build_extract_value(
                payload_value,
                field_idx as u32,
                &format!("{variant_name}.{field_name}"),
            )
            .unwrap();

        if field_ty.is_stack() {
            field
        } else {
            self.builder.build_load(field.into_pointer_value(), "")
        }
    }
}
