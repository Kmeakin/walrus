use super::*;
use inkwell::values::FloatValue;

impl<'ctx> Compiler<'ctx> {
    pub fn codegen_unit(&self) -> BasicValueEnum { self.llvm.const_struct(&[], false).into() }

    pub fn codegen_null_ptr(&self) -> BasicValueEnum { self.void_ptr_type().const_zero() }

    pub fn codegen_undef(&self, vars: &mut Vars<'ctx>, ty: &Type) -> BasicValueEnum {
        match self.value_type(vars, ty) {
            BasicTypeEnum::ArrayType(ty) => ty.get_undef().into(),
            BasicTypeEnum::FloatType(ty) => ty.get_undef().into(),
            BasicTypeEnum::IntType(ty) => ty.get_undef().into(),
            BasicTypeEnum::PointerType(ty) => ty.get_undef().into(),
            BasicTypeEnum::StructType(ty) => ty.get_undef().into(),
            BasicTypeEnum::VectorType(ty) => ty.get_undef().into(),
        }
    }

    pub fn codegen_lit(&self, vars: &mut Vars<'ctx>, lit: &Lit) -> BasicValueEnum<'ctx> {
        match lit {
            Lit::Bool(b) => self.llvm.bool_type().const_int(*b as _, false).into(),
            Lit::Int(val) => self.llvm.i32_type().const_int((*val).into(), false).into(),
            Lit::Float(val) => self.llvm.f32_type().const_float((*val).into()).into(),
            Lit::Char(val) => self.llvm.i32_type().const_int((*val).into(), false).into(),
            Lit::String(val) => self.codegen_string(vars, val),
        }
    }

    pub fn codegen_bool(&self, val: bool) -> IntValue<'ctx> {
        self.llvm.bool_type().const_int(val as _, false)
    }

    pub fn codegen_true(&self) -> IntValue<'ctx> { self.codegen_bool(true) }
    pub fn codegen_false(&self) -> IntValue<'ctx> { self.codegen_bool(false) }

    pub fn codegen_int(&self, val: u32) -> IntValue<'ctx> {
        self.llvm.i32_type().const_int(val.into(), false)
    }

    pub fn codegen_float(&self, val: f32) -> FloatValue<'ctx> {
        self.llvm.f32_type().const_float(val.into())
    }

    pub fn codegen_char(&self, val: char) -> IntValue<'ctx> {
        self.llvm.i32_type().const_int(val as _, false)
    }

    pub fn codegen_string(
        &self,
        vars: &mut Vars<'ctx>,
        val: &smol_str::SmolStr,
    ) -> BasicValueEnum<'ctx> {
        let bytes_type = self.llvm.i8_type().array_type(val.len() as _);
        let global_bytes =
            self.module
                .add_global(bytes_type, Some(AddressSpace::Generic), "String.lit");
        let bytes = self.llvm.i8_type().const_array(
            &val.as_bytes()
                .iter()
                .map(|byte| self.llvm.i8_type().const_int(u64::from(*byte), false))
                .collect::<Vec<_>>(),
        );

        assert!(bytes.is_const());
        global_bytes.set_initializer(&bytes);

        let global_bytes_ptr = global_bytes.as_pointer_value();
        let global_bytes_ptr = self.builder.build_bitcast(
            global_bytes_ptr,
            self.llvm.i8_type().ptr_type(AddressSpace::Generic),
            "",
        );

        let len = self.codegen_int(val.len() as _);
        self.string_type(vars)
            .const_named_struct(&[len.into(), global_bytes_ptr])
            .into()
    }
}
