use super::*;
use inkwell::values::FloatValue;

impl<'ctx> Compiler<'ctx> {
    pub fn codegen_unit(&self) -> BasicValueEnum { self.llvm.const_struct(&[], false).into() }

    pub fn codegen_null_ptr(&self) -> BasicValueEnum { self.void_ptr_type().const_zero() }

    pub fn codegen_undef(&self) -> BasicValueEnum { self.llvm.i8_type().get_undef().into() }

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
        let len = self.codegen_int(val.len() as _);

        // TODO: this truncates the string at the first null byte
        let bytes = self.builder.build_global_string_ptr(val, "string");
        self.string_type(vars)
            .const_named_struct(&[len.into(), bytes.as_pointer_value().into()])
            .into()
    }
}
