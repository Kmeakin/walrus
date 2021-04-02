use super::*;

impl<'ctx> Compiler<'ctx> {
    pub fn codegen_unit(&self) -> BasicValueEnum { self.llvm.const_struct(&[], false).into() }

    pub fn codegen_null_ptr(&self) -> BasicValueEnum { self.void_ptr_type().const_zero().into() }

    pub fn codegen_undef(&self) -> BasicValueEnum { self.llvm.i8_type().get_undef().into() }

    pub fn codegen_lit(&self, lit: Lit) -> BasicValueEnum<'ctx> {
        match lit {
            Lit::Bool(b) => self.llvm.bool_type().const_int(b as _, false).into(),
            Lit::Int(val) => self.llvm.i32_type().const_int(val.into(), false).into(),
            Lit::Float(val) => self.llvm.f32_type().const_float(val.0.into()).into(),
            Lit::Char(val) => self.llvm.i32_type().const_int(val.into(), false).into(),
        }
    }

    pub fn codegen_true(&self) -> IntValue<'ctx> {
        self.llvm.bool_type().const_int(true as _, false)
    }
    pub fn codegen_false(&self) -> IntValue<'ctx> {
        self.llvm.bool_type().const_int(false as _, false)
    }
}
