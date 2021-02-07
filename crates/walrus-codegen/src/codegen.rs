use std::{cell::RefCell, collections::HashMap};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, IntPredicate,
};
use walrus_semantics::{hir, ty};

pub struct CodegenModule {
    pub hir: hir::Module,
    pub tys: ty::InferenceResult,
}

pub struct Compiler<'ctx> {
    pub llvm: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub cached_types: RefCell<HashMap<Type, BasicTypeEnum<'ctx>>>,
}

type Type = ty::Type<!, !>;
type FnType = ty::FnType<!, !>;
type TypeApp = ty::TypeApp<!, !>;

impl<'ctx> Compiler<'ctx> {
    fn void_ptr_type(&self) -> BasicTypeEnum {
        self.llvm.i8_type().ptr_type(AddressSpace::Generic).into()
    }

    fn value_type(&self, ty: &Type) -> BasicTypeEnum {
        let Type::App(TypeApp { ctor, params }) = ty;
        match ctor {
            ty::TypeCtor::Bool => self.llvm.bool_type().into(),
            ty::TypeCtor::Int => self.llvm.i32_type().into(),
            ty::TypeCtor::Float => self.llvm.f32_type().into(),
            ty::TypeCtor::Char => self.llvm.i32_type().into(),
            ty::TypeCtor::Tuple => self
                .llvm
                .struct_type(
                    &params
                        .iter()
                        .map(|ty| self.value_type(ty))
                        .collect::<Vec<_>>(),
                    false,
                )
                .into(),
            ty::TypeCtor::Fn => self.closure_type(ty.as_fn().unwrap()),
            ty::TypeCtor::Never => todo!(),
            ty::TypeCtor::Struct(_) => todo!(),
        }
    }

    fn fn_type(&self, ty: &FnType) -> FunctionType {
        let FnType { params, ret } = ty;
        self.value_type(ret).fn_type(
            &std::iter::once(self.void_ptr_type())
                .chain(params.iter().map(|ty| self.value_type(ty)))
                .collect::<Vec<_>>(),
            false,
        )
    }

    fn closure_type(&self, ty: FnType) -> BasicTypeEnum {
        let struct_type = self.llvm.struct_type(
            &[
                self.fn_type(&ty).ptr_type(AddressSpace::Generic).into(),
                self.void_ptr_type(),
            ],
            false,
        );
        struct_type.into()
    }

    fn tuple_type(&self, tys: &[Type]) -> StructType {
        self.llvm.struct_type(
            &tys.iter().map(|ty| self.value_type(ty)).collect::<Vec<_>>(),
            true,
        )
    }

    fn codegen_module(self, module: &CodegenModule) -> Module<'ctx> {
        for (id, func) in module.hir.data.fn_defs.iter() {
            let fn_ty = ();
        }
        todo!()
    }
}
