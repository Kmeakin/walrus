use super::*;

impl<'ctx> Compiler<'ctx> {
    pub fn void_ptr_type(&self) -> BasicTypeEnum<'ctx> {
        self.llvm.i8_type().ptr_type(AddressSpace::Generic).into()
    }

    pub fn value_type(&self, vars: &mut Vars<'ctx>, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Fn(func) => self.closure_type(vars, func).into(),
            Type::Struct(id) => self.struct_type(vars, *id).into(),
            Type::Enum(id) => self.enum_type(vars, *id).into(),
            Type::Tuple(tys) => self.tuple_type(vars, tys).into(),
            Type::Primitive(PrimitiveType::Bool) => self.llvm.bool_type().into(),
            Type::Primitive(PrimitiveType::Int | PrimitiveType::Char) => {
                self.llvm.i32_type().into()
            }
            Type::Primitive(PrimitiveType::String) => self.string_type().into(),
            Type::Primitive(PrimitiveType::Float) => self.llvm.f32_type().into(),
            Type::Primitive(PrimitiveType::Never) | Type::Infer(_) | Type::Unknown => {
                unreachable!("This type should not exist at codegen: {:?}", ty)
            }
        }
    }

    pub fn string_type(&self) -> StructType<'ctx> {
        self.llvm.struct_type(
            &[
                self.llvm.i32_type().into(),
                self.llvm.i8_type().ptr_type(AddressSpace::Generic).into(),
            ],
            false,
        )
    }

    // type of toplevel or builtin functions - ie no env ptr needed
    pub fn known_fn_type(&self, vars: &mut Vars<'ctx>, ty: &FnType) -> FunctionType<'ctx> {
        let FnType { params, ret } = ty;
        let params = params
            .iter()
            .map(|ty| self.value_type(vars, ty))
            .collect::<Vec<_>>();

        if ret.as_ref() == &Type::NEVER {
            self.llvm.void_type().fn_type(&params, false)
        } else {
            self.value_type(vars, ret).fn_type(&params, false)
        }
    }

    pub fn fn_type(&self, vars: &mut Vars<'ctx>, ty: &FnType) -> FunctionType<'ctx> {
        let FnType { params, ret } = ty;
        let params = &std::iter::once(self.void_ptr_type())
            .chain(params.iter().map(|ty| self.value_type(vars, ty)))
            .collect::<Vec<_>>();

        if ret.as_ref() == &Type::NEVER {
            self.llvm.void_type().fn_type(params, false)
        } else {
            self.value_type(vars, ret).fn_type(params, false)
        }
    }

    pub fn closure_type(&self, vars: &mut Vars<'ctx>, ty: &FnType) -> StructType<'ctx> {
        self.llvm.struct_type(
            &[
                self.fn_type(vars, ty)
                    .ptr_type(AddressSpace::Generic)
                    .into(),
                self.void_ptr_type(),
            ],
            false,
        )
    }

    pub fn tuple_type(&self, vars: &mut Vars<'ctx>, tys: &[Type]) -> StructType<'ctx> {
        self.llvm.struct_type(
            &tys.iter()
                .map(|ty| self.value_type(vars, ty))
                .collect::<Vec<_>>(),
            false,
        )
    }

    pub fn unit_type(&self) -> StructType<'ctx> { self.llvm.struct_type(&[], false) }

    pub fn aggregate_type(&self, vars: &mut Vars<'ctx>, types: &[StructField]) -> StructType<'ctx> {
        self.llvm
            .struct_type(&self.aggregate_types(vars, types), false)
    }

    fn aggregate_types(
        &self,
        vars: &mut Vars<'ctx>,
        types: &[StructField],
    ) -> Vec<BasicTypeEnum<'ctx>> {
        types
            .iter()
            .map(|field| {
                let ty = &self.types[field.ty];
                if ty.is_stack() {
                    self.value_type(vars, ty)
                } else {
                    self.value_type(vars, ty)
                        .ptr_type(AddressSpace::Generic)
                        .into()
                }
            })
            .collect::<Vec<_>>()
    }

    pub fn struct_type(&self, vars: &mut Vars<'ctx>, id: StructDefId) -> StructType<'ctx> {
        match vars.types.get(&Left(id)) {
            Some(ty) => *ty,
            None => {
                let struct_def = &self.hir[id];
                let struct_name = &self.hir[struct_def.name];
                let ty = self.llvm.opaque_struct_type(struct_name.as_str());
                vars.types.insert(Left(id), ty);
                ty.set_body(&self.aggregate_types(vars, &struct_def.fields), false);
                ty
            }
        }
    }

    pub fn enum_type(&self, vars: &mut Vars<'ctx>, enum_id: EnumDefId) -> StructType<'ctx> {
        match vars.types.get(&Right(enum_id)) {
            Some(ty) => *ty,
            None => {
                let enum_def = &self.hir[enum_id];
                let enum_name = &self.hir[enum_def.name];
                let ty = self.llvm.opaque_struct_type(enum_name.as_str());
                vars.types.insert(Right(enum_id), ty);

                let discriminant = self
                    .enum_discriminant_type(enum_id)
                    .map_or_else(|| self.unit_type().into(), Into::into);

                let target_triple = self.module.get_triple();
                let target_str = target_triple.as_str().to_str().unwrap();
                let layout = TargetData::create(target_str);
                let largest_payload = enum_def
                    .variants
                    .iter()
                    .map(|variant| self.aggregate_type(vars, &variant.fields))
                    .max_by_key(|ty| layout.get_bit_size(&ty.as_any_type_enum()))
                    .unwrap_or_else(|| self.unit_type());

                ty.set_body(&[discriminant, largest_payload.into()], false);
                ty
            }
        }
    }
}
