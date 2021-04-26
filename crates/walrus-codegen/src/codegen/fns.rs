use super::*;

/// Wrapping global functions
impl<'ctx> Compiler<'ctx> {
    /// Wrap a `FunctionValue` in a closure with an empty environment
    pub fn codegen_fn_wrapper(
        &self,
        vars: &mut Vars<'ctx>,
        fn_name: &str,
        fn_value: FunctionValue<'ctx>,
        fn_type: &FnType,
    ) -> BasicValueEnum {
        let code_ptr = fn_value.as_global_value().as_pointer_value();
        let code_ptr = self.builder.build_bitcast(
            code_ptr,
            self.fn_type(vars, fn_type).ptr_type(AddressSpace::Generic),
            &format!("{fn_name}.closure"),
        );
        let closure = self
            .closure_type(vars, fn_type)
            .const_named_struct(&[code_ptr, self.codegen_null_ptr()]);
        closure.into()
    }

    /// Wrap a builtin function in a closure with an empty environment
    pub fn codegen_builtin_fn_wrapper(
        &self,
        vars: &mut Vars<'ctx>,
        builtin: Builtin,
    ) -> BasicValueEnum {
        match builtin {
            Builtin::Fn { name, ty } => {
                let fn_value = self.get_builtin_fn(vars, name, &ty);
                self.codegen_fn_wrapper(vars, name, fn_value, &ty)
            }
            Builtin::Type { .. } => {
                unreachable!("Attempt to codegen non-value builtin: {:#?}", builtin)
            }
        }
    }

    fn get_builtin_fn(
        &self,
        vars: &mut Vars<'ctx>,
        name: &str,
        fn_type: &FnType,
    ) -> FunctionValue<'ctx> {
        match vars.builtin_fns.get(name) {
            Some(fn_value) => *fn_value,
            None => {
                let fn_value = self.module.add_function(
                    &format!("builtin_{name}"),
                    self.known_fn_type(vars, fn_type),
                    None,
                );
                vars.builtin_fns.insert(name.to_string(), fn_value);
                fn_value
            }
        }
    }
}

/// Calling functions
impl<'ctx> Compiler<'ctx> {
    fn build_call(
        &'ctx self,
        fn_value: impl Into<Either<FunctionValue<'ctx>, PointerValue<'ctx>>>,
        args: &[Value<'ctx>],
        fn_name: &str,
    ) -> OptValue<'ctx> {
        let call = self
            .builder
            .build_call(fn_value.into(), args, &format!("{fn_name}.call"))
            .try_as_basic_value();
        match call {
            Left(value) => Some(value),
            Right(_) => None,
        }
    }

    pub fn codegen_fn_args(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        args: &[ExprId],
    ) -> Option<Vec<Value>> {
        args.iter()
            .map(|arg| self.codegen_expr(vars, *arg))
            .collect::<Option<Vec<_>>>()
    }

    fn codegen_known_fn_call(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        fn_id: FnDefId,
        args: &[Value<'ctx>],
    ) -> OptValue<'ctx> {
        let fn_def = &self.hir[fn_id];
        let fn_name = &self.hir[fn_def.name].as_str();
        let fn_value = vars[fn_id];
        self.build_call(fn_value, args, fn_name)
    }

    pub fn codegen_builtin_fn_call(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        name: &str,
        fn_type: &FnType,
        args: &[Value<'ctx>],
    ) -> OptValue<'ctx> {
        let fn_value = self.get_builtin_fn(vars, name, fn_type);
        self.build_call(fn_value, args, name)
    }

    fn codegen_lambda_call(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        func: ExprId,
        args: &[Value<'ctx>],
    ) -> OptValue<'ctx> {
        let mut args = args.to_vec();
        let closure_value = self.codegen_expr(vars, func)?.into_struct_value();
        let code_ptr = self
            .builder
            .build_extract_value(closure_value, 0, "closure.code")
            .unwrap()
            .into_pointer_value();
        let env_ptr = self
            .builder
            .build_extract_value(closure_value, 1, "closure.env")
            .unwrap();
        args.insert(0, env_ptr);
        self.build_call(code_ptr, &args, "lambda")
    }

    pub fn codegen_call(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        func: ExprId,
        args: &[Value<'ctx>],
    ) -> OptValue<'ctx> {
        let func_val = &self.hir[func];
        match func_val {
            Expr::Var(var_id) => {
                let var = &self.hir[*var_id];
                let denotation = self.scopes.lookup_var(*var_id, var);
                match denotation {
                    Some(Denotation::Fn(id)) => self.codegen_known_fn_call(vars, id, args),
                    Some(Denotation::Builtin(Builtin::Fn { name, ty })) => {
                        self.codegen_builtin_fn_call(vars, name, &ty, args)
                    }
                    _ => self.codegen_lambda_call(vars, func, args),
                }
            }
            _ => self.codegen_lambda_call(vars, func, args),
        }
    }
}

/// Constructing lambdas
impl<'ctx> Compiler<'ctx> {
    pub fn codegen_lambda(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        expr: ExprId,
        params: &[Param],
        body: ExprId,
    ) -> BasicValueEnum {
        let free_vars = self.free_vars(expr);

        let code_ptr = self
            .codegen_lambda_body(
                // this clone is necessary. Dont delete it!
                &mut vars.clone(),
                &free_vars,
                expr,
                params,
                body,
            )
            .as_global_value()
            .as_pointer_value();

        let closure_type = self.types[expr].as_fn().unwrap();
        let closure_alloca = self
            .builder
            .build_alloca(self.closure_type(vars, closure_type), "closure.alloca");
        let code_gep = self
            .builder
            .build_struct_gep(closure_alloca, 0, "closure.code")
            .unwrap();
        self.builder.build_store(code_gep, code_ptr);

        let env_gep = self
            .builder
            .build_struct_gep(closure_alloca, 1, "closure.env")
            .unwrap();

        // store free variables
        let free_vars_type = self.tuple_type(
            vars,
            &free_vars
                .iter()
                .map(|(pat, _)| self.types[pat].clone())
                .collect::<Vec<_>>(),
        );
        let env_alloca = self
            .builder
            .build_malloc(free_vars_type, "env.malloc")
            .unwrap();
        for (idx, (free_var, ())) in free_vars.iter().enumerate() {
            let gep = self
                .builder
                .build_struct_gep(env_alloca, idx as u32, &format!("env.{idx}.gep"))
                .unwrap();
            let val = self
                .builder
                .build_load(vars[free_var], &format!("env.{idx}"));
            self.builder.build_store(gep, val);
        }
        let env_alloca = self
            .builder
            .build_bitcast(env_alloca, self.void_ptr_type(), "env");
        self.builder.build_store(env_gep, env_alloca);
        self.builder.build_load(closure_alloca, "closure")
    }

    fn codegen_lambda_body(
        &'ctx self,
        vars: &mut Vars<'ctx>,
        free_vars: &FreeVars,
        expr: ExprId,
        params: &[Param],
        body: ExprId,
    ) -> FunctionValue<'ctx> {
        let fn_type = self.fn_type(vars, self.types[expr].as_fn().unwrap());
        let llvm_fn = self.module.add_function("lambda", fn_type, None);
        let old_bb = self.builder.get_insert_block().unwrap();

        let bb = self.llvm.append_basic_block(llvm_fn, "lambda.entry");
        self.builder.position_at_end(bb);

        // load free vars
        let free_vars_type = self.tuple_type(
            vars,
            &free_vars
                .iter()
                .map(|(pat, _)| self.types[pat].clone())
                .collect::<Vec<_>>(),
        );

        let env_param = llvm_fn.get_nth_param(0).unwrap();
        env_param.set_name("env_ptr");
        let env_ptr = self.builder.build_bitcast(
            env_param,
            free_vars_type.ptr_type(AddressSpace::Generic),
            "env_ptr",
        );
        let env = self.builder.build_load(env_ptr.into_pointer_value(), "env");
        for (idx, (free_var, ())) in free_vars.iter().enumerate() {
            let val = self
                .builder
                .build_extract_value(env.into_struct_value(), idx as u32, &format!("env.{idx}"))
                .unwrap();
            self.codegen_local_var(vars, free_var, val)
        }

        // load params
        llvm_fn
            .get_param_iter()
            .skip(1)
            .zip(params)
            .enumerate()
            .for_each(|(idx, (llvm_param, hir_param))| {
                llvm_param.set_name(&format!("params.{idx}"));
                self.codegen_pat(vars, hir_param.pat, llvm_param)
            });

        if let Some(value) = self.codegen_expr(vars, body) {
            self.builder.build_return(Some(&value));
        }

        self.builder.position_at_end(old_bb);

        llvm_fn
    }
}
