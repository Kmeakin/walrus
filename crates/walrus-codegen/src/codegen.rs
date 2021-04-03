#![allow(clippy::cast_possible_truncation)]

use crate::free_vars::FreeVars;
use arena::ArenaMap;
use either::{self, *};
pub use inkwell::context::Context;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    memory_buffer::MemoryBuffer,
    module::Module,
    targets::TargetData,
    types::{AnyType, BasicType, BasicTypeEnum, FunctionType, IntType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};
use std::{collections::HashMap, ops::Index};
use walrus_semantics::{
    builtins::Builtin,
    hir::{
        self, ArithmeticBinop, Binop, CmpBinop, EnumDefId, Expr, ExprId, Field, FieldInit, FnDefId,
        LazyBinop, Lit, MatchCase, Param, Pat, PatId, StructDefId, StructField, Unop, VarId,
    },
    scopes::{self, Denotation},
    ty,
    ty::{FnType, PrimitiveType, Type},
};

mod expr;
mod pat;
mod trivial;
mod types;
mod util;

pub struct Compiler<'ctx> {
    pub llvm: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,

    pub hir: hir::HirData,
    pub scopes: scopes::Scopes,
    pub types: ty::InferenceResult,
}

#[derive(Debug, Clone)]
pub struct Loop<'ctx> {
    body_bb: BasicBlock<'ctx>,
    exit_bb: BasicBlock<'ctx>,
    result_alloca: PointerValue<'ctx>,

    does_continue: bool,
    does_break: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Vars<'a> {
    locals: ArenaMap<VarId, PointerValue<'a>>,
    fns: ArenaMap<FnDefId, FunctionValue<'a>>,
    types: HashMap<Either<StructDefId, EnumDefId>, StructType<'a>>,
    string_type: Option<StructType<'a>>,
    current_loop: Option<Loop<'a>>,
}

impl<'a> Index<VarId> for Vars<'a> {
    type Output = PointerValue<'a>;
    fn index(&self, id: VarId) -> &Self::Output { &self.locals[id] }
}
impl<'a> Index<FnDefId> for Vars<'a> {
    type Output = FunctionValue<'a>;
    fn index(&self, id: FnDefId) -> &Self::Output { &self.fns[id] }
}

type Value<'ctx> = Option<BasicValueEnum<'ctx>>;

impl<'ctx> Compiler<'ctx> {
    #[allow(clippy::useless_transmute)]
    pub fn codegen_module(self) -> String {
        let this: &'static Compiler<'static> = unsafe { std::mem::transmute(&self) };

        let builtins_source = include_str!("builtins.ll");
        let builtins =
            MemoryBuffer::create_from_memory_range_copy(builtins_source.as_bytes(), "builtins");
        let builtins = this
            .llvm
            .create_module_from_ir(builtins)
            .map_err(|e| eprintln!("{}", e.to_string()))
            .unwrap();
        this.module.link_in_module(builtins).unwrap();
        let mut vars = Vars::default();

        for (id, func) in this.hir.fn_defs.iter() {
            let fn_type = this.known_fn_type(&mut vars, &this.types[id]);
            let name = this.hir[func.name].as_str();
            let llvm_fn = this.module.add_function(name, fn_type, None);
            vars.fns.insert(id, llvm_fn);
        }

        for (id, _) in this.hir.fn_defs.iter() {
            this.codegen_fn(&mut vars, id)
        }

        if let Err(e) = this.module.verify() {
            eprintln!("{}", this.module.print_to_string().to_string());
            eprintln!("{}", e.to_string());
            panic!()
        }

        this.module.print_to_string().to_string()
    }

    fn codegen_fn(&'ctx self, vars: &mut Vars<'ctx>, id: FnDefId) {
        let llvm_fn = vars[id];
        let fn_def = &self.hir[id];
        let name = &self.hir[fn_def.name];
        let bb = self
            .llvm
            .append_basic_block(llvm_fn, &format!("{name}.entry"));
        self.builder.position_at_end(bb);

        llvm_fn
            .get_param_iter()
            .zip(&fn_def.params)
            .enumerate()
            .for_each(|(idx, (llvm_param, hir_param))| {
                llvm_param.set_name(&format!("{name}.params.{idx}"));
                self.codegen_pat(vars, hir_param.pat, llvm_param)
            });

        if let Some(value) = self.codegen_expr(vars, fn_def.expr) {
            self.builder.build_return(Some(&value));
        }
    }
}
