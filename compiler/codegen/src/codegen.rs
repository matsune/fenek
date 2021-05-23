use crate::builder::FnBuilder;
use crate::ctx::*;
use crate::wrap::*;
use hir::def::DefId;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::BasicTypeEnum;
use inkwell::values::FunctionValue;
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use types::ty;

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    def_fn_value_map: HashMap<DefId, FunctionValue<'ctx>>,
    machine: TargetMachine,
}

fn get_default_machine() -> TargetMachine {
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    target
        .create_target_machine(
            &triple,
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap()
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Self {
            context,
            module: context.create_module("fenec"),
            def_fn_value_map: HashMap::new(),
            machine: get_default_machine(),
        }
    }

    fn module_ctx(&self) -> ModuleCtx<'ctx, '_> {
        ModuleCtx {
            context: &self.context,
            module: &self.module,
            def_fn_value_map: &self.def_fn_value_map,
        }
    }

    pub fn emit_llvm_ir<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), LLVMString> {
        self.module.print_to_file(path)
    }

    pub fn emit_llvm_bc<P: AsRef<std::path::Path>>(&self, path: P) -> bool {
        self.module.write_bitcode_to_path(path.as_ref())
    }

    pub fn emit_asm<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), LLVMString> {
        self.machine
            .write_to_file(&self.module, FileType::Assembly, path.as_ref())
    }

    pub fn emit_obj<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), LLVMString> {
        self.machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
    }

    fn create_fn_value(
        &self,
        name: &str,
        ret_ty: &ty::Type,
        arg_tys: &[ty::Type],
    ) -> FunctionValue<'ctx> {
        let param_types: Vec<BasicTypeEnum<'ctx>> = arg_tys
            .iter()
            .map(|arg_ty| llvm_basic_ty(&self.context, arg_ty))
            .collect();
        let fn_type = match ret_ty {
            ty::Type::Int(int_ty) => match int_ty {
                ty::IntType::I8 => self.context.i8_type(),
                ty::IntType::I16 => self.context.i16_type(),
                ty::IntType::I32 => self.context.i32_type(),
                ty::IntType::I64 => self.context.i64_type(),
            }
            .fn_type(&param_types, false),
            ty::Type::Float(float_ty) => match float_ty {
                ty::FloatType::F32 => self.context.f32_type(),
                ty::FloatType::F64 => self.context.f64_type(),
            }
            .fn_type(&param_types, false),
            ty::Type::Bool => self.context.bool_type().fn_type(&param_types, false),
            ty::Type::Void => self.context.void_type().fn_type(&param_types, false),
            _ => unimplemented!(),
        };
        self.module.add_function(name, fn_type, None)
    }

    pub fn build_module(&mut self, module: hir::Module) {
        for fun in &module.funs {
            let fn_value = self.create_fn_value(
                &fun.name,
                &fun.def.ty.as_fun().ret,
                &fun.def.ty.as_fun().args,
            );
            self.def_fn_value_map.insert(fun.def.id, fn_value);
        }

        for fun in module.funs {
            self.build_fun(fun);
        }
    }

    pub fn verify(&self) -> Result<(), LLVMString> {
        self.module.verify()
    }

    pub fn build_fun(&self, fun: hir::Fun) {
        let fn_value = self.def_fn_value_map.get(&fun.def.id).unwrap();
        // append and set cursor to an entrance basicblock
        let start_bb = self.context.append_basic_block(*fn_value, "start");
        let builder = self.context.create_builder();
        builder.position_at_end(start_bb);
        let function = Function::new(*fn_value, builder);
        FnBuilder::new(self.module_ctx(), function).build_fun(&fun);
    }
}
