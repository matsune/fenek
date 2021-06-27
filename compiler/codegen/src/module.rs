use super::builder::FnBuilder;
use super::llvm;
use super::wrap::Function;
use hir::def::DefId;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{FileType, TargetMachine};
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use std::collections::HashMap;
use std::convert::TryInto;
use types::ty;

pub struct ModuleBuilder<'ctx> {
    pub context: &'ctx Context,
    module: Module<'ctx>,
    pub machine: TargetMachine,
    pub def_fn_value_map: HashMap<DefId, FunctionValue<'ctx>>,
    struct_map: HashMap<ty::StructID, StructType<'ctx>>,
}

impl<'ctx> ModuleBuilder<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        Self {
            context,
            module: context.create_module(name),
            machine: llvm::get_default_machine(),
            def_fn_value_map: HashMap::new(),
            struct_map: HashMap::new(),
        }
    }

    pub fn llvm_intrinsic(&self, name: &str, fn_type: FunctionType<'ctx>) -> FunctionValue<'ctx> {
        llvm::get_llvm_intrinsic(&self.module, name, fn_type)
    }

    pub fn get_llvm_struct_type(&mut self, strukt: &ty::StructType) -> StructType<'ctx> {
        let id = strukt.id;
        if let Some(struct_type) = self.struct_map.get(&id) {
            return *struct_type;
        }
        let struct_type = self.context.opaque_struct_type(&strukt.name);
        let field_types = strukt
            .members
            .borrow()
            .iter()
            .map(|member| self.llvm_basic_type(&member.ty))
            .collect::<Vec<_>>();
        struct_type.set_body(&field_types, false);
        self.struct_map.insert(id, struct_type);
        struct_type
    }

    pub fn llvm_basic_type(&mut self, ty: &ty::Type) -> BasicTypeEnum<'ctx> {
        self.llvm_any_type(ty).try_into().unwrap()
    }

    pub fn llvm_any_type(&mut self, ty: &ty::Type) -> AnyTypeEnum<'ctx> {
        let any_type = match ty {
            ty::Type::Void => self.context.void_type().into(),
            ty::Type::Int(int_ty) => match int_ty {
                ty::IntType::I8 => self.context.i8_type(),
                ty::IntType::I16 => self.context.i16_type(),
                ty::IntType::I32 => self.context.i32_type(),
                ty::IntType::I64 => self.context.i64_type(),
            }
            .into(),
            ty::Type::Float(float_ty) => match float_ty {
                ty::FloatType::F32 => self.context.f32_type(),
                ty::FloatType::F64 => self.context.f64_type(),
            }
            .into(),
            ty::Type::Bool => self.context.bool_type().into(),
            ty::Type::Ptr(ty) => {
                let ty = self.llvm_basic_type(ty);
                ty.ptr_type(AddressSpace::Generic).into()
            }
            ty::Type::Struct(strukt) => self.get_llvm_struct_type(strukt).into(),
            ty::Type::Fun(fun_ty) => {
                let param_types: Vec<BasicTypeEnum<'ctx>> = fun_ty
                    .args
                    .iter()
                    .map(|arg_ty| self.llvm_basic_type(arg_ty))
                    .collect();
                match self.llvm_any_type(&fun_ty.ret) {
                    AnyTypeEnum::VoidType(ty) => ty.fn_type(&param_types, false),
                    AnyTypeEnum::FloatType(ty) => ty.fn_type(&param_types, false),
                    AnyTypeEnum::IntType(ty) => ty.fn_type(&param_types, false),
                    AnyTypeEnum::PointerType(ty) => ty.fn_type(&param_types, false),
                    AnyTypeEnum::StructType(ty) => ty.fn_type(&param_types, false),
                    AnyTypeEnum::VectorType(ty) => ty.fn_type(&param_types, false),
                    AnyTypeEnum::ArrayType(ty) => ty.fn_type(&param_types, false),
                    AnyTypeEnum::FunctionType(ty) => ty,
                }
                .into()
            }
        };
        any_type
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

    pub fn verify(&self) -> Result<(), LLVMString> {
        self.module.verify()
    }

    pub fn build_module(&mut self, module: hir::Module) -> Result<(), LLVMString> {
        for fun in &module.funs {
            let fn_type = self.llvm_any_type(fun.def.ty()).into_function_type();
            let fn_value = self.module.add_function(&fun.name, fn_type, None);
            self.def_fn_value_map.insert(fun.def.id(), fn_value);
        }

        for fun in module.funs {
            self.build_fun(fun);
        }
        // self.module.print_to_stderr();
        self.verify()
    }

    pub fn build_fun(&mut self, fun: hir::Fun) {
        let fn_value = self.def_fn_value_map.get(&fun.def.id()).unwrap();
        // append and set cursor to an entrance basicblock
        let start_bb = self.context.append_basic_block(*fn_value, "start");
        let builder = self.context.create_builder();
        builder.position_at_end(start_bb);
        let function = Function::new(*fn_value, builder);
        FnBuilder::new(self, function).build_fun(&fun);
    }
}
