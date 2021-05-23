use hir::def::DefId;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use std::collections::HashMap;
use types::ty;

pub fn llvm_intrinsic_type_name(ty: BasicTypeEnum) -> String {
    match ty {
        BasicTypeEnum::ArrayType(_) => unimplemented!(),
        BasicTypeEnum::IntType(ty) => format!("i{}", ty.get_bit_width()),
        BasicTypeEnum::FloatType(_) => unimplemented!(),
        BasicTypeEnum::PointerType(_) => unimplemented!(),
        BasicTypeEnum::StructType(_) => unimplemented!(),
        BasicTypeEnum::VectorType(ty) => format!(
            "v{}{}",
            ty.get_size(),
            llvm_intrinsic_type_name(ty.get_element_type())
        ),
    }
}

pub struct ModuleCtx<'ctx, 'codegen> {
    pub context: &'ctx Context,
    pub module: &'codegen Module<'ctx>,
    pub def_fn_value_map: &'codegen HashMap<DefId, FunctionValue<'ctx>>,
}

pub fn llvm_basic_ty<'ctx>(context: &'ctx Context, ty: &ty::Type) -> BasicTypeEnum<'ctx> {
    match ty {
        ty::Type::Int(int_ty) => match int_ty {
            ty::IntType::I8 => context.i8_type(),
            ty::IntType::I16 => context.i16_type(),
            ty::IntType::I32 => context.i32_type(),
            ty::IntType::I64 => context.i64_type(),
        }
        .into(),
        ty::Type::Float(float_ty) => match float_ty {
            ty::FloatType::F32 => context.f32_type(),
            ty::FloatType::F64 => context.f64_type(),
        }
        .into(),
        ty::Type::Bool => context.bool_type().into(),
        ty::Type::Ptr(ty) => {
            let ty = llvm_basic_ty(context, ty);
            ty.ptr_type(AddressSpace::Generic).into()
        }
        _ => unreachable!(),
    }
}

pub fn get_llvm_intrinsic<'ctx>(
    module: &Module<'ctx>,
    name: &str,
    fn_type: FunctionType<'ctx>,
) -> FunctionValue<'ctx> {
    match module.get_function(name) {
        Some(fn_value) => {
            if fn_value.get_type() == fn_type {
                fn_value
            } else {
                panic!("Requested multiple LLVM intrinsics with same name but different type signatures")
            }
        }
        None => module.add_function(name, fn_type, None),
    }
}
