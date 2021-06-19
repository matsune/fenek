use inkwell::module::Module;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{BasicTypeEnum, FunctionType};
use inkwell::values::FunctionValue;
use inkwell::OptimizationLevel;

pub fn get_default_machine() -> TargetMachine {
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

pub fn intrinsic_type_name(ty: BasicTypeEnum) -> String {
    match ty {
        BasicTypeEnum::ArrayType(_) => unimplemented!(),
        BasicTypeEnum::IntType(ty) => format!("i{}", ty.get_bit_width()),
        BasicTypeEnum::FloatType(_) => unimplemented!(),
        BasicTypeEnum::PointerType(_) => unimplemented!(),
        BasicTypeEnum::StructType(_) => unimplemented!(),
        BasicTypeEnum::VectorType(ty) => format!(
            "v{}{}",
            ty.get_size(),
            intrinsic_type_name(ty.get_element_type())
        ),
    }
}
