use hir::def::DefId;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::values::BasicValueEnum;
use inkwell::values::{FunctionValue, PointerValue};
use std::collections::HashMap;
use types::ty;

#[derive(Debug)]
pub struct Function<'ctx> {
    pub fn_value: FunctionValue<'ctx>,
    pub builder: Builder<'ctx>,
    pub var_map: HashMap<DefId, Variable<'ctx>>,
}

impl<'ctx> Function<'ctx> {
    pub fn new(fn_value: FunctionValue<'ctx>, builder: Builder<'ctx>) -> Self {
        Self {
            fn_value,
            builder,
            var_map: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Variable<'ctx> {
    pub name: String,
    pub ty: ty::Type,
    pub is_arg: bool,
    pub ptr: PointerValue<'ctx>,
}

impl<'ctx> Variable<'ctx> {
    pub fn new(name: String, ty: ty::Type, is_arg: bool, ptr: PointerValue<'ctx>) -> Self {
        Self {
            name,
            ty,
            is_arg,
            ptr,
        }
    }
}

pub trait PhiMergeable<'ctx>: Sized {
    fn merge(
        self,
        other: Self,
        this_bb: BasicBlock<'ctx>,
        other_bb: BasicBlock<'ctx>,
        builder: &Builder<'ctx>,
    ) -> Self;
}

impl<'ctx> PhiMergeable<'ctx> for () {
    fn merge(
        self,
        other: Self,
        this_bb: BasicBlock<'ctx>,
        other_bb: BasicBlock<'ctx>,
        builder: &Builder<'ctx>,
    ) -> Self {
    }
}

impl<'ctx> PhiMergeable<'ctx> for BasicValueEnum<'ctx> {
    fn merge(
        self,
        other: Self,
        this_bb: BasicBlock<'ctx>,
        other_bb: BasicBlock<'ctx>,
        builder: &Builder<'ctx>,
    ) -> Self {
        let phi = builder.build_phi(self.get_type(), "mergeValuesEndIf");
        phi.add_incoming(&[(&self, this_bb), (&other, other_bb)]);
        phi.as_basic_value()
    }
}
