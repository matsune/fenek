use super::ty::*;
use crate::ty::{FloatType, IntType};
use typed_arena::Arena;

#[derive(Default)]
pub struct InferTyArena<'a> {
    pub inner: Arena<InferTy<'a>>,
}

impl<'a> InferTyArena<'a> {
    fn alloc(&'a self, kind: TyKind<'a>) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::new(self.inner.len(), kind))
    }

    pub fn alloc_i8(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Int(IntType::I8))
    }

    pub fn alloc_i16(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Int(IntType::I16))
    }

    pub fn alloc_i32(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Int(IntType::I32))
    }

    pub fn alloc_i64(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Int(IntType::I64))
    }

    pub fn alloc_f32(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Float(FloatType::F32))
    }

    pub fn alloc_f64(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Float(FloatType::F64))
    }

    pub fn alloc_int_lit(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::IntLit)
    }

    pub fn alloc_float_lit(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::FloatLit)
    }

    pub fn alloc_void(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Void)
    }

    pub fn alloc_bool(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Bool)
    }

    pub fn alloc_struct(&'a self, name: &str) -> &'a InferTy<'a> {
        self.alloc(TyKind::Struct(name.to_string()))
    }

    pub fn alloc_any(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Any)
    }

    pub fn alloc_ptr(&'a self, inner: &'a InferTy<'a>) -> &'a InferTy<'a> {
        self.alloc(TyKind::Ptr(inner))
    }

    pub fn alloc_fun(
        &'a self,
        args: Vec<&'a InferTy<'a>>,
        ret: &'a InferTy<'a>,
    ) -> &'a InferTy<'a> {
        self.alloc(TyKind::Fun(FunTy { args, ret }))
    }
}
