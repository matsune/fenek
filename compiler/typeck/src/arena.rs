use super::infer::*;
use hir::def::Def;
use typed_arena::Arena;

pub struct DefArena<Ty> {
    pub inner: Arena<Def<Ty>>,
}

impl<Ty> DefArena<Ty> {
    pub fn new() -> Self {
        Self {
            inner: Arena::new(),
        }
    }

    pub fn alloc(&self, ty: Ty, is_mut: bool) -> &Def<Ty> {
        self.inner.alloc(Def::new(self.inner.len(), ty, is_mut))
    }
}

#[derive(Default)]
pub struct InferTyArena<'a> {
    pub inner: Arena<InferTy<'a>>,
}

impl<'a> InferTyArena<'a> {
    pub fn alloc(&self, kind: InferTyKind<'a>) -> &InferTy<'a> {
        self.inner.alloc(InferTy::new(self.inner.len(), kind))
    }

    pub fn alloc_var(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Var)
    }

    pub fn alloc_i8(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Int(IntKind::I8))
    }

    pub fn alloc_i16(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Int(IntKind::I16))
    }

    pub fn alloc_i32(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Int(IntKind::I32))
    }

    pub fn alloc_i64(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Int(IntKind::I64))
    }

    pub fn alloc_int_lit(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::IntLit)
    }

    pub fn alloc_f32(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Float(FloatKind::F32))
    }

    pub fn alloc_f64(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Float(FloatKind::F64))
    }

    pub fn alloc_float_lit(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::FloatLit)
    }

    pub fn alloc_bool(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Bool)
    }

    pub fn alloc_void(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Void)
    }

    pub fn alloc_fun(
        &self,
        arg_tys: Vec<&'a InferTy<'a>>,
        ret_ty: &'a InferTy<'a>,
    ) -> &InferTy<'a> {
        self.alloc(InferTyKind::Fun(FunTy { arg_tys, ret_ty }))
    }

    pub fn alloc_ptr(&self, ty: &'a InferTy<'a>) -> &InferTy<'a> {
        self.alloc(InferTyKind::Ptr(&ty.kind))
    }
}
