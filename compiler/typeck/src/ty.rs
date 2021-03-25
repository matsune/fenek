#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    Void,
    Int(IntTy),
    Float(FloatTy),
    Bool,
    String,
}

impl Type {
    pub fn into_int_ty(self) -> IntTy {
        match self {
            Self::Int(v) => v,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatTy {
    F32,
    F64,
}
