#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    Void,
    Int(IntTy),
    Float(FloatTy),
    Bool,
    String,
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
