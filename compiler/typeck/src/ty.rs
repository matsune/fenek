#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Int(IntKind),
    Float(FloatKind),
    Bool,
    String,
    Fun(FunType),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunType {
    args: Vec<Type>,
    ret: Option<Box<Type>>,
}

impl Type {
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, Type::Fun(_))
    }

    pub fn into_int(self) -> IntKind {
        match self {
            Self::Int(v) => v,
            _ => panic!(),
        }
    }

    pub fn into_float(self) -> FloatKind {
        match self {
            Self::Float(v) => v,
            _ => panic!(),
        }
    }

    pub fn as_fun(&self) -> &FunType {
        match self {
            Self::Fun(f) => f,
            _ => panic!(),
        }
    }

    pub fn into_fun(self) -> FunType {
        match self {
            Self::Fun(f) => f,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IntKind {
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}
