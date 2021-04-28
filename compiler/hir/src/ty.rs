use std::string::ToString;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Int(IntKind),
    Float(FloatKind),
    Bool,
    String,
    Fun(FunType),
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Self::Void => "void".to_string(),
            Self::Int(k) => k.to_string(),
            Self::Float(k) => k.to_string(),
            Self::Bool => "bool".to_string(),
            Self::String => "string".to_string(),
            Self::Fun(fun) => fun.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunType {
    pub args: Vec<Type>,
    pub ret: Option<Box<Type>>,
}

impl ToString for FunType {
    fn to_string(&self) -> String {
        format!(
            "({}) -> {}",
            self.args
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.ret
                .as_ref()
                .map(|b| b.to_string())
                .unwrap_or_else(|| Type::Void.to_string())
        )
    }
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

impl ToString for IntKind {
    fn to_string(&self) -> String {
        match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
        }
        .to_string()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

impl ToString for FloatKind {
    fn to_string(&self) -> String {
        match self {
            Self::F32 => "f32",
            Self::F64 => "f64",
        }
        .to_string()
    }
}
