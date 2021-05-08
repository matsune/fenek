use std::string::ToString;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Int(IntType),
    Float(FloatType),
    Bool,
    String,
    Fun(FunType),
    Ref(Box<Type>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunType {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
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

    pub fn is_ref(&self) -> bool {
        matches!(self, Type::Ref(_))
    }

    pub fn is_var(&self) -> bool {
        !self.is_fun()
    }

    pub fn into_fun(self) -> FunType {
        match self {
            Self::Fun(f) => f,
            _ => panic!(),
        }
    }

    pub fn as_fun(&self) -> &FunType {
        match self {
            Self::Fun(f) => f,
            _ => panic!(),
        }
    }
}

impl FunType {
    pub fn new(args: Vec<Type>, ret: Box<Type>) -> Self {
        Self { args, ret }
    }
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
            Self::Ref(ty) => format!("&{}", ty.to_string()),
        }
    }
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
            self.ret.to_string()
        )
    }
}

impl ToString for IntType {
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

impl ToString for FloatType {
    fn to_string(&self) -> String {
        match self {
            Self::F32 => "f32",
            Self::F64 => "f64",
        }
        .to_string()
    }
}
