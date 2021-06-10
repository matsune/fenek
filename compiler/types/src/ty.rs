pub type StructID = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int(IntType),
    Float(FloatType),
    Bool,
    // String,
    Ptr(Box<Type>),
    Void,
    Fun(FunType),
    Struct(StructType),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(inner) => inner.fmt(f),
            Self::Float(inner) => inner.fmt(f),
            Self::Bool => write!(f, "bool"),
            Self::Ptr(inner) => write!(f, "*{}", inner),
            Self::Void => write!(f, "void"),
            Self::Fun(inner) => inner.fmt(f),
            Self::Struct(inner) => inner.fmt(f),
        }
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

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
}

impl std::fmt::Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatType {
    F32,
    F64,
}

impl std::fmt::Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct MutType {
    pub is_mut: bool,
    pub ty: Type,
}

impl std::fmt::Display for MutType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", if self.is_mut { "mut" } else { "" }, self.ty)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunType {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl std::fmt::Display for FunType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> {}",
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", "),
            self.ret
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub id: StructID,
    pub name: String,
    pub members: Vec<StructMember>,
}

impl std::fmt::Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "struct {} {{{}}}",
            self.name,
            self.members
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructMember {
    is_mut: bool,
    name: String,
    ty: Type,
}

impl std::fmt::Display for StructMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}: {}",
            if self.is_mut { "mut" } else { "" },
            self.name,
            self.ty
        )
    }
}

macro_rules! impl_Serialize_for_ToString {
    ($($name:ident),*) => {
        $(
            impl serde::Serialize for $name {
                fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    serializer.serialize_str(&self.to_string())
                }
            }
        )*
    }
}

impl_Serialize_for_ToString!(Type, IntType, FloatType, FunType, StructType, StructMember);
