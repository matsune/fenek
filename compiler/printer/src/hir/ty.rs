use serde::{Serialize, Serializer};
use std::ops::Deref;

macro_rules! impl_serializer_for_ToString {
    ($name:ident) => {
        impl Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                serializer.serialize_str(&self.to_string())
            }
        }
    };
}

impl_serializer_for_ToString!(Type);
impl_serializer_for_ToString!(FunType);
impl_serializer_for_ToString!(IntKind);
impl_serializer_for_ToString!(FloatKind);

pub enum Type {
    Void,
    Int(IntKind),
    Float(FloatKind),
    Bool,
    String,
    Fun(FunType),
    Ref(Box<Type>),
}

impl From<&types::ty::Type> for Type {
    fn from(ty: &types::ty::Type) -> Self {
        match ty {
            types::ty::Type::Void => Self::Void,
            types::ty::Type::Int(kind) => Self::Int(kind.into()),
            types::ty::Type::Float(kind) => Self::Float(kind.into()),
            types::ty::Type::Bool => Self::Bool,
            types::ty::Type::String => Self::String,
            types::ty::Type::Fun(fun) => Self::Fun(fun.into()),
            types::ty::Type::Ref(ty) => {
                let ty: &types::ty::Type = &ty;
                Self::Ref(Box::new(ty.into()))
            }
        }
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

pub struct FunType {
    args: Vec<Type>,
    ret: Box<Type>,
}

impl From<&types::ty::FunType> for FunType {
    fn from(fun: &types::ty::FunType) -> Self {
        Self {
            args: fun.args.iter().map(|arg| arg.into()).collect(),
            ret: Box::new(fun.ret.deref().into()),
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

pub enum IntKind {
    I8,
    I16,
    I32,
    I64,
}

impl From<&types::ty::IntType> for IntKind {
    fn from(kind: &types::ty::IntType) -> Self {
        match kind {
            types::ty::IntType::I8 => Self::I8,
            types::ty::IntType::I16 => Self::I16,
            types::ty::IntType::I32 => Self::I32,
            types::ty::IntType::I64 => Self::I64,
        }
    }
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

pub enum FloatKind {
    F32,
    F64,
}

impl From<&types::ty::FloatType> for FloatKind {
    fn from(kind: &types::ty::FloatType) -> Self {
        match kind {
            types::ty::FloatType::F32 => Self::F32,
            types::ty::FloatType::F64 => Self::F64,
        }
    }
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
