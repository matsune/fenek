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
}

impl From<&hir::ty::Type> for Type {
    fn from(ty: &hir::ty::Type) -> Self {
        match ty {
            hir::ty::Type::Void => Self::Void,
            hir::ty::Type::Int(kind) => Self::Int(kind.into()),
            hir::ty::Type::Float(kind) => Self::Float(kind.into()),
            hir::ty::Type::Bool => Self::Bool,
            hir::ty::Type::String => Self::String,
            hir::ty::Type::Fun(fun) => Self::Fun(fun.into()),
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
        }
    }
}

pub struct FunType {
    args: Vec<Type>,
    ret: Box<Type>,
}

impl From<&hir::ty::FunType> for FunType {
    fn from(fun: &hir::ty::FunType) -> Self {
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

impl From<&hir::ty::IntKind> for IntKind {
    fn from(kind: &hir::ty::IntKind) -> Self {
        match kind {
            hir::ty::IntKind::I8 => Self::I8,
            hir::ty::IntKind::I16 => Self::I16,
            hir::ty::IntKind::I32 => Self::I32,
            hir::ty::IntKind::I64 => Self::I64,
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

impl From<&hir::ty::FloatKind> for FloatKind {
    fn from(kind: &hir::ty::FloatKind) -> Self {
        match kind {
            hir::ty::FloatKind::F32 => Self::F32,
            hir::ty::FloatKind::F64 => Self::F64,
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
