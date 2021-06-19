use super::ty::*;
use std::cell::Cell;
use std::string::ToString;
use typed_arena::Arena;

#[derive(Default)]
pub struct InferTyArena<'a> {
    pub inner: Arena<InferTy<'a>>,
}

impl<'a> InferTyArena<'a> {
    pub fn alloc_any(&'a self) -> &'a InferTy<'a> {
        let id = self.inner.len();
        self.inner.alloc(InferTy::Any(BindableTy::new(id)))
    }

    pub fn alloc_int_lit(&'a self) -> &'a InferTy<'a> {
        let id = self.inner.len();
        self.inner.alloc(InferTy::IntLit(BindableTy::new(id)))
    }

    pub fn alloc_float_lit(&'a self) -> &'a InferTy<'a> {
        let id = self.inner.len();
        self.inner.alloc(InferTy::FloatLit(BindableTy::new(id)))
    }

    pub fn alloc_ptr(&'a self, elem: &'a InferTy<'a>) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Ptr(elem))
    }

    pub fn alloc_fun(
        &'a self,
        args: Vec<&'a InferTy<'a>>,
        ret: &'a InferTy<'a>,
    ) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Fun(FunTy { args, ret }))
    }

    pub fn alloc_i8(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Int(IntType::I8))
    }

    pub fn alloc_i16(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Int(IntType::I16))
    }

    pub fn alloc_i32(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Int(IntType::I32))
    }

    pub fn alloc_i64(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Int(IntType::I64))
    }

    pub fn alloc_f32(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Float(FloatType::F32))
    }

    pub fn alloc_f64(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Float(FloatType::F64))
    }

    pub fn alloc_bool(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Bool)
    }

    pub fn alloc_void(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Void)
    }

    pub fn alloc_struct(&'a self, strukt: StructType) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Struct(strukt))
    }
}

pub type BindableTyID = usize;

#[derive(Debug, PartialEq)]
pub enum InferTy<'a> {
    Any(BindableTy<'a>),
    IntLit(BindableTy<'a>),
    FloatLit(BindableTy<'a>),
    Ptr(&'a InferTy<'a>),
    Fun(FunTy<'a>),
    Int(IntType),
    Float(FloatType),
    Bool,
    Void,
    Struct(StructType),
}

impl<'a> std::fmt::Display for InferTy<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any(_) => write!(f, "any"),
            Self::IntLit(_) => write!(f, "int_lit"),
            Self::FloatLit(_) => write!(f, "float_lit"),
            Self::Ptr(elem) => write!(f, "*{}", elem),
            Self::Fun(inner) => inner.fmt(f),
            Self::Int(inner) => inner.fmt(f),
            Self::Float(inner) => inner.fmt(f),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Struct(inner) => inner.fmt(f),
        }
    }
}

impl<'a> InferTy<'a> {
    pub fn try_as_bindable(&self) -> Option<&BindableTy<'a>> {
        match self {
            Self::Any(inner) | Self::IntLit(inner) | Self::FloatLit(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn as_fun(&self) -> &FunTy<'a> {
        match self {
            Self::Fun(fun) => fun,
            _ => panic!(),
        }
    }
}

#[derive(Debug)]
pub struct BindableTy<'a> {
    pub id: BindableTyID,
    pub next: Cell<Option<&'a InferTy<'a>>>,
}

impl<'a> PartialEq for BindableTy<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'a> BindableTy<'a> {
    fn new(id: BindableTyID) -> Self {
        Self {
            id,
            next: Cell::new(None),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunTy<'a> {
    pub args: Vec<&'a InferTy<'a>>,
    pub ret: &'a InferTy<'a>,
}

impl<'a> std::fmt::Display for FunTy<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            self.ret
        )
    }
}
