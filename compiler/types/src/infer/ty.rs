use std::cell::Cell;

pub type InferTyID = usize;

/// `InferTy` represents a temporary type before resolving
/// actual type by solver. Every instances will be free when
/// `InferTyArena` drops.
///
/// Each instance is a connected node  which has a reference
/// to a next node. Connection means that those nodes will be
/// the same type after resolving type inference, so all nodes
/// of the tree must have the same type.
pub struct InferTy<'a> {
    pub id: InferTyID,
    pub kind: InferTyKind<'a>,
    next: Cell<Option<&'a InferTy<'a>>>,
}

impl<'a> std::fmt::Debug for InferTy<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InferTy")
            .field("id", &self.id)
            .field("kind", &self.kind)
            .field("next", &self.next)
            .finish()
    }
}

impl<'a> InferTy<'a> {
    pub fn new(id: InferTyID, kind: InferTyKind<'a>) -> Self {
        Self {
            id,
            kind,
            next: Cell::new(None),
        }
    }

    /// get the most tip node from this node.
    pub fn prune(&'a self) -> &'a InferTy<'a> {
        self.next
            .get()
            .map(|to_node| to_node.prune())
            .unwrap_or(self)
    }

    /// returns dereferenced type if self is Ref
    pub fn elem_ty(&'a self) -> Option<&'a InferTy<'a>> {
        match self.kind {
            InferTyKind::Ref(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn set_next(&'a self, next: &'a InferTy<'a>) {
        let mut pruned = self;
        loop {
            // prevent to add duplicated node
            if pruned.id == next.id {
                return;
            }
            match pruned.next.get() {
                Some(n) => pruned = n,
                None => break,
            }
        }
        self.next.set(Some(next));
    }
}

#[derive(Debug)]
pub enum InferTyKind<'a> {
    Var,
    Int(IntKind),
    IntLit,
    Float(FloatKind),
    FloatLit,
    Bool,
    Void,
    Fun(FunTy<'a>),
    Ref(&'a InferTy<'a>),
    Deref(&'a InferTy<'a>),
    Struct(String),
}

impl<'a> PartialEq for InferTyKind<'a> {
    fn eq(&self, other: &Self) -> bool {
        use InferTyKind::*;
        match (self, other) {
            (Var, Var) | (IntLit, IntLit) | (FloatLit, FloatLit) | (Bool, Bool) | (Void, Void) => {
                true
            }
            (Int(il), Int(ir)) => il == ir,
            (Float(il), Float(ir)) => il == ir,
            (Fun(fl), Fun(fr)) => {
                let fl_arg_tys: Vec<&InferTyKind<'a>> =
                    fl.arg_tys.iter().map(|ty| &ty.kind).collect();
                let fr_arg_tys: Vec<&InferTyKind<'a>> =
                    fr.arg_tys.iter().map(|ty| &ty.kind).collect();
                fl_arg_tys == fr_arg_tys && fl.ret_ty.kind == fr.ret_ty.kind
            }
            (Ref(l), Ref(r)) => l.kind == r.kind,
            (Deref(l), Deref(r)) => l.kind == r.kind,
            (Struct(l), Struct(r)) => l == r,
            _ => false,
        }
    }
}

impl<'a> InferTyKind<'a> {
    pub fn as_fun(&self) -> &FunTy<'a> {
        match self {
            Self::Fun(ty) => ty,
            _ => panic!(),
        }
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, Self::Fun(_))
    }

    pub fn is_var(&self) -> bool {
        !self.is_fun()
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Self::Ref(_))
    }

    // types that can store in variable
    pub fn is_variable(&self) -> bool {
        !matches!(self, Self::Void)
    }
}

impl<'a> ToString for InferTyKind<'a> {
    fn to_string(&self) -> String {
        match self {
            Self::Var => "var".to_string(),
            Self::Int(i) => i.to_string(),
            Self::IntLit => "int lit".to_string(),
            Self::Float(f) => f.to_string(),
            Self::FloatLit => "float lit".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Void => "void".to_string(),
            Self::Fun(fun) => format!(
                "({}) -> {}",
                fun.arg_tys
                    .iter()
                    .map(|ty| ty.kind.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                fun.ret_ty.kind.to_string()
            ),
            Self::Ref(k) => format!("&{}", k.kind.to_string()),
            Self::Deref(k) => format!("*{}", k.kind.to_string()),
            Self::Struct(k) => k.clone(),
        }
    }
}

#[derive(Debug)]
pub struct FunTy<'a> {
    pub arg_tys: Vec<&'a InferTy<'a>>,
    pub ret_ty: &'a InferTy<'a>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
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
