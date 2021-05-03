use error::TypeCkError;
use hir::ty;
use std::cell::{Cell, Ref, RefCell};
use typed_arena::Arena;

pub type InferTyID = usize;

/// `InferTy` represents a temporary type during inferencing
/// actual type by `TyAnalyzer`. This is also a node of type tree
/// which lives under `InferTyArena`.
///
/// Each connected node has a reference from and to next node.
/// Connection means that those nodes will be same type after
/// resolving type inference, so all nodes of a tree must have
/// same type.
#[derive(Debug)]
pub struct InferTy<'a> {
    pub id: InferTyID,
    pub kind: InferTyKind<'a>,
    to_node: Cell<Option<&'a InferTy<'a>>>,
    from_node: RefCell<Vec<&'a InferTy<'a>>>,
}

impl<'a> InferTy<'a> {
    pub fn new(id: InferTyID, kind: InferTyKind<'a>) -> Self {
        Self {
            id,
            kind,
            to_node: Cell::new(None),
            from_node: RefCell::new(Vec::new()),
        }
    }

    /// Set the most tip node
    pub fn set_prune(&'a self, tip: &'a InferTy<'a>) {
        let t = self.prune();
        if t.id == tip.id {
            // prevent self reference
            return;
        }

        match (&t.kind, &tip.kind) {
            (InferTyKind::Fun(t_fun), InferTyKind::Fun(tip_fun))
                if t_fun.arg_tys.len() == tip_fun.arg_tys.len() =>
            {
                for i in 0..t_fun.arg_tys.len() {
                    t_fun.arg_tys[i].set_prune(tip_fun.arg_tys[i]);
                }
                t_fun.ret_ty.set_prune(tip_fun.ret_ty);
            }
            _ => {}
        }

        tip.from_node.borrow_mut().push(t);
        t.to_node.set(Some(tip));
    }

    /// Get the most tip node from this node.
    pub fn prune(&'a self) -> &'a InferTy<'a> {
        match self.to_node.get() {
            Some(to_node) => to_node.prune(),
            None => self,
        }
    }

    pub fn borrow_from_nodes(&self) -> Ref<'_, Vec<&'a InferTy<'a>>> {
        self.from_node.borrow()
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
}

#[derive(Debug)]
pub struct FunTy<'a> {
    pub arg_tys: Vec<&'a InferTy<'a>>,
    pub ret_ty: &'a InferTy<'a>,
}

impl<'a> InferTyKind<'a> {
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
            // FIXME
            Self::Fun(_) => "fun".to_string(),
        }
    }
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

#[derive(Default)]
pub struct InferTyArena<'a> {
    pub inner: Arena<InferTy<'a>>,
}

impl<'a> InferTyArena<'a> {
    pub fn alloc(&self, kind: InferTyKind<'a>) -> &InferTy<'a> {
        self.inner.alloc(InferTy::new(self.inner.len(), kind))
    }

    pub fn alloc_var(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Var)
    }

    pub fn alloc_i8(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Int(IntKind::I8))
    }

    pub fn alloc_i16(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Int(IntKind::I16))
    }

    pub fn alloc_i32(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Int(IntKind::I32))
    }

    pub fn alloc_i64(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Int(IntKind::I64))
    }

    pub fn alloc_int_lit(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::IntLit)
    }

    pub fn alloc_f32(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Float(FloatKind::F32))
    }

    pub fn alloc_f64(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Float(FloatKind::F64))
    }

    pub fn alloc_float_lit(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::FloatLit)
    }

    pub fn alloc_bool(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Bool)
    }

    pub fn alloc_void(&self) -> &InferTy<'a> {
        self.alloc(InferTyKind::Void)
    }

    pub fn alloc_fun(
        &self,
        arg_tys: Vec<&'a InferTy<'a>>,
        ret_ty: &'a InferTy<'a>,
    ) -> &InferTy<'a> {
        self.alloc(InferTyKind::Fun(FunTy { arg_tys, ret_ty }))
    }
}
