use std::cell::{Cell, Ref, RefCell};
use std::fmt;
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
pub struct InferTy<'a> {
    pub id: InferTyID,
    pub kind: InferTyKind,
    to_node: Cell<Option<&'a InferTy<'a>>>,
    from_node: RefCell<Vec<&'a InferTy<'a>>>,
}

impl<'a> fmt::Debug for InferTy<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GraphNode")
            .field("id", &self.id)
            .field("kind", &self.kind)
            .field("to_node", &self.to_node)
            .field("from_node count", &self.from_node.borrow().len())
            .finish()
    }
}

impl<'a> InferTy<'a> {
    pub fn new(id: InferTyID, kind: InferTyKind) -> Self {
        Self {
            id,
            kind,
            to_node: Cell::new(None),
            from_node: RefCell::new(Vec::new()),
        }
    }

    pub fn is_void(&self) -> bool {
        self.kind == InferTyKind::Void
    }

    /// Set the most tip node
    pub fn set_prune(&'a self, tip: &'a InferTy<'a>) {
        let t = self.prune();
        if t.id == tip.id {
            // prevent self reference
            return;
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InferTyKind {
    Var,
    Int(IntKind),
    IntLit,
    Float(FloatKind),
    FloatLit,
    Bool,
    Void,
}

impl ToString for InferTyKind {
    fn to_string(&self) -> String {
        match self {
            Self::Var => "var".to_string(),
            Self::Int(i) => i.to_string(),
            Self::IntLit => "int lit".to_string(),
            Self::Float(f) => f.to_string(),
            Self::FloatLit => "float lit".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Void => "void".to_string(),
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
    pub fn alloc(&self, kind: InferTyKind) -> &InferTy<'a> {
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
}
