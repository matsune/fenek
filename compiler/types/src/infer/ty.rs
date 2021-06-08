use crate::ty::Type;
use std::cell::Cell;

pub type InferTyID = usize;
pub type AtomTyID = usize;

pub struct InferTy<'a> {
    pub id: InferTyID,
    pub kind: TyKind<'a>,
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
    pub fn new(id: InferTyID, kind: TyKind<'a>) -> Self {
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

    pub fn set_prune(&'a self, next: &'a InferTy<'a>) {
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
        pruned.next.set(Some(next));
    }
}

#[derive(Debug)]
pub enum TyKind<'a> {
    Any,
    Atom(AtomTy),
    Ptr(&'a InferTy<'a>),
    Fun(FunTy<'a>),
}

#[derive(Debug)]
pub struct AtomTy {
    pub id: AtomTyID,
    pub name: String,
    pub family: Vec<AtomTyID>,
    pub solve_ty: Type,
}

impl AtomTy {
    pub fn solve_ty(&self) -> Type {
        self.solve_ty.clone()
    }
}

#[derive(Debug)]
pub struct FunTy<'a> {
    pub args: Vec<&'a InferTy<'a>>,
    pub ret: &'a InferTy<'a>,
}

impl<'a> ToString for TyKind<'a> {
    fn to_string(&self) -> String {
        match self {
            Self::Any => "Any".to_string(),
            Self::Atom(atom) => atom.name.to_string(),
            Self::Ptr(ptr) => format!("*{}", ptr.kind.to_string()),
            Self::Fun(fun) => format!(
                "{} -> {}",
                fun.args
                    .iter()
                    .map(|ty| ty.kind.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                fun.ret.kind.to_string()
            ),
        }
    }
}
