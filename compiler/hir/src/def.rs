use serde::Serialize;

pub type DefId = usize;

/// User defined symbols. This will be created when
/// user defines functions or variables which has a
/// unique name in the scope.
///
/// Generic type <Ty> will be `infer_ty::InferTy` or
/// `ty::Type` because this will be used by either
/// type inference and type checking.
#[derive(Debug, Clone, Serialize)]
pub struct Def<Ty> {
    pub id: DefId,
    pub ty: Ty,
    pub kind: DefKind,
}

#[derive(Debug, Clone, Serialize)]
pub enum DefKind {
    Var { is_mut: bool },
    // array of arguments mutability
    Fn(Vec<bool>, bool),
}

impl<Ty> Def<Ty> {
    pub fn new(id: DefId, ty: Ty, kind: DefKind) -> Self {
        Self { id, ty, kind }
    }

    pub fn is_var(&self) -> bool {
        matches!(self.kind, DefKind::Var { is_mut: _ })
    }

    pub fn is_mutable(&self) -> bool {
        matches!(self.kind, DefKind::Var { is_mut: true })
    }

    pub fn as_fn(&self) -> (&[bool], bool) {
        match &self.kind {
            DefKind::Fn(arg_muts, ret_mut) => (arg_muts, *ret_mut),
            _ => panic!(),
        }
    }
}
