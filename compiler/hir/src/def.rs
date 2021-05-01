pub type DefId = usize;

/// User defined symbols. This will be created when
/// user defines functions or variables which has a
/// unique name in the scope.
///
/// Generic type <Ty> will be `infer_ty::InferTy` or
/// `ty::Type` because this will be used by either
/// type inference and type checking.
#[derive(Debug, Clone)]
pub struct Def<Ty> {
    pub id: DefId,
    pub ty: Ty,
    pub is_mut: bool,
}

impl<Ty> Def<Ty> {
    pub fn new(id: DefId, ty: Ty, is_mut: bool) -> Self {
        Self { id, ty, is_mut }
    }
}
