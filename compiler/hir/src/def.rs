pub type DefId = usize;

/// User defined symbols. This will be created when
/// user defines functions or variables which has a
/// unique name in the scope.
///
/// Generic type <Ty> will be `infer_ty::InferTy` or
/// `ty::Type` because this will be used by either
/// type inference and type checking.
#[derive(Debug, Clone)]
pub enum Def<Ty> {
    Fun(FunDef<Ty>),
    Var(VarDef<Ty>),
}

impl<Ty> Def<Ty> {
    pub fn into_fun_def(self) -> FunDef<Ty> {
        match self {
            Def::Fun(def) => def,
            _ => panic!(),
        }
    }

    pub fn into_var_def(self) -> VarDef<Ty> {
        match self {
            Def::Var(var_def) => var_def,
            _ => panic!(),
        }
    }

    pub fn as_var_def(&self) -> &VarDef<Ty> {
        match self {
            Def::Var(ref var_def) => var_def,
            _ => panic!(),
        }
    }

    pub fn id(&self) -> DefId {
        match self {
            Def::Fun(fun_def) => fun_def.id,
            Def::Var(var_def) => var_def.id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunDef<Ty> {
    pub id: DefId,
    pub ret_ty: Ty,
    pub arg_tys: Vec<Ty>,
}

impl<Ty> FunDef<Ty> {
    pub fn new(id: DefId, ret_ty: Ty, arg_tys: Vec<Ty>) -> Self {
        Self {
            id,
            ret_ty,
            arg_tys,
        }
    }
}

impl<Ty> Into<Def<Ty>> for FunDef<Ty> {
    fn into(self) -> Def<Ty> {
        Def::Fun(self)
    }
}

#[derive(Debug, Clone)]
pub struct VarDef<Ty> {
    pub id: DefId,
    pub ty: Ty,
    pub is_mut: bool,
}

impl<Ty> VarDef<Ty> {
    pub fn new(id: DefId, ty: Ty, is_mut: bool) -> Self {
        Self { id, ty, is_mut }
    }
}

impl<Ty> Into<Def<Ty>> for VarDef<Ty> {
    fn into(self) -> Def<Ty> {
        Def::Var(self)
    }
}
