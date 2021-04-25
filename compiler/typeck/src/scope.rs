use parse::ast;
use std::collections::HashMap;

pub type ArenaIdx = usize;

pub struct ScopeArena<T> {
    pub inner: Vec<ScopeTable<T>>,
}

impl<T> ScopeArena<T> {
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn add_node(&mut self, parent: Option<ArenaIdx>) -> ArenaIdx {
        let idx = self.inner.len();
        self.inner.push(ScopeTable::new(idx, parent));
        idx
    }

    pub fn get(&self, idx: ArenaIdx) -> &ScopeTable<T> {
        &self.inner[idx]
    }

    pub fn get_mut(&mut self, idx: ArenaIdx) -> &mut ScopeTable<T> {
        &mut self.inner[idx]
    }
}

impl<T> std::convert::From<Vec<ScopeTable<T>>> for ScopeArena<T> {
    fn from(inner: Vec<ScopeTable<T>>) -> Self {
        Self { inner }
    }
}

#[derive(Debug)]
pub struct ScopeTable<T> {
    pub idx: ArenaIdx,
    pub table: HashMap<String, Def<T>>,
    pub parent: Option<ArenaIdx>,
}

impl<T> ScopeTable<T> {
    pub fn new(idx: ArenaIdx, parent: Option<ArenaIdx>) -> Self {
        Self {
            idx,
            table: HashMap::new(),
            parent,
        }
    }

    pub fn lookup(&self, symbol: &str) -> Option<&Def<T>> {
        self.table.get(symbol)
    }

    pub fn lookup_mut(&mut self, symbol: &str) -> Option<&mut Def<T>> {
        self.table.get_mut(symbol)
    }

    pub fn insert(&mut self, key: String, value: Def<T>) {
        self.table.insert(key, value);
    }

    pub fn insert_var(&mut self, symbol: String, def: VarDef<T>) {
        self.table.insert(symbol, Def::Var(def));
    }

    pub fn insert_fun(&mut self, symbol: String, def: FunDef<T>) {
        self.table.insert(symbol, Def::Fun(def));
    }
}

pub type DefId = usize;

/// User defined symbols. This will be created when
/// user defines functions or variables which has a
/// unique name in the scope.
///
/// Generic type <T> will be `infer_ty::InferTy` or
/// `ty::Type` because this will be used by either
/// type inference and type checking.
#[derive(Debug, Clone)]
pub enum Def<T> {
    Fun(FunDef<T>),
    Var(VarDef<T>),
}

impl<T> Def<T> {
    pub fn into_fun_def(self) -> FunDef<T> {
        match self {
            Def::Fun(def) => def,
            _ => panic!(),
        }
    }

    pub fn into_var_def(self) -> VarDef<T> {
        match self {
            Def::Var(var_def) => var_def,
            _ => panic!(),
        }
    }

    pub fn as_var_def(&self) -> &VarDef<T> {
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
pub struct FunDef<T> {
    pub id: DefId,
    pub ret_ty: T,
    pub arg_tys: Vec<T>,
}

impl<T> FunDef<T> {
    pub fn new(id: DefId, ret_ty: T, arg_tys: Vec<T>) -> Self {
        Self {
            id,
            ret_ty,
            arg_tys,
        }
    }
}

impl<T> Into<Def<T>> for FunDef<T> {
    fn into(self) -> Def<T> {
        Def::Fun(self)
    }
}

#[derive(Debug, Clone)]
pub struct VarDef<T> {
    pub id: DefId,
    pub ty: T,
    pub is_mut: bool,
}

impl<T> VarDef<T> {
    pub fn new(id: DefId, ty: T, is_mut: bool) -> Self {
        Self { id, ty, is_mut }
    }
}

impl<T> Into<Def<T>> for VarDef<T> {
    fn into(self) -> Def<T> {
        Def::Var(self)
    }
}
