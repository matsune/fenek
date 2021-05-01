use crate::infer_ty::InferTy;
use hir::def::Def;
use hir::ty;
use std::collections::HashMap;
use std::rc::Rc;

pub type ArenaIdx = usize;

#[derive(Default)]
pub struct ScopeArena<T> {
    pub inner: Vec<ScopeTable<T>>,
}

impl<T> ScopeArena<T> {
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
    pub table: HashMap<String, Rc<Def<T>>>,
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

    pub fn lookup_mut(&mut self, symbol: &str) -> Option<&mut Rc<Def<T>>> {
        self.table.get_mut(symbol)
    }

    pub fn insert(&mut self, key: String, value: Def<T>) {
        self.table.insert(key, Rc::new(value));
    }

    pub fn insert_var(&mut self, symbol: String, def: Def<T>) -> Rc<Def<T>> {
        let rc = Rc::new(def);
        self.table.insert(symbol, rc.clone());
        rc
    }

    pub fn insert_fun(&mut self, symbol: String, def: Def<T>) -> Rc<Def<T>> {
        let rc = Rc::new(def);
        self.table.insert(symbol, rc.clone());
        rc
    }
}

impl ScopeTable<ty::Type> {
    pub fn lookup_fun(&self, symbol: &str) -> Option<Rc<Def<ty::Type>>> {
        match self.table.get(symbol) {
            Some(cell) => {
                let def: &Def<_> = cell;
                if def.ty.is_fun() {
                    Some(cell.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn lookup_var(&self, symbol: &str) -> Option<Rc<Def<ty::Type>>> {
        match self.table.get(symbol) {
            Some(cell) => {
                let def: &Def<_> = cell;
                if !def.ty.is_fun() {
                    Some(cell.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl<'a> ScopeTable<&'a InferTy<'a>> {
    pub fn lookup_fun(&self, symbol: &str) -> Option<Rc<Def<&'a InferTy<'a>>>> {
        match self.table.get(symbol) {
            Some(cell) => {
                let def: &Def<_> = cell;
                if def.ty.kind.is_fun() {
                    Some(cell.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn lookup_var(&self, symbol: &str) -> Option<Rc<Def<&'a InferTy<'a>>>> {
        match self.table.get(symbol) {
            Some(cell) => {
                let def: &Def<_> = cell;
                if !def.ty.kind.is_fun() {
                    Some(cell.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
