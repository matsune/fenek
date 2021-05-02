use crate::infer_ty::InferTy;
use hir::def::Def;
use hir::ty;
use std::collections::HashMap;

pub type ArenaIdx = usize;

#[derive(Default)]
pub struct ScopeArena<'a, T> {
    pub inner: Vec<ScopeTable<'a, T>>,
}

impl<'a, T> ScopeArena<'a, T> {
    pub fn add_node(&mut self, parent: Option<ArenaIdx>) -> ArenaIdx {
        let idx = self.inner.len();
        self.inner.push(ScopeTable::new(idx, parent));
        idx
    }

    pub fn get(&self, idx: ArenaIdx) -> &ScopeTable<'a, T> {
        &self.inner[idx]
    }

    pub fn get_mut(&mut self, idx: ArenaIdx) -> &mut ScopeTable<'a, T> {
        &mut self.inner[idx]
    }
}

impl<'a, T> std::convert::From<Vec<ScopeTable<'a, T>>> for ScopeArena<'a, T> {
    fn from(inner: Vec<ScopeTable<'a, T>>) -> Self {
        Self { inner }
    }
}

#[derive(Debug)]
pub struct ScopeTable<'a, T> {
    pub idx: ArenaIdx,
    pub table: HashMap<String, &'a Def<T>>,
    pub parent: Option<ArenaIdx>,
}

impl<'a, T> ScopeTable<'a, T> {
    pub fn new(idx: ArenaIdx, parent: Option<ArenaIdx>) -> Self {
        Self {
            idx,
            table: HashMap::new(),
            parent,
        }
    }

    pub fn insert(&mut self, key: String, value: &'a Def<T>) {
        self.table.insert(key, value);
    }
}

impl<'a> ScopeTable<'a, ty::Type> {
    pub fn lookup_fun(&self, symbol: &str) -> Option<&'a Def<ty::Type>> {
        match self.table.get(symbol) {
            Some(def) => {
                if def.ty.is_fun() {
                    Some(def)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn lookup_var(&self, symbol: &str) -> Option<&'a Def<ty::Type>> {
        match self.table.get(symbol) {
            Some(def) => {
                if !def.ty.is_fun() {
                    Some(def)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl<'infer> ScopeTable<'infer, &'infer InferTy<'infer>> {
    pub fn lookup_fun(&self, symbol: &str) -> Option<&'infer Def<&'infer InferTy<'infer>>> {
        match self.table.get(symbol) {
            Some(def) => {
                if def.ty.kind.is_fun() {
                    Some(def)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn lookup_var(&self, symbol: &str) -> Option<&'infer Def<&'infer InferTy<'infer>>> {
        match self.table.get(symbol) {
            Some(def) => {
                if !def.ty.kind.is_fun() {
                    Some(def)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
