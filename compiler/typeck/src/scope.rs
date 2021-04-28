use hir::def::{Def, FunDef, VarDef};
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
