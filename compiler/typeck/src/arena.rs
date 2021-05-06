use hir::def::Def;
use typed_arena::Arena;

pub struct DefArena<Ty> {
    pub inner: Arena<Def<Ty>>,
}

impl<Ty> DefArena<Ty> {
    pub fn new() -> Self {
        Self {
            inner: Arena::new(),
        }
    }

    pub fn alloc(&self, ty: Ty, is_mut: bool) -> &Def<Ty> {
        self.inner.alloc(Def::new(self.inner.len(), ty, is_mut))
    }
}
