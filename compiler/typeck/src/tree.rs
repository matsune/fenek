pub type TreeNodeIdx = usize;

#[derive(Debug, Default)]
pub struct ArenaTree<T> {
    arena: Vec<TreeNode<T>>,
}

impl<T> ArenaTree<T> {
    pub fn add_node(&mut self, val: T, parent: Option<TreeNodeIdx>) -> TreeNodeIdx {
        let idx = self.arena.len();
        if let Some(parent_idx) = parent {
            self.get_mut(parent_idx).unwrap().children.push(idx);
        }
        self.arena.push(TreeNode::new(idx, val, parent));
        idx
    }

    pub fn get(&self, idx: TreeNodeIdx) -> Option<&TreeNode<T>> {
        self.arena.get(idx)
    }

    pub fn get_mut(&mut self, idx: TreeNodeIdx) -> Option<&mut TreeNode<T>> {
        self.arena.get_mut(idx)
    }
}

#[derive(Debug)]
pub struct TreeNode<T> {
    idx: TreeNodeIdx,
    inner: T,
    pub parent: Option<TreeNodeIdx>,
    children: Vec<TreeNodeIdx>,
}

impl<T> TreeNode<T> {
    pub fn new(idx: TreeNodeIdx, inner: T, parent: Option<TreeNodeIdx>) -> Self {
        Self {
            idx,
            inner,
            parent,
            children: vec![],
        }
    }

    pub fn get(&self) -> &T {
        &self.inner
    }

    pub fn get_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}
