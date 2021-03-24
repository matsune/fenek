pub type TreeNodeIdx = usize;

#[derive(Debug, Default)]
pub struct ArenaTree<T> {
    arena: Vec<TreeNode<T>>,
}

impl<T> ArenaTree<T> {
    pub fn add_node(&mut self, val: T) -> TreeNodeIdx {
        let idx = self.arena.len();
        self.arena.push(TreeNode::new(idx, val));
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
    val: T,
    parent: Option<TreeNodeIdx>,
    children: Vec<TreeNodeIdx>,
}

impl<T> TreeNode<T> {
    pub fn new(idx: TreeNodeIdx, val: T) -> Self {
        Self {
            idx,
            val,
            parent: None,
            children: vec![],
        }
    }

    pub fn get(&self) -> &T {
        &self.val
    }

    pub fn get_mut(&mut self) -> &mut T {
        &mut self.val
    }
}
