use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ID(usize);

pub struct Node<T> {
    id: ID,
    val: T,
    succ: HashSet<ID>,
    pred: HashSet<ID>,
}

impl<T> Node<T> {
    pub fn val(&self) -> &T {
        &self.val
    }

    fn new(val: T, id: ID) -> Self {
        Node {
            id,
            val,
            succ: HashSet::new(),
            pred: HashSet::new(),
        }
    }

    pub fn succ(&self) -> impl Iterator<Item = ID> + '_ {
        self.succ.iter().copied()
    }

    pub fn pred(&self) -> impl Iterator<Item = ID> + '_ {
        self.pred.iter().copied()
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Eq for Node<T> {}

pub struct Graph<T> {
    nodes: Vec<Node<T>>,
}

impl<T> Graph<T> {
    pub fn get(&self, id: ID) -> &Node<T> {
        &self.nodes[id.0]
    }

    pub fn insert(&mut self, val: T) -> ID {
        let id = self.nodes.len();
        self.nodes.push(Node::new(val, ID(id)));
        ID(id)
    }

    pub fn link(&mut self, from: ID, to: ID) {
        assert!(self.nodes[from.0].succ.insert(to));
        assert!(self.nodes[to.0].pred.insert(from));
    }

    pub fn unlink(&mut self, from: ID, to: ID) {
        assert!(self.nodes[from.0].succ.remove(&to));
        assert!(self.nodes[to.0].pred.remove(&from));
    }

    pub fn new() -> Self {
        Graph { nodes: Vec::new() }
    }

    pub fn nodes(&self) -> &[Node<T>] {
        &self.nodes
    }
}
