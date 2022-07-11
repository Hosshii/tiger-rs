use itertools::Itertools;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ID(usize);

#[derive(Debug)]
pub struct Node<T> {
    id: ID,
    val: T,
    succ: HashSet<ID>,
    pred: HashSet<ID>,
}

impl<T> Node<T> {
    pub fn id(&self) -> ID {
        self.id
    }

    /// Unique number between `0 ~ (graph.len() - 1)`.
    pub fn index(&self) -> usize {
        self.id.0
    }

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

    pub fn adj(&self) -> impl Iterator<Item = ID> + '_ {
        self.succ().chain(self.pred()).unique()
    }

    pub fn deg(&self) -> usize {
        self.succ.len() + self.pred.len()
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Eq for Node<T> {}

#[derive(Debug)]
pub struct Graph<T> {
    nodes: Vec<Node<T>>,
}

impl<T> Graph<T> {
    pub fn deg(&self, id: ID) -> usize {
        self.get(id).deg()
    }

    pub fn get(&self, id: ID) -> &Node<T> {
        &self.nodes[id.0]
    }

    pub fn insert(&mut self, val: T) -> ID {
        let id = self.nodes.len();
        self.nodes.push(Node::new(val, ID(id)));
        ID(id)
    }

    pub fn link(&mut self, from: ID, to: ID) -> bool {
        let t1 = self.nodes[from.0].succ.insert(to);
        let t2 = self.nodes[to.0].pred.insert(from);
        assert_eq!(t1, t2);
        t1
    }

    pub fn pred(&self, id: ID) -> impl Iterator<Item = ID> + '_ {
        self.get(id).pred()
    }

    pub fn succ(&self, id: ID) -> impl Iterator<Item = ID> + '_ {
        self.get(id).succ()
    }

    pub fn unlink(&mut self, from: ID, to: ID) -> bool {
        let t1 = self.nodes[from.0].succ.remove(&to);
        let t2 = self.nodes[to.0].pred.remove(&from);
        assert_eq!(t1, t2);
        t1
    }

    pub fn new() -> Self {
        Graph { nodes: Vec::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Graph {
            nodes: Vec::with_capacity(cap),
        }
    }

    pub fn nodes(&self) -> &[Node<T>] {
        &self.nodes
    }
}
