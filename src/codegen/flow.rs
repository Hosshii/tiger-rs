use std::collections::{HashMap, HashSet};

use crate::asm::{Instruction, LabelTrait, TempTrait};

use super::graph::Graph;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node<T: TempTrait> {
    pub id: usize, // for debug
    pub defs: HashSet<T>,
    pub uses: HashSet<T>,
    pub is_move: bool,
}

impl<T: TempTrait> Node<T> {
    pub fn defs(&self) -> &HashSet<T> {
        &self.defs
    }

    pub fn uses(&self) -> &HashSet<T> {
        &self.uses
    }
}
pub struct FlowGraph<T: TempTrait> {
    pub(super) graph: Graph<Node<T>>,
}

impl<T: TempTrait> FlowGraph<T> {
    pub fn graph_ref(&self) -> &Graph<Node<T>> {
        &self.graph
    }

    pub fn convert<L: LabelTrait>(instructions: Vec<Instruction<T, L>>) -> Self {
        let mut graph = FlowGraph {
            graph: Graph::new(),
        };
        graph.convert_inner(instructions);

        graph
    }

    fn convert_inner<L: LabelTrait>(&mut self, instructions: Vec<Instruction<T, L>>) {
        let mut jumps = Vec::new();
        let mut labels: HashMap<L, _> = HashMap::new();

        let mut before_id = None;
        for (id, instruction) in instructions.into_iter().enumerate() {
            match instruction {
                Instruction::Move { dst, src, .. } => {
                    let node = Node {
                        id,
                        defs: HashSet::from([dst]),
                        uses: HashSet::from([src]),
                        is_move: true,
                    };
                    let id = self.graph.insert(node);
                    if let Some(before) = before_id {
                        let linked = self.graph.link(before, id);
                        assert!(linked);
                    }
                    before_id = Some(id);
                }
                Instruction::Operand { dst, src, jump, .. } => {
                    let node = Node {
                        id,
                        defs: HashSet::from_iter(dst.into_iter()),
                        uses: HashSet::from_iter(src.into_iter()),
                        is_move: false,
                    };
                    let id = self.graph.insert(node);
                    match (before_id, jump) {
                        (Some(before), Some(jump)) => {
                            let linked = self.graph.link(before, id);
                            assert!(linked);
                            jumps.push((id, jump));
                            before_id = None;
                        }
                        (Some(before), None) => {
                            let linked = self.graph.link(before, id);
                            assert!(linked);
                            before_id = Some(id);
                        }
                        (None, Some(jump)) => {
                            jumps.push((id, jump));
                            before_id = None;
                        }
                        (None, None) => {
                            before_id = Some(id);
                        }
                    }
                }
                Instruction::Label { label, .. } => {
                    let node = Node {
                        id,
                        defs: HashSet::new(),
                        uses: HashSet::new(),
                        is_move: false,
                    };
                    let id = self.graph.insert(node);
                    labels.insert(label, id);

                    if let Some(before) = before_id {
                        let linked = self.graph.link(before, id);
                        assert!(linked);
                    }
                    before_id = Some(id);
                }
                Instruction::Comment { .. } => {
                    continue;
                }
            }
        }

        for (from, tos) in jumps {
            for to in tos {
                let to = labels[&to];
                let linked = self.graph.link(from, to);
                assert!(linked);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::arm64::{ARM64Label, ARM64Temp};

    use super::*;

    #[test]
    fn test_convert() {
        use Instruction::*;
        let mut block1 = vec![
            Label {
                assembly: Default::default(),
                label: ARM64Label::with_num(0),
            },
            Move {
                assembly: Default::default(),
                dst: ARM64Temp::with(0),
                src: ARM64Temp::with(1),
            },
            Operand {
                assembly: Default::default(),
                dst: vec![ARM64Temp::with(2)],
                src: vec![ARM64Temp::with(3)],
                jump: None,
            },
            Operand {
                assembly: Default::default(),
                dst: vec![ARM64Temp::with(4)],
                src: vec![ARM64Temp::with(5)],
                jump: Some(vec![ARM64Label::with_num(1)]),
            },
        ];

        let mut block2 = vec![
            Label {
                assembly: Default::default(),
                label: ARM64Label::with_num(1),
            },
            Operand {
                assembly: Default::default(),
                dst: vec![],
                src: vec![],
                jump: Some(vec![ARM64Label::with_num(0), ARM64Label::with_num(1)]),
            },
        ];

        block1.append(&mut block2);

        let flow = FlowGraph::convert(block1);
        let graph = flow.graph;

        struct Assert {
            cur: Node<ARM64Temp>,
            pred: Vec<Node<ARM64Temp>>,
            succ: Vec<Node<ARM64Temp>>,
        }

        let nodes = vec![
            Node {
                id: 0,
                defs: HashSet::new(),
                uses: HashSet::new(),
                is_move: false,
            },
            Node {
                id: 1,
                defs: HashSet::from([ARM64Temp::with(0)]),
                uses: HashSet::from([ARM64Temp::with(1)]),
                is_move: true,
            },
            Node {
                id: 2,
                defs: HashSet::from([ARM64Temp::with(2)]),
                uses: HashSet::from([ARM64Temp::with(3)]),
                is_move: false,
            },
            Node {
                id: 3,
                defs: HashSet::from([ARM64Temp::with(4)]),
                uses: HashSet::from([ARM64Temp::with(5)]),
                is_move: false,
            },
            Node {
                id: 4,
                defs: HashSet::new(),
                uses: HashSet::new(),
                is_move: false,
            },
            Node {
                id: 5,
                defs: HashSet::new(),
                uses: HashSet::new(),
                is_move: false,
            },
        ];

        let expected = vec![
            Assert {
                cur: nodes[0].clone(),
                pred: vec![nodes[5].clone()],
                succ: vec![nodes[1].clone()],
            },
            Assert {
                cur: nodes[1].clone(),
                pred: vec![nodes[0].clone()],
                succ: vec![nodes[2].clone()],
            },
            Assert {
                cur: nodes[2].clone(),
                pred: vec![nodes[1].clone()],
                succ: vec![nodes[3].clone()],
            },
            Assert {
                cur: nodes[3].clone(),
                pred: vec![nodes[2].clone()],
                succ: vec![nodes[4].clone()],
            },
            Assert {
                cur: nodes[4].clone(),
                pred: vec![nodes[3].clone(), nodes[5].clone()],
                succ: vec![nodes[5].clone()],
            },
            Assert {
                cur: nodes[5].clone(),
                pred: vec![nodes[4].clone()],
                succ: vec![nodes[0].clone(), nodes[4].clone()],
            },
        ];

        assert_eq!(graph.nodes().len(), expected.len());
        for (expected, actual) in expected.into_iter().zip(graph.nodes().iter()) {
            assert_eq!(&expected.cur, actual.val());

            let mut actual_pred = graph
                .pred(actual.id())
                .map(|id| graph.get(id).val())
                .cloned()
                .collect::<Vec<_>>();
            actual_pred.sort_by_key(|v| v.id);
            assert_eq!(expected.pred, actual_pred);

            let mut actual_succ = graph
                .succ(actual.id())
                .map(|id| graph.get(id).val())
                .cloned()
                .collect::<Vec<_>>();
            actual_succ.sort_by_key(|v| v.id);
            assert_eq!(expected.succ, actual_succ);
        }
    }
}
