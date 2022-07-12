use std::collections::{HashMap, HashSet};

use crate::asm::Temp;

use super::{
    flow::FlowGraph,
    graph::{Graph, ID},
};

pub fn analyze(flow_graph: &FlowGraph) -> (LiveGraph, LiveMap) {
    let live_map = live_map(flow_graph);

    let mut live_graph = LiveGraph::new(flow_graph);
    live_graph.analyze(flow_graph, &live_map);

    (live_graph, live_map)
}

pub type Node = Temp;
pub struct LiveGraph {
    graph: Graph<Node>,
    temp2id: HashMap<Node, ID>,
    id2temp: HashMap<ID, Node>,
    _moves: Vec<(Node, Node)>, // (from, to)
}

impl LiveGraph {
    pub fn graph_ref(&self) -> &Graph<Node> {
        &self.graph
    }

    pub fn id(&self, temp: &Node) -> ID {
        self.temp2id[temp]
    }

    pub fn temp(&self, id: ID) -> Node {
        self.id2temp[&id]
    }

    fn new(flow_graph: &FlowGraph) -> Self {
        Self::init_graph(flow_graph)
    }

    fn init_graph(flow_graph: &FlowGraph) -> Self {
        let mut temps: HashSet<Temp> = HashSet::new();

        for node in flow_graph.graph_ref().nodes() {
            temps.extend(node.val().defs());
            temps.extend(node.val().uses());
        }

        let mut graph = Graph::with_capacity(temps.len());
        let mut temp2id = HashMap::new();
        let mut id2temp = HashMap::new();

        for temp in temps {
            let id = graph.insert(temp);
            temp2id.insert(temp, id);
            id2temp.insert(id, temp);
        }

        let mut moves = Vec::new();
        for node in flow_graph.graph_ref().nodes() {
            let node = node.val();
            if node.is_move {
                assert_eq!(node.defs().len(), 1);
                assert_eq!(node.uses().len(), 1);

                let from = node.defs().iter().next().unwrap();
                let to = node.uses().iter().next().unwrap();
                moves.push((*from, *to));
            }
        }

        Self {
            graph,
            temp2id,
            id2temp,
            _moves: moves,
        }
    }

    fn analyze(&mut self, flow_graph: &FlowGraph, live_map: &LiveMap) {
        for flow_node in flow_graph.graph_ref().nodes() {
            let defs = flow_node.val().defs();
            let flow_id = flow_node.id();

            let lives = &live_map[&flow_id];
            for def in defs {
                for live in lives {
                    if live != def {
                        let from = self.temp2id[def];
                        let to = self.temp2id[live];
                        self.graph.double_link(from, to);
                    }
                }
            }
        }
    }
}

pub type LiveSet = HashSet<Temp>;
pub type LiveMap = HashMap<ID, LiveSet>; // FlowGraph's ID

fn live_map(flow_graph: &FlowGraph) -> LiveMap {
    let flow_graph = flow_graph.graph_ref();
    let flow_nodes = flow_graph.nodes();

    let mut ins = LiveMap::new();
    let mut outs = LiveMap::new();

    // init
    for node in flow_nodes {
        let id = node.id();
        ins.insert(id, HashSet::new());
        outs.insert(id, HashSet::new());
    }

    let mut has_change = false;
    loop {
        for flow_node in flow_nodes {
            let id = flow_node.id();

            let uses = flow_node.val().uses();
            let def = flow_node.val().defs();
            let out = &outs[&id];
            let diff = out - def;
            // TODO: use efficient algorithm.
            let new_in = uses.union(&diff).copied().collect();

            has_change |= ins[&id] != new_in;

            let succs = flow_graph.succ(id);
            let mut new_out = HashSet::new();
            for succ in succs {
                let succ = &ins[&succ];
                new_out = new_out.union(succ).copied().collect();
            }

            has_change |= outs[&id] != new_out;

            ins.insert(id, new_in);
            outs.insert(id, new_out);
        }

        if !has_change {
            break;
        }
        has_change = false
    }

    outs
}

#[cfg(test)]
mod tests {
    use crate::codegen::flow::Node;

    use super::*;

    #[test]
    fn test_liveness() {
        //     a <- 0
        // L1: b <- a + 1
        //     c <- c + b
        //     a <- b * 2
        //     if a < N goto L1
        //     return c

        // create graph above.
        let nodes = vec![
            Node {
                id: 0,
                defs: HashSet::from([Temp::new_with(0)]),
                uses: HashSet::new(),
                is_move: false, // TODO: should be true. Currently, we cannot use immediate in `uses`.
            },
            Node {
                id: 1,
                defs: HashSet::from([Temp::new_with(1)]),
                uses: HashSet::from([Temp::new_with(0)]),
                is_move: false,
            },
            Node {
                id: 2,
                defs: HashSet::from([Temp::new_with(2)]),
                uses: HashSet::from([Temp::new_with(2), Temp::new_with(1)]),
                is_move: false,
            },
            Node {
                id: 3,
                defs: HashSet::from([Temp::new_with(0)]),
                uses: HashSet::from([Temp::new_with(1)]),
                is_move: false,
            },
            Node {
                id: 4,
                defs: HashSet::new(),
                uses: HashSet::from([Temp::new_with(0)]),
                is_move: false,
            },
            Node {
                id: 5,
                defs: HashSet::new(),
                uses: HashSet::from([Temp::new_with(2)]),
                is_move: false,
            },
        ];
        let edges = vec![(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (4, 1)];
        let mut ids = Vec::new();

        let mut graph = Graph::new();
        for node in nodes {
            let id = graph.insert(node);
            ids.push(id);
        }

        for (from, to) in edges {
            graph.link(ids[from], ids[to]);
        }

        // crate flow graph
        let flow = FlowGraph { graph };

        let (live_graph, live_map) = analyze(&flow);

        // live map test
        let expected_live_map = vec![
            HashSet::from([Temp::new_with(0), Temp::new_with(2)]),
            HashSet::from([Temp::new_with(1), Temp::new_with(2)]),
            HashSet::from([Temp::new_with(1), Temp::new_with(2)]),
            HashSet::from([Temp::new_with(0), Temp::new_with(2)]),
            HashSet::from([Temp::new_with(0), Temp::new_with(2)]),
            HashSet::from([]),
        ];
        assert_eq!(expected_live_map.len(), ids.len());

        let expected_live_map: HashMap<_, _> =
            ids.into_iter().zip(expected_live_map.into_iter()).collect();

        assert_eq!(expected_live_map, live_map);

        // live_node test
        let live_nodes = live_graph.graph.nodes();

        for node in live_nodes {
            let tmp = *node.val();
            let pred: HashSet<_> = live_graph.graph.pred(node.id()).collect();
            let succ: HashSet<_> = live_graph.graph.succ(node.id()).collect();
            assert_eq!(pred, succ);

            let set: HashSet<_> = pred.iter().map(|id| live_graph.id2temp[id]).collect();

            if tmp == Temp::new_with(0) || tmp == Temp::new_with(1) {
                assert_eq!(set, HashSet::from([Temp::new_with(2)]));
            } else if tmp == Temp::new_with(2) {
                assert_eq!(set, HashSet::from([Temp::new_with(0), Temp::new_with(1)]));
            } else {
                unreachable!()
            }
        }
    }
}
