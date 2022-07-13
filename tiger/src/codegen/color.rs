use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::{
    asm::{Allocation, Temp},
    frame::Frame,
};

use super::{
    graph::{Matrix, ID},
    liveness::LiveGraph,
};

type SpillCost = HashMap<Temp, u32>;
type Registers<F> = Vec<<F as Frame>::Register>;

pub fn color<F: Frame>(
    interference: LiveGraph,
    initial: Allocation<F>,
    _spill_cost: SpillCost,
    registers: Registers<F>,
) -> (Allocation<F>, Vec<Temp>) {
    // All machine register that is already colored.
    let pre_colored: PreColored = initial.keys().copied().collect();

    // all temporary in graph node.
    let all_temp: HashSet<_> = interference
        .graph_ref()
        .nodes()
        .iter()
        .map(|node| node.val())
        .collect();

    // colored node.
    let colors: Colors<F> = initial
        .iter()
        .filter(|(temp, _)| all_temp.contains(temp))
        .map(|(temp, reg)| (interference.id(temp), reg.clone()))
        .collect();

    // all machine registers set
    let all_colors: AllColors<F> = registers.into_iter().collect();

    let all_id_set: HashSet<LiveID> = all_temp.iter().map(|temp| interference.id(temp)).collect();
    let colored_node_id_set: HashSet<LiveID> = colors.keys().copied().collect();

    // non colored temporary in graph.
    let initial: Initial = all_id_set
        .difference(&colored_node_id_set)
        .cloned()
        .collect();

    let (colors, spills) = _main::<F>(&interference, pre_colored, colors, all_colors, initial);

    let allocation: Allocation<F> = colors
        .into_iter()
        .map(|(id, reg)| (interference.temp(id), reg))
        .collect();

    let spills: Vec<Temp> = spills.into_iter().map(|id| interference.temp(id)).collect();

    (allocation, spills)
}

type Degree = HashMap<LiveID, usize>;
type AdjList = HashMap<LiveID, HashSet<LiveID>>;
type AdjSet = HashSet<Edge>;
type PreColored = HashSet<Temp>;
type Edge = (LiveID, LiveID);
type SimplifyWorkList = Vec<LiveID>;
type SpillWorkList = HashSet<ID>;
type SelectStack = Vec<LiveID>;
type LiveID = ID;
type ColoredNodes = HashSet<LiveID>;
type Colors<F> = HashMap<LiveID, <F as Frame>::Register>; // fmap (\temp,v -> temp2id(temp)) Allocation
type AllColors<F> = HashSet<<F as Frame>::Register>;
type Initial = HashSet<LiveID>;
type ID2Temp = HashMap<LiveID, Temp>;
type Temp2ID = HashMap<Temp, LiveID>;

fn _main<F: Frame>(
    live_graph: &LiveGraph,
    precolored: PreColored,
    mut colors: Colors<F>,
    all_colors: AllColors<F>,
    initial: Initial,
) -> (Colors<F>, SpillWorkList) {
    // dbg!(&live_graph);
    // dbg!(&precolored);
    // dbg!(&colors);
    // dbg!(&all_colors);
    // dbg!(&initial);

    let (_matrix, _adj_set, adj_list, mut degree) = build(live_graph, &precolored);
    let k = F::registers().len();
    let (mut simplify_worklist, mut spill_work_list) = make_work_list(&initial, &degree, k);

    let mut select_stack = SelectStack::new();
    loop {
        if !simplify_worklist.is_empty() {
            simplify(
                &mut select_stack,
                &mut simplify_worklist,
                &mut spill_work_list,
                &adj_list,
                &mut degree,
                k,
            )
        } else if !spill_work_list.is_empty() {
            select_spill(&mut spill_work_list, &mut simplify_worklist);
        }

        if simplify_worklist.is_empty() && spill_work_list.is_empty() {
            break;
        }
    }

    let mut colored_nodes: ColoredNodes = colors.keys().copied().collect();

    assign_colors::<F>(
        live_graph.id2temp(),
        &select_stack,
        &adj_list,
        &precolored,
        &mut colored_nodes,
        &mut colors,
        &all_colors,
        &mut spill_work_list,
    );

    if !spill_work_list.is_empty() {
        unimplemented!("spill is not implemented yet");
    }

    (colors, spill_work_list)
}

fn build(
    live_graph: &LiveGraph,
    precolored: &PreColored,
) -> (Matrix<bool>, AdjSet, AdjList, Degree) {
    let mut adj_set = AdjSet::new();
    let mut adj_list = AdjList::new();
    let mut degree = Degree::new();

    let graph_inner = live_graph.graph_ref();

    // if `m \in precolored` and `m \in node`
    // then degree[m] = usize::max
    for node in graph_inner.nodes() {
        let temp = node.val();
        if precolored.contains(temp) {
            let id = node.id();
            degree.insert(id, usize::max_value());
        }
    }

    let node_len = graph_inner.nodes().len();
    let mut matrix = Matrix::new_with(node_len);
    for node in graph_inner.nodes() {
        let one = node.id();
        for other in node.adj() {
            if one != other {
                matrix.link(one, other);
                add_edge(
                    live_graph,
                    precolored,
                    &mut adj_set,
                    &mut adj_list,
                    &mut degree,
                    (one, other),
                );
            }
        }
    }

    (matrix, adj_set, adj_list, degree)
}

fn add_edge(
    live_graph: &LiveGraph,
    precolored: &PreColored,
    adj_set: &mut AdjSet,
    adj_list: &mut AdjList,
    degree: &mut Degree,
    (u, v): Edge,
) {
    if u == v {
        return;
    }

    adj_set.insert((u, v));
    adj_set.insert((v, u));

    if !precolored.contains(live_graph.graph_ref().get(u).val()) {
        adj_list.entry(u).or_default().insert(v);
        *degree.entry(u).or_insert(0) += 1;
    }

    if !precolored.contains(live_graph.graph_ref().get(v).val()) {
        adj_list.entry(v).or_default().insert(u);
        *degree.entry(v).or_insert(0) += 1;
    }
}

fn adjacent<'a>(
    id: LiveID,
    adj_list: &'a AdjList,
    select_stack: &'a SelectStack,
) -> impl Iterator<Item = LiveID> {
    let set = adj_list.get(&id).expect("adj_list");
    let select_stack_set: HashSet<LiveID> = select_stack.iter().copied().collect();
    set.difference(&select_stack_set)
        .copied()
        .collect::<HashSet<_>>()
        .into_iter()
}

/// Take initial registers, which is temporary and not colored, and max degree of graph.
/// Returns simplifyWorkList.
fn make_work_list(
    initial: &Initial,
    degree: &Degree,
    k: usize,
) -> (SimplifyWorkList, SpillWorkList) {
    let mut simplify_work_list = SimplifyWorkList::new();
    let mut spill_work_list = SpillWorkList::new();
    for &id in initial {
        let deg = degree[&id];
        if k <= deg {
            spill_work_list.insert(id);
        } else if deg < k {
            simplify_work_list.push(id);
        }
    }

    (simplify_work_list, spill_work_list)
}

fn simplify(
    select_stack: &mut SelectStack,
    simplify_worklist: &mut SimplifyWorkList,
    spill_work_list: &mut SpillWorkList,
    adj_list: &HashMap<LiveID, HashSet<LiveID>>,
    degree: &mut Degree,
    k: usize,
) {
    if simplify_worklist.is_empty() {
        panic!("simplify_worklist is empty");
    }

    let id = simplify_worklist.pop().unwrap();

    select_stack.push(id);

    let adjs = adjacent(id, adj_list, select_stack);
    for adj in adjs {
        decrement_degree(adj, k, degree, simplify_worklist, spill_work_list);
    }
}

fn decrement_degree(
    id: LiveID,
    k: usize,
    degree: &mut Degree,
    simplify_worklist: &mut SimplifyWorkList,
    spill_work_list: &mut SpillWorkList,
) {
    let old_deg = degree[&id];
    *degree.get_mut(&id).unwrap() -= 1;

    if old_deg == k {
        assert!(spill_work_list.remove(&id));
        simplify_worklist.push(id);
    }
}

fn select_spill(
    spill_work_list: &mut SpillWorkList,
    simplify_worklist: &mut SimplifyWorkList,
) -> LiveID {
    let &key = spill_work_list.iter().next().unwrap();
    spill_work_list.remove(&key);

    simplify_worklist.push(key);
    key
}

fn assign_colors<F: Frame>(
    id2temp: &ID2Temp,
    select_stack: &SelectStack,
    adj_list: &AdjList,
    precolored: &PreColored,
    colored_nodes: &mut ColoredNodes,
    colors: &mut Colors<F>,
    all_colors: &AllColors<F>,
    spill_work_list: &mut SpillWorkList,
) {
    for id in select_stack {
        let mut ok_colors = all_colors.clone();

        for adj in adj_list.get(id).unwrap() {
            if colored_nodes.contains(adj) || precolored.contains(&id2temp[adj]) {
                ok_colors.remove(&colors[adj]);
            }
        }

        if ok_colors.is_empty() {
            spill_work_list.insert(*id);
        } else {
            colored_nodes.insert(*id);
            let c = ok_colors.iter().next().unwrap();
            colors.insert(*id, c.clone());
        }
    }
}
