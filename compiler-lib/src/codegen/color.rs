use std::collections::{HashMap, HashSet};

use crate::{
    asm::{Allocation, Instruction, Temp},
    common::Temp as CommonTemp,
    frame::Frame,
    ir::{Expr, Stmt},
};

use super::{
    graph::{Matrix, ID},
    liveness::LiveGraph,
    Codegen,
};

type SpillCost = HashMap<Temp, u32>;
type Registers<F> = Vec<<F as Frame>::Register>;
type SpilledNodes = HashSet<Temp>;

pub fn color<C: Codegen>(
    _frame: &mut C::Frame,
    interference: &LiveGraph,
    initial: Allocation<C::Frame>,
    _spill_cost: &SpillCost,
    registers: Registers<C::Frame>,
) -> (Allocation<C::Frame>, SpilledNodes) {
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
    let colors: Colors<C::Frame> = initial
        .iter()
        .filter(|(temp, _)| all_temp.contains(temp))
        .map(|(temp, reg)| (interference.id(temp), reg.clone()))
        .collect();

    // all machine registers set
    let all_colors: AllColors<C::Frame> = registers.into_iter().collect();

    let all_id_set: HashSet<LiveID> = all_temp.iter().map(|temp| interference.id(temp)).collect();
    let colored_node_id_set: HashSet<LiveID> = colors.keys().copied().collect();

    // non colored temporary in graph.
    let initial: Initial = all_id_set
        .difference(&colored_node_id_set)
        .cloned()
        .collect();

    let (colors, spilled_node) = _main::<C>(
        _frame,
        interference,
        &pre_colored,
        colors,
        &all_colors,
        initial,
    );

    let allocation: Allocation<C::Frame> = colors
        .into_iter()
        .map(|(id, reg)| (interference.temp(id), reg))
        .collect();

    (allocation, spilled_node)
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
// type Temp2ID = HashMap<Temp, LiveID>;

fn _main<C: Codegen>(
    _frame: &mut C::Frame,
    live_graph: &LiveGraph,
    precolored: &PreColored,
    mut colors: Colors<C::Frame>,
    all_colors: &AllColors<C::Frame>,
    initial: Initial,
) -> (Colors<C::Frame>, SpilledNodes) {
    // dbg!(&live_graph);
    // dbg!(&precolored);
    // dbg!(&colors);
    // dbg!(&all_colors);
    // dbg!(&initial);

    let (_matrix, _adj_set, adj_list, mut degree) = build(live_graph, precolored);
    let k = <C::Frame as Frame>::registers().len();
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

    let mut spilled_nodes = SpilledNodes::new();
    assign_colors::<C::Frame>(
        live_graph.id2temp(),
        &select_stack,
        &adj_list,
        precolored,
        &mut colored_nodes,
        &mut colors,
        all_colors,
        &mut spilled_nodes,
    );

    (colors, spilled_nodes)
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

#[allow(clippy::too_many_arguments)]
fn assign_colors<F: Frame>(
    id2temp: &ID2Temp,
    select_stack: &SelectStack,
    adj_list: &AdjList,
    precolored: &PreColored,
    colored_nodes: &mut ColoredNodes,
    colors: &mut Colors<F>,
    all_colors: &AllColors<F>,
    spilled_nodes: &mut SpilledNodes,
) {
    for id in select_stack {
        let mut ok_colors = all_colors.clone();

        for adj in adj_list.get(id).unwrap() {
            if colored_nodes.contains(adj) || precolored.contains(&id2temp[adj]) {
                ok_colors.remove(&colors[adj]);
            }
        }

        if ok_colors.is_empty() {
            spilled_nodes.insert(id2temp[id]);
        } else {
            colored_nodes.insert(*id);
            let c = ok_colors.iter().next().unwrap();
            colors.insert(*id, c.clone());
        }
    }
}

pub fn rewrite_program<C: Codegen>(
    frame: &mut C::Frame,
    live_graph: &LiveGraph,
    spilled_nodes: &SpilledNodes,
    instructions: Vec<Instruction>,
) -> (Vec<Instruction>, Initial) {
    #[allow(clippy::type_complexity)]
    fn new_instruction<C: Codegen>(
        live_graph: &LiveGraph,
        frame: &C::Frame,
        dst: &[Temp],
        src: &[Temp],
        access_map: &HashMap<ID, <C::Frame as Frame>::Access>,
    ) -> (
        Vec<Instruction>,
        (Vec<Temp>, Vec<Temp>),
        Vec<Instruction>,
        Vec<Temp>,
    ) {
        let mut new_temps = Vec::new();
        let mut new_dst = Vec::new();
        let mut begin_instructions = Vec::new();

        for &temp in dst {
            if let Some(access) = access_map.get(&live_graph.id(&temp)) {
                let new_temp = CommonTemp::new();
                new_temps.push(Temp::from(new_temp));

                let mem =
                    <C::Frame as Frame>::exp(access.clone(), Expr::Temp(<C::Frame as Frame>::fp()));
                let stmt = Stmt::Move(Box::new(Expr::Temp(new_temp)), Box::new(mem));

                begin_instructions.append(&mut C::codegen(frame, stmt));
                new_dst.push(Temp::from(new_temp));
            } else {
                new_dst.push(temp);
            }
        }

        let mut new_src = Vec::new();
        let mut end_instructions = Vec::new();
        for &temp in src {
            if let Some(access) = access_map.get(&live_graph.id(&temp)) {
                let new_temp = CommonTemp::new();
                new_temps.push(Temp::from(new_temp));

                let mem =
                    <C::Frame as Frame>::exp(access.clone(), Expr::Temp(<C::Frame as Frame>::fp()));

                let stmt = Stmt::Move(Box::new(Expr::Temp(new_temp)), Box::new(mem));
                end_instructions.append(&mut C::codegen(frame, stmt));
                new_src.push(Temp::from(new_temp));
            } else {
                new_src.push(temp);
            }
        }

        (
            begin_instructions,
            (new_dst, new_src),
            end_instructions,
            new_temps,
        )
    }

    let mut access_map = HashMap::new();
    for spilled in spilled_nodes.iter() {
        let access = frame.alloc_local(true);
        let id = live_graph.id(spilled);
        access_map.insert(id, access);
    }

    let mut new_instructions = Vec::new();
    let mut new_temps = Vec::new();
    for instruction in instructions {
        match instruction {
            Instruction::Operand {
                dst,
                src,
                assembly,
                jump,
            } => {
                let (
                    mut begin_instructions,
                    (new_dst, new_src),
                    mut end_instructions,
                    mut new_temps_inner,
                ) = new_instruction::<C>(live_graph, frame, &dst, &src, &access_map);
                new_temps.append(&mut new_temps_inner);

                new_instructions.append(&mut begin_instructions);
                new_instructions.push(Instruction::Operand {
                    dst: new_dst,
                    src: new_src,
                    assembly,
                    jump,
                });
                new_instructions.append(&mut end_instructions);
            }

            Instruction::Move { dst, src, assembly } => {
                let (
                    mut begin_instructions,
                    (new_dst, new_src),
                    mut end_instructions,
                    mut new_temps_inner,
                ) = new_instruction::<C>(live_graph, frame, &[dst], &[src], &access_map);
                new_temps.append(&mut new_temps_inner);

                assert!(new_dst.len() == 1);
                assert!(new_src.len() == 1);

                new_instructions.append(&mut begin_instructions);
                new_instructions.push(Instruction::Move {
                    dst: new_dst[0],
                    src: new_src[0],
                    assembly,
                });
                new_instructions.append(&mut end_instructions);
            }

            x => {
                new_instructions.push(x);
            }
        }
    }

    // TODO: initial
    // let new_temps: HashSet<_> = new_temps.into_iter().map(|v| live_graph.id(&v)).collect();
    // let initial: HashSet<_> = new_temps.union(colored_nodes).copied().collect();
    let initial = Initial::new();

    (new_instructions, initial)
}
