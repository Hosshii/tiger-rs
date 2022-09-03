use std::collections::{HashMap, HashSet};

use crate::{
    asm::{Allocation, Instruction, Temp},
    frame::Frame,
};

use super::{
    color,
    flow::FlowGraph,
    liveness::{self},
    Codegen,
};

pub fn alloc<C: Codegen>(
    instructions: Vec<Instruction>,
    frame: &mut C::Frame,
) -> (Vec<Instruction>, Allocation<C::Frame>) {
    let flow_graph = FlowGraph::convert(instructions.clone());
    let (interference, _) = liveness::analyze(&flow_graph);

    let initial = <C::Frame as Frame>::temp_map()
        .iter()
        .map(|(temp, reg)| (temp.into(), reg.clone()))
        .collect();

    let spill_cost: HashMap<Temp, u32> = interference
        .graph_ref()
        .nodes()
        .iter()
        .map(|node| (*node.val(), 1))
        .collect();

    let registers = <C::Frame as Frame>::registers().to_vec();

    let (alloc, _, instructions) = color::color::<C>(
        frame,
        instructions,
        &interference,
        initial,
        &spill_cost,
        registers,
    );

    (instructions, alloc)
}
