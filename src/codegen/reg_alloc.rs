use std::collections::HashMap;

use crate::{
    asm::{Allocation, Instruction},
    frame::Frame,
};

use super::{color, flow::FlowGraph, liveness};

pub fn alloc<F: Frame>(
    instructions: Vec<Instruction<F::Temp, F::Label>>,
    _frame: &F,
) -> (Vec<Instruction<F::Temp, F::Label>>, Allocation<F>) {
    let flow_graph = FlowGraph::convert(instructions.clone());
    let (interference, _) = liveness::analyze(&flow_graph);

    let initial = F::temp_map()
        .iter()
        .map(|(temp, reg)| (temp.into(), reg.clone()))
        .collect();

    let spill_cost: HashMap<F::Temp, u32> = interference
        .graph_ref()
        .nodes()
        .iter()
        .map(|node| (*node.val(), 1))
        .collect();

    let registers = F::registers().to_vec();

    let (alloc, _) = color::color::<F>(interference, initial, spill_cost, registers);

    (instructions, alloc)
}
