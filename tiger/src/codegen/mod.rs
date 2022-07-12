use crate::{asm::Instruction, frame::Frame, ir::Stmt};

pub mod arm64;
pub(super) mod color;
pub(super) mod flow;
mod graph;
pub(super) mod liveness;
mod reg_alloc;

pub trait Codegen {
    type Frame: Frame;

    fn codegen(frame: Self::Frame, stmt: Stmt) -> Vec<Instruction>;
}
