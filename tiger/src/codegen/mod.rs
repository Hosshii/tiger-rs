use crate::{asm::Instruction, frame::Frame, ir::Stmt};

pub mod arm64;
mod graph;

pub trait Codegen {
    type Frame: Frame;

    fn codegen(frame: Self::Frame, stmt: Stmt) -> Vec<Instruction>;
}
