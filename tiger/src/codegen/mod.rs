use crate::{asm::Instruction, frame::Frame, ir::Stmt};

pub mod arm64;

trait Codegen {
    type Frame: Frame;

    fn codegen(f: Self::Frame, stmt: Stmt) -> Vec<Instruction>;
}
