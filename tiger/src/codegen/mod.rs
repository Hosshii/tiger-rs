use self::asm::Instruction;
use crate::{frame::Frame, ir::Stmt};

mod arm64;
mod asm;

trait Codegen {
    type Frame: Frame;

    fn codegen(f: Self::Frame, stmt: Stmt) -> Vec<Instruction>;
}
