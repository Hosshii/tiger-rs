use crate::{asm::Instruction, common::Label, frame::Frame, ir::Stmt};

pub mod aarch64_apple_darwin;
pub mod x86_64_apple_darwin;
pub mod x86_64_linux_gnu;

pub(super) mod color;
pub(super) mod flow;
mod graph;
pub(super) mod liveness;
pub mod reg_alloc;

pub trait Codegen: sealed::Sealed {
    type Frame: Frame;
    const MAIN_SYMBOL: &'static str;

    fn codegen(frame: &Self::Frame, stmt: Stmt) -> Vec<Instruction>;

    /// Convert string literal to asm.
    fn string(label: &Label, s: &str) -> String;

    fn header() -> String;
}

mod sealed {
    pub trait Sealed {}
}
