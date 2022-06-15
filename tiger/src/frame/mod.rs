mod x86;

use crate::{
    ir::Expr,
    temp::{Label, Temp},
};
pub use x86::X86;

pub trait Frame: Clone {
    type Access: Clone;

    const WORD_SIZE: u64;

    fn fp() -> Temp;

    fn new(name: Label, formals: Vec<bool>) -> Self;
    fn name(&self) -> &Label;
    fn formals(&self) -> &[Self::Access];
    fn alloc_local(&mut self, is_escape: bool) -> Self::Access;

    fn exp(access: Self::Access, stack_addr: Expr) -> Expr;
}
