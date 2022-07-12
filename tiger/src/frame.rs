use std::{cell::RefCell, collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

use crate::{
    common::{Label, Temp},
    ir::{Expr, Stmt},
};

use super::asm::Instruction;

pub trait Frame: Clone {
    /// Represents an access to variable.
    /// Typically implemented like `enum {InReg(Temp), InFrame(offset)}`.
    type Access: Clone;
    type Register: Eq + Hash + Clone + Display + 'static;

    /// Machine specific word size.
    const WORD_SIZE: u64;

    /// All registers.
    fn registers() -> &'static [Self::Register];

    /// Represents special registers like fp, sp, lr, etc.
    fn special_regs() -> &'static [Temp];

    /// Represents argument registers.
    /// Must be in the same order as the arguments in the function.
    /// ex. `[x0, x1, x2, ..., x7]` for ARM64.
    /// https://developer.arm.com/documentation/102374/0100/Procedure-Call-Standard
    fn arg_regs() -> &'static [Temp];
    fn calee_save_regs() -> &'static [Temp];
    fn caller_save_regs() -> &'static [Temp];

    /// Maps temporaries to registers.
    /// All machine register is contained.
    fn temp_map() -> &'static HashMap<Temp, Self::Register>;

    /// Represents frame pointer.
    fn fp() -> Temp;
    /// Represents return value register.
    fn rv() -> Temp;

    /// Create new frame with given name and the escape information of arguments.
    fn new(name: Label, formals: Vec<bool>) -> Self;

    /// Return frame name
    fn name(&self) -> &Label;

    /// Return `Access` for formals.
    fn formals(&self) -> &[Self::Access];

    /// Allocate new local variable with given escape information.
    /// Returns the `Access` to the created variable.
    fn alloc_local(&mut self, is_escape: bool) -> Self::Access;

    /// Convert an `Access` to IR Tree.
    /// The second argument means frame pointer in which `Access` is allocated.
    fn exp(access: Self::Access, stack_addr: Expr) -> Expr;

    /// Call extern function which name is `name` with `args`.
    fn extern_call(name: &str, args: Vec<Expr>) -> Expr;

    /// Does view shift.
    fn proc_entry_exit1(&mut self, stmt: Stmt) -> Stmt;

    /// Add sink instruction.
    fn proc_entry_exit2(&self, instructions: Vec<Instruction>) -> Vec<Instruction>;

    /// Add prologue and epilogue.
    fn proc_entry_exit3(&self, instructions: Vec<Instruction>) -> Vec<Instruction>;
}

#[derive(Debug)]
pub enum Fragment<F: Frame> {
    Proc(Stmt, Rc<RefCell<F>>),
    String(Label, String),
}
