use once_cell::sync::Lazy;

use crate::{
    common::{Label, Temp},
    ir::{BinOp, Expr, Stmt},
};

use super::Frame;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ARM64 {
    name: Label,
    formals: Vec<Access>,
    pointer: i64,
}

const PTR_SIZE: i64 = 8;
static REGISTERS_GLOBAL: Lazy<Registers> = Lazy::new(|| Registers {
    rbp: Temp::new(),
    rax: Temp::new(),
});

impl Frame for ARM64 {
    type Access = Access;

    const WORD_SIZE: u64 = 8;

    fn new(name: Label, formals: Vec<bool>) -> Self {
        let mut frame = Self {
            name,
            formals: vec![],
            pointer: 0,
        };
        let formals = formals.into_iter().map(|v| frame.alloc_local(v)).collect();
        frame.formals = formals;
        frame
    }

    fn name(&self) -> &Label {
        &self.name
    }

    fn formals(&self) -> &[Self::Access] {
        &self.formals
    }

    fn alloc_local(&mut self, is_escape: bool) -> Self::Access {
        if is_escape {
            self.pointer += PTR_SIZE;
            Access::InFrame(self.pointer)
        } else {
            Access::InReg(Temp::new())
        }
    }

    fn exp(access: Self::Access, stack_addr: Expr) -> Expr {
        match access {
            Access::InFrame(offset) => Expr::Mem(
                Box::new(Expr::BinOp(
                    BinOp::Plus,
                    Box::new(stack_addr),
                    Box::new(Expr::Const(offset)),
                )),
                Self::WORD_SIZE,
            ),
            Access::InReg(reg) => Expr::Temp(reg),
        }
    }

    fn fp() -> Temp {
        REGISTERS_GLOBAL.rbp
    }

    fn rv() -> Temp {
        REGISTERS_GLOBAL.rax
    }

    fn extern_call(_name: &str, _args: Vec<Expr>) -> Expr {
        // todo!("extern call")
        Expr::Const(0)
    }

    fn proc_entry_exit1(&mut self, _stmt: Stmt) -> Stmt {
        // todo!("proc_entry_exit1")
        Stmt::Expr(Box::new(Expr::Const(0)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Access {
    InFrame(i64),
    InReg(Temp),
}

struct Registers {
    rbp: Temp,
    rax: Temp,
}
