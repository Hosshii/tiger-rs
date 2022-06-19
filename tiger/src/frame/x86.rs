use once_cell::sync::Lazy;

use crate::{
    ir::{BinOp, Expr},
    temp::{Label, Temp},
};

use super::Frame;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86 {
    name: Label,
    formals: Vec<Access>,
    pointer: i64,
}

const PTR_SIZE: i64 = 8;
static REGISTERS_GLOBAL: Lazy<Registers> = Lazy::new(|| Registers { rbp: Temp::new() });

impl Frame for X86 {
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

    fn extern_call(name: &str, args: Vec<Expr>) -> Expr {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Access {
    InFrame(i64),
    InReg(Temp),
}

struct Registers {
    rbp: Temp,
}
