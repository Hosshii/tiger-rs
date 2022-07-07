use std::{collections::HashMap, ops::Deref};

use once_cell::sync::Lazy;

use crate::{
    codegen::asm::Instruction,
    common::{Label, Temp},
    ir::{BinOp, Expr, Stmt},
};

use super::Frame;

const PTR_SIZE: i64 = 8;
static REGISTERS_GLOBAL: Lazy<Registers> = Lazy::new(|| Registers {
    sp: Temp::new(),
    xzr: Temp::new(),
    pc: Temp::new(),
    x0: Temp::new(),
    x1: Temp::new(),
    x2: Temp::new(),
    x3: Temp::new(),
    x4: Temp::new(),
    x5: Temp::new(),
    x6: Temp::new(),
    x7: Temp::new(),
    x8: Temp::new(),
    x9: Temp::new(),
    x10: Temp::new(),
    x11: Temp::new(),
    x12: Temp::new(),
    x13: Temp::new(),
    x14: Temp::new(),
    x15: Temp::new(),
    x16: Temp::new(),
    x17: Temp::new(),
    x18: Temp::new(),
    x19: Temp::new(),
    x20: Temp::new(),
    x21: Temp::new(),
    x22: Temp::new(),
    x23: Temp::new(),
    x24: Temp::new(),
    x25: Temp::new(),
    x26: Temp::new(),
    x27: Temp::new(),
    x28: Temp::new(),
    x29: Temp::new(),
    x30: Temp::new(),
});

static SPECIAL_REGS: Lazy<Vec<Temp>> = Lazy::new(|| {
    vec![
        REGISTERS_GLOBAL.sp,
        REGISTERS_GLOBAL.xzr,
        REGISTERS_GLOBAL.pc,
        REGISTERS_GLOBAL.x0, // TODO: Duplicate with ARG_REGS.
        REGISTERS_GLOBAL.x29,
        REGISTERS_GLOBAL.x30,
    ]
});

static ARG_REGS: Lazy<Vec<Temp>> = Lazy::new(|| {
    vec![
        REGISTERS_GLOBAL.x0,
        REGISTERS_GLOBAL.x1,
        REGISTERS_GLOBAL.x2,
        REGISTERS_GLOBAL.x3,
        REGISTERS_GLOBAL.x4,
        REGISTERS_GLOBAL.x5,
        REGISTERS_GLOBAL.x6,
        REGISTERS_GLOBAL.x7,
    ]
});

static CALEE_SAVE_REGS: Lazy<Vec<Temp>> = Lazy::new(|| {
    vec![
        REGISTERS_GLOBAL.x8,
        REGISTERS_GLOBAL.x16,
        REGISTERS_GLOBAL.x17,
        REGISTERS_GLOBAL.x18,
        REGISTERS_GLOBAL.x19,
        REGISTERS_GLOBAL.x20,
        REGISTERS_GLOBAL.x21,
        REGISTERS_GLOBAL.x22,
        REGISTERS_GLOBAL.x23,
        REGISTERS_GLOBAL.x24,
        REGISTERS_GLOBAL.x25,
        REGISTERS_GLOBAL.x26,
        REGISTERS_GLOBAL.x27,
        REGISTERS_GLOBAL.x28,
    ]
});

static CALLER_SAVE_REGS: Lazy<Vec<Temp>> = Lazy::new(|| {
    vec![
        REGISTERS_GLOBAL.x9,
        REGISTERS_GLOBAL.x10,
        REGISTERS_GLOBAL.x11,
        REGISTERS_GLOBAL.x12,
        REGISTERS_GLOBAL.x13,
        REGISTERS_GLOBAL.x14,
        REGISTERS_GLOBAL.x15,
    ]
});

static CALL_DEFS: Lazy<Vec<Temp>> = Lazy::new(|| {
    let mut v = CALLER_SAVE_REGS.clone();
    let mut arg_regs = ARG_REGS.clone();
    v.append(&mut arg_regs);
    // return register
    v.push(REGISTERS_GLOBAL.x29);
    v
});

static TEMP_MAP: Lazy<HashMap<Temp, &'static str>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(REGISTERS_GLOBAL.sp, "sp");
    m.insert(REGISTERS_GLOBAL.xzr, "xzr");
    m.insert(REGISTERS_GLOBAL.pc, "pc");
    m.insert(REGISTERS_GLOBAL.x0, "x0");
    m.insert(REGISTERS_GLOBAL.x1, "x1");
    m.insert(REGISTERS_GLOBAL.x2, "x2");
    m.insert(REGISTERS_GLOBAL.x3, "x3");
    m.insert(REGISTERS_GLOBAL.x4, "x4");
    m.insert(REGISTERS_GLOBAL.x5, "x5");
    m.insert(REGISTERS_GLOBAL.x6, "x6");
    m.insert(REGISTERS_GLOBAL.x7, "x7");
    m.insert(REGISTERS_GLOBAL.x8, "x8");
    m.insert(REGISTERS_GLOBAL.x9, "x9");
    m.insert(REGISTERS_GLOBAL.x10, "x10");
    m.insert(REGISTERS_GLOBAL.x11, "x11");
    m.insert(REGISTERS_GLOBAL.x12, "x12");
    m.insert(REGISTERS_GLOBAL.x13, "x13");
    m.insert(REGISTERS_GLOBAL.x14, "x14");
    m.insert(REGISTERS_GLOBAL.x15, "x15");
    m.insert(REGISTERS_GLOBAL.x16, "x16");
    m.insert(REGISTERS_GLOBAL.x17, "x17");
    m.insert(REGISTERS_GLOBAL.x18, "x18");
    m.insert(REGISTERS_GLOBAL.x19, "x19");
    m.insert(REGISTERS_GLOBAL.x20, "x20");
    m.insert(REGISTERS_GLOBAL.x21, "x21");
    m.insert(REGISTERS_GLOBAL.x22, "x22");
    m.insert(REGISTERS_GLOBAL.x23, "x23");
    m.insert(REGISTERS_GLOBAL.x24, "x24");
    m.insert(REGISTERS_GLOBAL.x25, "x25");
    m.insert(REGISTERS_GLOBAL.x26, "x26");
    m.insert(REGISTERS_GLOBAL.x27, "x27");
    m.insert(REGISTERS_GLOBAL.x28, "x28");
    m.insert(REGISTERS_GLOBAL.x29, "x29");
    m.insert(REGISTERS_GLOBAL.x30, "x30");
    m
});

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ARM64 {
    name: Label,
    formals: Vec<Access>,
    pointer: i64,
}

impl ARM64 {
    pub fn call_defs() -> &'static [Temp] {
        CALL_DEFS.as_ref()
    }
}

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

    fn special_regs() -> &'static [Temp] {
        SPECIAL_REGS.as_ref()
    }

    fn arg_regs() -> &'static [Temp] {
        ARG_REGS.as_ref()
    }

    fn calee_save_regs() -> &'static [Temp] {
        CALEE_SAVE_REGS.as_ref()
    }

    fn caller_save_regs() -> &'static [Temp] {
        CALLER_SAVE_REGS.as_ref()
    }

    fn temp_map() -> &'static HashMap<Temp, &'static str> {
        TEMP_MAP.deref()
    }

    fn fp() -> Temp {
        REGISTERS_GLOBAL.x29
    }

    fn rv() -> Temp {
        REGISTERS_GLOBAL.x0
    }

    fn extern_call(_name: &str, _args: Vec<Expr>) -> Expr {
        // todo!("extern call")
        Expr::Const(0)
    }

    fn proc_entry_exit1(&mut self, _stmt: Stmt) -> Stmt {
        // todo!("proc_entry_exit1")
        Stmt::Expr(Box::new(Expr::Const(0)))
    }

    fn proc_entry_exit2(&self, instructions: &mut Vec<Instruction>) {
        let mut src: Vec<_> = Self::calee_save_regs().iter().map(Into::into).collect();
        let mut special_regs = Self::special_regs().iter().map(Into::into).collect();
        src.append(&mut special_regs);
        let instruction = Instruction::Operand {
            assembly: String::new(),
            dst: vec![],
            src,
            jump: None,
        };
        instructions.push(instruction);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Access {
    InFrame(i64),
    InReg(Temp),
}

struct Registers {
    sp: Temp,
    xzr: Temp,
    pc: Temp,
    x0: Temp,
    x1: Temp,
    x2: Temp,
    x3: Temp,
    x4: Temp,
    x5: Temp,
    x6: Temp,
    x7: Temp,
    x8: Temp,
    x9: Temp,
    x10: Temp,
    x11: Temp,
    x12: Temp,
    x13: Temp,
    x14: Temp,
    x15: Temp,
    x16: Temp,
    x17: Temp,
    x18: Temp,
    x19: Temp,
    x20: Temp,
    x21: Temp,
    x22: Temp,
    x23: Temp,
    x24: Temp,
    x25: Temp,
    x26: Temp,
    x27: Temp,
    x28: Temp,
    x29: Temp,
    x30: Temp,
}
