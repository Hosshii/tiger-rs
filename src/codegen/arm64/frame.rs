use std::{collections::HashMap, ops::Deref};

use once_cell::sync::Lazy;

use crate::{
    asm::Instruction,
    common::{Label, Temp},
    frame::Frame,
    ir::{BinOp, Expr, Stmt},
};

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

static REGISTERS: [&str; 34] = [
    "sp", "xzr", "pc", "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11",
    "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24",
    "x25", "x26", "x27", "x28", "x29", "x30",
];

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
    m.insert(REGISTERS_GLOBAL.sp, REGISTERS[0]);
    m.insert(REGISTERS_GLOBAL.xzr, REGISTERS[1]);
    m.insert(REGISTERS_GLOBAL.pc, REGISTERS[2]);
    m.insert(REGISTERS_GLOBAL.x0, REGISTERS[3]);
    m.insert(REGISTERS_GLOBAL.x1, REGISTERS[4]);
    m.insert(REGISTERS_GLOBAL.x2, REGISTERS[5]);
    m.insert(REGISTERS_GLOBAL.x3, REGISTERS[6]);
    m.insert(REGISTERS_GLOBAL.x4, REGISTERS[7]);
    m.insert(REGISTERS_GLOBAL.x5, REGISTERS[8]);
    m.insert(REGISTERS_GLOBAL.x6, REGISTERS[9]);
    m.insert(REGISTERS_GLOBAL.x7, REGISTERS[10]);
    m.insert(REGISTERS_GLOBAL.x8, REGISTERS[11]);
    m.insert(REGISTERS_GLOBAL.x9, REGISTERS[12]);
    m.insert(REGISTERS_GLOBAL.x10, REGISTERS[13]);
    m.insert(REGISTERS_GLOBAL.x11, REGISTERS[14]);
    m.insert(REGISTERS_GLOBAL.x12, REGISTERS[15]);
    m.insert(REGISTERS_GLOBAL.x13, REGISTERS[16]);
    m.insert(REGISTERS_GLOBAL.x14, REGISTERS[17]);
    m.insert(REGISTERS_GLOBAL.x15, REGISTERS[18]);
    m.insert(REGISTERS_GLOBAL.x16, REGISTERS[19]);
    m.insert(REGISTERS_GLOBAL.x17, REGISTERS[20]);
    m.insert(REGISTERS_GLOBAL.x18, REGISTERS[21]);
    m.insert(REGISTERS_GLOBAL.x19, REGISTERS[22]);
    m.insert(REGISTERS_GLOBAL.x20, REGISTERS[23]);
    m.insert(REGISTERS_GLOBAL.x21, REGISTERS[24]);
    m.insert(REGISTERS_GLOBAL.x22, REGISTERS[25]);
    m.insert(REGISTERS_GLOBAL.x23, REGISTERS[26]);
    m.insert(REGISTERS_GLOBAL.x24, REGISTERS[27]);
    m.insert(REGISTERS_GLOBAL.x25, REGISTERS[28]);
    m.insert(REGISTERS_GLOBAL.x26, REGISTERS[29]);
    m.insert(REGISTERS_GLOBAL.x27, REGISTERS[30]);
    m.insert(REGISTERS_GLOBAL.x28, REGISTERS[31]);
    m.insert(REGISTERS_GLOBAL.x29, REGISTERS[32]);
    m.insert(REGISTERS_GLOBAL.x30, REGISTERS[33]);
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

    pub fn debug_registers() {
        dbg!(&REGISTERS_GLOBAL);
    }
}

impl Frame for ARM64 {
    type Access = Access;
    type Register = &'static str; // TODO

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
            Access::InFrame(-self.pointer)
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

    fn registers() -> &'static [Self::Register] {
        REGISTERS.as_ref()
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

    fn temp_map() -> &'static HashMap<Temp, Self::Register> {
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

    fn proc_entry_exit1(&mut self, body: Stmt) -> Stmt {
        let arg_leg_len = Self::arg_regs().len();
        if self.formals.len() > arg_leg_len {
            unimplemented!("argument more than {} is not implemented", arg_leg_len - 1);
        }

        let mut start_stmts = Vec::new();
        let mut callee_save_regs_access = Vec::new();
        start_stmts.push(Stmt::Comment("proc entry exit 1 start".to_string()));

        // move callee_save_regs to mem
        for reg in Self::calee_save_regs() {
            let access = self.alloc_local(true);
            callee_save_regs_access.push(access.clone());
            start_stmts.push(Stmt::Move(
                Box::new(Self::exp(access, Expr::Temp(Self::fp()))),
                Box::new(Expr::Temp(*reg)),
            ));
        }

        for (formal, arg_reg) in self.formals().iter().zip(Self::arg_regs().iter()) {
            let dst = Self::exp(formal.clone(), Expr::Temp(Self::fp()));
            start_stmts.push(Stmt::Move(Box::new(dst), Box::new(Expr::Temp(*arg_reg))));
        }

        let mut end_stmts = Vec::new();

        for (reg, loc) in Self::calee_save_regs()
            .iter()
            .zip(callee_save_regs_access.iter())
        {
            end_stmts.push(Stmt::Move(
                Box::new(Expr::Temp(*reg)),
                Box::new(Self::exp(loc.clone(), Expr::Temp(Self::fp()))),
            ));
        }

        start_stmts.push(Stmt::Comment("body start".to_string()));
        start_stmts.push(body);
        start_stmts.push(Stmt::Comment("body end".to_string()));
        start_stmts.append(&mut end_stmts);
        start_stmts.push(Stmt::Comment("proc entry exit 1 end".to_string()));

        Stmt::seq(start_stmts)
    }

    fn proc_entry_exit2(&self, mut instructions: Vec<Instruction>) -> Vec<Instruction> {
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

        instructions
    }

    fn proc_entry_exit3(&self, mut instructions: Vec<Instruction>) -> Vec<Instruction> {
        let mut prologue = vec![
            Instruction::Comment {
                assembly: "// prologue start".to_string(),
            },
            Instruction::Label {
                assembly: format!("{}:", super::format_label(self.name())),
                label: self.name.clone(),
            },
            Instruction::Operand {
                assembly: format!("    sub 'd0, 's0, #{}", self.pointer),
                dst: vec![REGISTERS_GLOBAL.sp.into()],
                src: vec![REGISTERS_GLOBAL.sp.into()],
                jump: None,
            },
            Instruction::Comment {
                assembly: "// prologue end".to_string(),
            },
        ];

        let mut epilogue = vec![
            Instruction::Comment {
                assembly: "// epilogue start".to_string(),
            },
            Instruction::Operand {
                assembly: format!("    add 'd0, 's0, #{}", self.pointer),
                dst: vec![REGISTERS_GLOBAL.sp.into()],
                src: vec![REGISTERS_GLOBAL.sp.into()],
                jump: None,
            },
            Instruction::Operand {
                assembly: "    ret".to_string(),
                dst: vec![],
                src: vec![],
                jump: None,
            },
            Instruction::Comment {
                assembly: "// epilogue end".to_string(),
            },
        ];

        prologue.append(&mut instructions);
        prologue.append(&mut epilogue);

        prologue
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Access {
    InFrame(i64),
    InReg(Temp),
}

#[derive(Debug)]
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
