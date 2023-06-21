use std::{collections::HashMap, ops::Deref};

use once_cell::sync::Lazy;

use crate::{
    asm::Instruction,
    common::{Label, Temp},
    frame::Frame,
    ir::{BinOp, Expr, Stmt},
};

static REGISTERS_GLOBAL: Lazy<Registers> = Lazy::new(|| Registers {
    rbp: Temp::new(),
    rsp: Temp::new(),
    rax: Temp::new(),
    rbx: Temp::new(),
    rcx: Temp::new(),
    rdx: Temp::new(),
    rsi: Temp::new(),
    rdi: Temp::new(),
    r8: Temp::new(),
    r9: Temp::new(),
    r10: Temp::new(),
    r11: Temp::new(),
    r12: Temp::new(),
    r13: Temp::new(),
    r14: Temp::new(),
    r15: Temp::new(),
});

static REGISTERS_STR: [&str; 16] = [
    "rbp", "rsp", "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13",
    "r14", "r15",
];

static SPECIAL_REGS: Lazy<Vec<Temp>> = Lazy::new(|| {
    vec![
        REGISTERS_GLOBAL.rsp,
        REGISTERS_GLOBAL.rbp,
        REGISTERS_GLOBAL.rax,
    ]
});

static ARG_REGS: Lazy<Vec<Temp>> = Lazy::new(|| {
    vec![
        REGISTERS_GLOBAL.rdi,
        REGISTERS_GLOBAL.rsi,
        REGISTERS_GLOBAL.rdx,
        REGISTERS_GLOBAL.rcx,
        REGISTERS_GLOBAL.r8,
        REGISTERS_GLOBAL.r9,
    ]
});

static CALEE_SAVE_REGS: Lazy<Vec<Temp>> = Lazy::new(|| {
    vec![
        REGISTERS_GLOBAL.rbx,
        REGISTERS_GLOBAL.r12,
        REGISTERS_GLOBAL.r13,
        REGISTERS_GLOBAL.r14,
        REGISTERS_GLOBAL.r15,
    ]
});

static CALLER_SAVE_REGS: Lazy<Vec<Temp>> =
    Lazy::new(|| vec![REGISTERS_GLOBAL.r10, REGISTERS_GLOBAL.r11]);

static CALL_DEFS: Lazy<Vec<Temp>> = Lazy::new(|| {
    let mut v = CALLER_SAVE_REGS.clone();
    let mut arg_regs = ARG_REGS.clone();
    v.append(&mut arg_regs);
    // return register
    v.push(REGISTERS_GLOBAL.rax);
    v
});

static TEMP_MAP: Lazy<HashMap<Temp, &'static str>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(REGISTERS_GLOBAL.rbp, REGISTERS_STR[0]);
    m.insert(REGISTERS_GLOBAL.rsp, REGISTERS_STR[1]);
    m.insert(REGISTERS_GLOBAL.rax, REGISTERS_STR[2]);
    m.insert(REGISTERS_GLOBAL.rbx, REGISTERS_STR[3]);
    m.insert(REGISTERS_GLOBAL.rcx, REGISTERS_STR[4]);
    m.insert(REGISTERS_GLOBAL.rdx, REGISTERS_STR[5]);
    m.insert(REGISTERS_GLOBAL.rsi, REGISTERS_STR[6]);
    m.insert(REGISTERS_GLOBAL.rdi, REGISTERS_STR[7]);
    m.insert(REGISTERS_GLOBAL.r8, REGISTERS_STR[8]);
    m.insert(REGISTERS_GLOBAL.r9, REGISTERS_STR[9]);
    m.insert(REGISTERS_GLOBAL.r10, REGISTERS_STR[10]);
    m.insert(REGISTERS_GLOBAL.r11, REGISTERS_STR[11]);
    m.insert(REGISTERS_GLOBAL.r12, REGISTERS_STR[12]);
    m.insert(REGISTERS_GLOBAL.r13, REGISTERS_STR[13]);
    m.insert(REGISTERS_GLOBAL.r14, REGISTERS_STR[14]);
    m.insert(REGISTERS_GLOBAL.r15, REGISTERS_STR[15]);

    m
});

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86 {
    name: Label,
    formals: Vec<Access>,
    pointer: u64, // greater than 0
}

impl X86 {
    pub(super) fn call_defs() -> &'static [Temp] {
        CALL_DEFS.as_ref()
    }

    #[allow(unused)]
    pub(super) fn debug_registers() {
        dbg!(&REGISTERS_GLOBAL);
    }

    pub(super) fn sp() -> Temp {
        REGISTERS_GLOBAL.rsp
    }

    pub(super) fn rax() -> Temp {
        REGISTERS_GLOBAL.rax
    }

    pub(super) fn rdx() -> Temp {
        REGISTERS_GLOBAL.rdx
    }

    fn aligned_ptr(&self) -> u64 {
        (self.pointer + 16)
            .checked_sub(self.pointer % 16)
            .expect("ovefrlow")
    }
}

impl Frame for X86 {
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
            self.pointer += Self::WORD_SIZE;
            Access::InFrame(-(self.pointer as i64))
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
        REGISTERS_STR.as_ref()
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
        REGISTERS_GLOBAL.rbp
    }

    fn rv() -> Temp {
        REGISTERS_GLOBAL.rax
    }

    fn extern_call(name: &str, args: Vec<Expr>) -> Expr {
        Expr::Call(
            Box::new(Expr::Name(Label::with_named_fn(name.to_string()))),
            args,
        )
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
                assembly: super::format_label_stmt(self.name()),
                label: self.name.clone(),
            },
            Instruction::Operand {
                assembly: "    push 's0".to_string(),
                dst: vec![],
                src: vec![Self::fp().into()],
                jump: None,
            },
            Instruction::Operand {
                assembly: "    mov 'd0, 's0".to_string(),
                dst: vec![Self::fp().into()],
                src: vec![Self::sp().into()],
                jump: None,
            },
            Instruction::Operand {
                assembly: format!("    sub 'd0, {}", self.aligned_ptr()),
                dst: vec![Self::sp().into()],
                src: vec![Self::sp().into()],
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
                assembly: "    mov 'd0, 's0".to_string(),
                dst: vec![Self::sp().into()],
                src: vec![Self::fp().into()],
                jump: None,
            },
            Instruction::Operand {
                assembly: "    pop 's0".to_string(),
                dst: vec![],
                src: vec![Self::fp().into()],
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
    rbp: Temp,
    rsp: Temp,
    // pc: Temp,
    rax: Temp,
    rbx: Temp,
    rcx: Temp,
    rdx: Temp,
    rsi: Temp,
    rdi: Temp,
    r8: Temp,
    r9: Temp,
    r10: Temp,
    r11: Temp,
    r12: Temp,
    r13: Temp,
    r14: Temp,
    r15: Temp,
}
