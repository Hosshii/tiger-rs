pub mod frame;

use crate::{
    asm::{Instruction, LabelTrait, TempTrait},
    common::{Label, Temp},
    ir::{BinOp, Expr, RelOp, Stmt},
};
use frame::ARM64 as ARM64Frame;

use super::{Codegen, Frame as _};

#[derive(Hash, Clone, Debug, PartialEq, Eq, Copy)]
pub struct ARM64Temp(Temp);

impl ARM64Temp {
    pub fn new() -> Self {
        ARM64Temp(Temp::new())
    }
    pub fn with(num: u32) -> Self {
        ARM64Temp(Temp::new_with(num))
    }
}

impl TempTrait for ARM64Temp {}

impl From<Temp> for ARM64Temp {
    fn from(tmp: Temp) -> Self {
        ARM64Temp(tmp)
    }
}

impl<'a> From<&'a Temp> for ARM64Temp {
    fn from(tmp: &'a Temp) -> Self {
        ARM64Temp(*tmp)
    }
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
pub struct ARM64Label(Label);

impl ARM64Label {
    pub fn new() -> Self {
        ARM64Label(Label::new())
    }
    pub fn with_num(num: u32) -> Self {
        ARM64Label(Label::with_num(num))
    }
}

impl LabelTrait for ARM64Label {}

impl From<Label> for ARM64Label {
    fn from(label: Label) -> Self {
        ARM64Label(label)
    }
}

impl<'a> From<&'a Label> for ARM64Label {
    fn from(label: &'a Label) -> Self {
        ARM64Label(label.clone())
    }
}

pub struct ARM64<'a> {
    _frame: &'a ARM64Frame,
    instructions: Vec<Instruction<ARM64Temp, ARM64Label>>,
}

impl<'a> ARM64<'a> {
    pub fn debug() {
        ARM64Frame::debug_registers()
    }

    fn new(frame: &'a ARM64Frame) -> Self {
        Self {
            _frame: frame,
            instructions: Vec::new(),
        }
    }

    // TODO: use efficient algorithm (dp).
    fn munch_args(&mut self, args: &[Expr]) -> Vec<ARM64Temp> {
        if args.len() > ARM64Frame::arg_regs().len() {
            unimplemented!("too many arguments");
        }

        let mut temps = Vec::new();
        for (reg, arg) in ARM64Frame::arg_regs().iter().zip(args.iter()) {
            let instruction = Instruction::Move {
                assembly: "mov 'd0, 's0".to_string(),
                dst: ARM64Temp::from(reg),
                src: self.munch_expr(arg),
            };
            self.emit(instruction);
            temps.push(reg.into());
        }

        temps
    }

    fn munch_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Seq(_, _) => unreachable!("canonical tree may not have this node. {:#?}", stmt),
            Stmt::Comment(comment) => {
                let instruction = Instruction::Comment {
                    assembly: format!("@ {}", comment),
                };
                self.emit(instruction);
            }

            Stmt::Move(dst, src) => match dst.as_ref() {
                Expr::Temp(temp) => {
                    let instruction = Instruction::Move {
                        assembly: "mov 'd0, 's0".to_string(),
                        dst: temp.into(),
                        src: self.munch_expr(src).into(),
                    };
                    self.emit(instruction);
                }
                Expr::Mem(mem, size) => {
                    assert_eq!(*size, ARM64Frame::WORD_SIZE);

                    let instruction = Instruction::Move {
                        assembly: "str 's0, ['d0, #0]".to_string(),
                        dst: self.munch_expr(mem).into(),
                        src: self.munch_expr(src).into(),
                    };

                    self.emit(instruction);
                }
                _ => unreachable!("dst of move have to be temp or mem."),
            },
            Stmt::Expr(expr) => {
                self.munch_expr(expr);
            }
            Stmt::Jump(exp, labels) => {
                let temp = Temp::new();
                let instruction1 = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    dst: temp.into(),
                    src: self.munch_expr(exp).into(),
                };
                let instruction2 = Instruction::Operand {
                    assembly: "br 's0".to_string(),
                    dst: vec![],
                    src: vec![temp.into()],
                    jump: Some(labels.iter().map(|v| v.into()).collect()),
                };
                self.emit(instruction1);
                self.emit(instruction2);
            }
            Stmt::CJump(op, lhs, rhs, t, f) => {
                let instruction = Instruction::Operand {
                    assembly: "cmp 's0, 's1".to_string(),
                    dst: vec![],
                    src: vec![self.munch_expr(lhs).into(), self.munch_expr(rhs).into()],
                    jump: None,
                };
                self.emit(instruction);

                let suffix = match op {
                    RelOp::Eq => "eq",
                    RelOp::Ne => "ne",
                    RelOp::Lt => "lt",
                    RelOp::Le => "le",
                    RelOp::Gt => "gt",
                    RelOp::Ge => "ge",
                    RelOp::Ult => "cc",
                    RelOp::Ule => "ls",
                    RelOp::Ugt => "hi",
                    RelOp::Uge => "cs",
                };
                let instruction = Instruction::Operand {
                    assembly: format!("b.{} L.{}", suffix, t),
                    dst: vec![],
                    src: vec![],
                    jump: Some(vec![t.into(), f.into()]),
                };
                self.emit(instruction);
            }
            Stmt::Label(label) => {
                let instruction = Instruction::Label {
                    assembly: format!("L.{}:", label),
                    label: label.into(),
                };
                self.emit(instruction);
            }
        }
    }

    fn munch_expr(&mut self, expr: &Expr) -> ARM64Temp {
        let result = Temp::new().into();
        match expr {
            Expr::ESeq(_, _) => unreachable!("canonical tree may not have this node. {:#?}", expr),

            Expr::Const(val) => {
                let instruction = Instruction::Operand {
                    assembly: format!("mov 'd0, {}", val),
                    dst: vec![result],
                    src: vec![],
                    jump: None,
                };
                self.emit(instruction);
            }
            Expr::Name(label) => {
                let instruction = Instruction::Operand {
                    assembly: format!("adrp 'd0, {}", label),
                    dst: vec![result.into()],
                    src: vec![],
                    jump: None,
                };
                self.emit(instruction);

                let instruction = Instruction::Operand {
                    assembly: format!("add 'd0, 'd0, :lo12:L.{}", label),
                    dst: vec![result.into()],
                    src: vec![],
                    jump: None,
                };
                self.emit(instruction);
            }
            Expr::Temp(temp) => return ARM64Temp::from(temp),
            Expr::BinOp(op, lhs, rhs) => {
                let opcode = match op {
                    BinOp::Plus => "add",
                    BinOp::Minus => "sub",
                    BinOp::Mul => "mul",
                    BinOp::Div => "sdiv",
                    BinOp::And => "and",
                    BinOp::Or => "orr",
                    BinOp::LShift => "lsl",
                    BinOp::RShift => "lsr",
                    BinOp::ARShift => "asr",
                    BinOp::XOr => "eor",
                };
                let instruction = Instruction::Operand {
                    assembly: format!("{} 'd0, 's0, 's1", opcode),
                    dst: vec![result],
                    src: vec![self.munch_expr(lhs), self.munch_expr(rhs)],
                    jump: None,
                };
                self.emit(instruction);
            }
            Expr::Mem(expr, size) => {
                assert_eq!(*size, ARM64Frame::WORD_SIZE);

                let instruction = Instruction::Operand {
                    assembly: "ldr 'd0, ['s0, #0]".to_string(),
                    dst: vec![result],
                    src: vec![self.munch_expr(expr)],
                    jump: None,
                };
                self.emit(instruction);
            }
            Expr::Call(name, args) => {
                let mut src = vec![self.munch_expr(name)];
                src.append(&mut self.munch_args(args));

                let dst = ARM64Frame::call_defs().iter().map(Into::into).collect();
                let instruction = Instruction::Operand {
                    assembly: "blr 's0".to_string(),
                    dst,
                    src,
                    jump: None,
                };
                self.emit(instruction);

                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    dst: result,
                    src: ARM64Temp::from(ARM64Frame::rv()),
                };
                self.emit(instruction);
            }
        }

        result
    }

    fn emit(&mut self, instr: Instruction<ARM64Temp, ARM64Label>) {
        self.instructions.push(instr)
    }
}

impl<'a> Codegen for ARM64<'a> {
    type Frame = ARM64Frame;
    const MAIN_SYMBOL: &'static str = "main";

    fn codegen(frame: &Self::Frame, stmt: Stmt) -> Vec<Instruction<ARM64Temp, ARM64Label>> {
        let mut codegen = ARM64::new(frame);
        codegen.munch_stmt(&stmt);
        codegen.instructions
    }
}
