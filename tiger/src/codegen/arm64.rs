use crate::{
    frame::{Frame, ARM64 as ARM64Frame},
    ir::{BinOp, Expr, RelOp, Stmt},
};

use super::{
    asm::{Instruction, Temp},
    Codegen,
};

struct ARM64 {
    frame: ARM64Frame,
    instructions: Vec<Instruction>,
}

impl ARM64 {
    fn munch_args(&mut self, args: &[Expr]) -> Vec<Temp> {
        if args.len() > ARM64Frame::arg_regs().len() {
            unimplemented!("too many arguments");
        }

        let mut temps = Vec::new();
        for (reg, arg) in ARM64Frame::arg_regs().iter().zip(args.iter()) {
            let instruction = Instruction::Move {
                assembly: "mov 'd0, 's0".to_string(),
                dst: reg.into(),
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

            Stmt::Move(dst, src) => match dst.as_ref() {
                Expr::Temp(temp) => {
                    let instruction = Instruction::Move {
                        assembly: "mov 'd0, 's0".to_string(),
                        dst: Temp::from(temp),
                        src: self.munch_expr(src),
                    };
                    self.emit(instruction);
                }
                Expr::Mem(mem, size) => {
                    assert_eq!(*size, <ARM64Frame as Frame>::WORD_SIZE);

                    let instruction = Instruction::Move {
                        assembly: "str 's0, ['d0, #0]".to_string(),
                        dst: self.munch_expr(mem),
                        src: self.munch_expr(src),
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
                    dst: temp,
                    src: self.munch_expr(exp),
                };
                let instruction2 = Instruction::Operand {
                    assembly: "br 's0".to_string(),
                    dst: vec![],
                    src: vec![temp],
                    jump: Some(labels.clone()),
                };
                self.emit(instruction1);
                self.emit(instruction2);
            }
            Stmt::CJump(op, lhs, rhs, t, f) => {
                let instruction = Instruction::Operand {
                    assembly: "cmp 's0, 's1".to_string(),
                    dst: vec![],
                    src: vec![self.munch_expr(lhs), self.munch_expr(rhs)],
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
                    assembly: format!("b.{} {}", suffix, t),
                    dst: vec![],
                    src: vec![],
                    jump: Some(vec![t.clone(), f.clone()]),
                };
                self.emit(instruction);
            }
            Stmt::Label(label) => {
                let instruction = Instruction::Label {
                    assembly: format!("{}:", label),
                    label: label.clone(),
                };
                self.emit(instruction);
            }
        }
    }

    fn munch_expr(&mut self, expr: &Expr) -> Temp {
        let result = Temp::new();
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
                    dst: vec![result],
                    src: vec![],
                    jump: None,
                };
                self.emit(instruction);

                let instruction = Instruction::Operand {
                    assembly: format!("add 'd0, 'd0, :lo12:{}", label),
                    dst: vec![result],
                    src: vec![],
                    jump: None,
                };
                self.emit(instruction);
            }
            Expr::Temp(temp) => return Temp::from(temp),
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
                assert_eq!(*size, <ARM64Frame as Frame>::WORD_SIZE);

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

                let dst = ARM64Frame::call_defs().iter().map(Temp::from).collect();
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
                    src: Temp::from(ARM64Frame::rv()),
                };
                self.emit(instruction);
            }
        }

        result
    }

    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr)
    }
}

impl Codegen for ARM64 {
    type Frame = ARM64Frame;

    fn codegen(f: Self::Frame, stmt: Stmt) -> Vec<Instruction> {
        todo!()
    }
}
