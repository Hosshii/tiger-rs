pub mod frame;

use crate::{
    asm::{Instruction, Temp},
    common::Label,
    ir::{BinOp, Expr, RelOp, Stmt},
};
use frame::ARM64 as ARM64Frame;

use super::{Codegen, Frame as _};

pub struct ARM64 {
    instructions: Vec<Instruction>,
}

impl ARM64 {
    pub fn debug() {
        ARM64Frame::debug_registers()
    }

    fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    // TODO: use efficient algorithm (dp).
    fn munch_args(&mut self, args: &[Expr]) -> Vec<Temp> {
        if args.len() > ARM64Frame::arg_regs().len() {
            unimplemented!("too many arguments");
        }

        let mut temps = Vec::new();
        for (reg, arg) in ARM64Frame::arg_regs().iter().zip(args.iter()) {
            let instruction = Instruction::Move {
                assembly: "    mov 'd0, 's0".to_string(),
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
            Stmt::Comment(comment) => {
                let instruction = Instruction::Comment {
                    assembly: format!("// {}", comment),
                };
                self.emit(instruction);
            }

            Stmt::Move(dst, src) => match dst.as_ref() {
                Expr::Temp(temp) => {
                    let instruction = Instruction::Move {
                        assembly: "    mov 'd0, 's0".to_string(),
                        dst: Temp::from(temp),
                        src: self.munch_expr(src),
                    };
                    self.emit(instruction);
                }
                Expr::Mem(mem, size) => {
                    assert_eq!(*size, ARM64Frame::WORD_SIZE);

                    let instruction = Instruction::Operand {
                        assembly: "    str 's0, ['s1, #0]".to_string(),
                        dst: vec![],
                        src: vec![self.munch_expr(src), self.munch_expr(mem)],
                        jump: None,
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
                    assembly: "    mov 'd0, 's0".to_string(),
                    dst: temp,
                    src: self.munch_expr(exp),
                };
                let instruction2 = Instruction::Operand {
                    assembly: "    br 's0".to_string(),
                    dst: vec![],
                    src: vec![temp],
                    jump: Some(labels.clone()),
                };
                self.emit(instruction1);
                self.emit(instruction2);
            }
            Stmt::CJump(op, lhs, rhs, t, f) => {
                let instruction = Instruction::Operand {
                    assembly: "    cmp 's0, 's1".to_string(),
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
                    assembly: format!("    b.{} {}", suffix, format_label(t)),
                    dst: vec![],
                    src: vec![],
                    jump: Some(vec![t.clone(), f.clone()]),
                };
                self.emit(instruction);
            }
            Stmt::Label(label) => {
                let assembly = format_label_stmt(label);
                let instruction = Instruction::Label {
                    assembly,
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
                    assembly: format!("    mov 'd0, {}", val),
                    dst: vec![result],
                    src: vec![],
                    jump: None,
                };
                self.emit(instruction);
            }
            Expr::Name(label) => {
                let instruction = Instruction::Operand {
                    assembly: format!("    adrp 'd0, {}@PAGE", format_label(label)),
                    dst: vec![result],
                    src: vec![],
                    jump: None,
                };
                self.emit(instruction);

                let instruction = Instruction::Operand {
                    assembly: format!("    add 'd0, 'd0, {}@PAGEOFF", format_label(label)),
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
                    assembly: format!("    {} 'd0, 's0, 's1", opcode),
                    dst: vec![result],
                    src: vec![self.munch_expr(lhs), self.munch_expr(rhs)],
                    jump: None,
                };
                self.emit(instruction);
            }
            Expr::Mem(expr, size) => {
                assert_eq!(*size, ARM64Frame::WORD_SIZE);

                let instruction = Instruction::Operand {
                    assembly: "    ldr 'd0, ['s0, #0]".to_string(),
                    dst: vec![result],
                    src: vec![self.munch_expr(expr)],
                    jump: None,
                };
                self.emit(instruction);
            }
            Expr::Call(name, args) if matches!(name.as_ref(), Expr::Name(_)) => {
                let src = self.munch_args(args);
                let name = match name.as_ref() {
                    Expr::Name(name) => name,
                    _ => unreachable!(),
                };

                let dst = ARM64Frame::call_defs().iter().map(Temp::from).collect();
                let instruction = Instruction::Operand {
                    assembly: format!("    bl {}", format_label(name)),
                    dst,
                    src,
                    jump: None,
                };
                self.emit(instruction);

                let instruction = Instruction::Move {
                    assembly: "    mov 'd0, 's0".to_string(),
                    dst: result,
                    src: Temp::from(ARM64Frame::rv()),
                };
                self.emit(instruction);
            }

            Expr::Call(name, args) => {
                let mut src = vec![self.munch_expr(name)];
                src.append(&mut self.munch_args(args));

                let dst = ARM64Frame::call_defs().iter().map(Temp::from).collect();
                let instruction = Instruction::Operand {
                    assembly: "    blr 's0".to_string(),
                    dst,
                    src,
                    jump: None,
                };
                self.emit(instruction);

                let instruction = Instruction::Move {
                    assembly: "    mov 'd0, 's0".to_string(),
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
    const MAIN_SYMBOL: &'static str = "main";

    fn codegen(_: &Self::Frame, stmt: Stmt) -> Vec<Instruction> {
        let mut codegen = ARM64::new();
        codegen.munch_stmt(&stmt);
        codegen.instructions
    }

    fn string(label: &Label, s: &str) -> String {
        let label = format_label(label);

        format!(
            r##"    .section    __TEXT,__const
{}.STR:
    .ascii "{}"

    .section    __DATA,__data
    .p2align 3
{}:
    .xword {}
    .xword {}.STR

    .section	__TEXT,__text,regular,pure_instructions"##,
            label,
            s,
            label,
            s.len(),
            label
        )
    }

    fn header() -> String {
        String::new()
    }
}

fn format_label(label: &Label) -> String {
    match label {
        Label::Num(_) | Label::Fn(_, _) => {
            format!("L.{}", label)
        }
        Label::NamedFn(s) => {
            // Calling function named `exit` is not working correctry.
            // So rename it to `tiger_exit`.
            if s == "exit" {
                "_tiger_exit".to_string()
            } else {
                format!("_{}", label)
            }
        }
    }
}

/// label which is address
/// that is used like
///
/// LABEL:
///
fn format_label_stmt(label: &Label) -> String {
    match label {
        Label::Num(_) => {
            format!("{}:", format_label(label))
        }
        Label::Fn(_, _) => {
            format!("    .p2align 2\n{}:", format_label(label))
        }
        Label::NamedFn(_) => format!(
            "    .globl {}\n    .p2align 2\n{}:",
            format_label(label),
            format_label(label)
        ),
    }
}

#[cfg(test)]
mod tests {
    use crate::common::Temp as CommonTemp;

    use super::*;

    #[test]
    fn test_move_to_mem() {
        let cases = vec![
            (
                Stmt::Move(
                    Box::new(Expr::Temp(CommonTemp::new_with(0))),
                    Box::new(Expr::Temp(CommonTemp::new_with(1))),
                ),
                vec![Temp::new_with(0)],
                vec![Temp::new_with(1)],
            ),
            // if move to memory, then either regs should be src, and not go to dst.
            (
                Stmt::Move(
                    Box::new(Expr::Mem(Box::new(Expr::Temp(CommonTemp::new_with(0))), 8)),
                    Box::new(Expr::Temp(CommonTemp::new_with(1))),
                ),
                vec![],
                vec![Temp::new_with(1), Temp::new_with(0)],
            ),
        ];

        for (stmt, expected_dst, expected_src) in cases {
            let mut codegen = ARM64::new();
            codegen.munch_stmt(&stmt);
            let instructions = codegen.instructions;
            for instruction in instructions {
                match instruction {
                    Instruction::Operand { dst, src, .. } => {
                        assert_eq!(expected_dst, dst);
                        assert_eq!(expected_src, src);
                    }
                    Instruction::Move { dst, src, .. } => {
                        assert_eq!(expected_dst, vec![dst]);
                        assert_eq!(expected_src, vec![src]);
                    }
                    _ => panic!(),
                }
            }
        }
    }
}
