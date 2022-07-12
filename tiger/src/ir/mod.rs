mod canon;

pub use canon::{basic_blocks, linearize, trace_schedule};

use crate::{
    common::{Label, Temp},
    parser::ast::Operator,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Const(i64), // TODO: maybe u64 is better
    Name(Label),
    Temp(Temp),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Mem(Box<Expr>, u64),
    Call(Box<Expr>, Vec<Expr>),
    // maybe unnecessary box for stmt
    ESeq(Box<Stmt>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    Move(Box<Expr>, Box<Expr>),
    Expr(Box<Expr>),
    Jump(Box<Expr>, Vec<Label>),
    CJump(RelOp, Box<Expr>, Box<Expr>, Label, Label),
    Seq(Box<Stmt>, Box<Stmt>),
    Label(Label),
}

impl Stmt {
    pub fn seq(stmts: Vec<Stmt>) -> Stmt {
        if stmts.is_empty() {
            Stmt::nop()
        } else if stmts.len() == 1 {
            stmts.into_iter().next().unwrap()
        } else {
            stmts
                .into_iter()
                .reduce(|accum, item| {
                    let lhs = Box::new(accum);
                    let rhs = Box::new(item);
                    Stmt::Seq(lhs, rhs)
                })
                .unwrap()
        }
    }

    pub fn nop() -> Stmt {
        Stmt::Expr(Box::new(Expr::Const(0)))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    LShift,
    RShift,
    ARShift,
    XOr,
}

impl TryFrom<Operator> for BinOp {
    type Error = ();

    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Plus => Ok(Self::Plus),
            Operator::Minus => Ok(Self::Minus),
            Operator::Mul => Ok(Self::Mul),
            Operator::Div => Ok(Self::Div),
            Operator::And => Ok(Self::And),
            Operator::Or => Ok(Self::Or),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RelOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

impl TryFrom<Operator> for RelOp {
    type Error = ();

    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Eq => Ok(RelOp::Eq),
            Operator::Neq => Ok(RelOp::Ne),
            Operator::Ge => Ok(RelOp::Ge),
            Operator::Gt => Ok(RelOp::Gt),
            Operator::Le => Ok(RelOp::Le),
            Operator::Lt => Ok(RelOp::Lt),
            _ => Err(()),
        }
    }
}
