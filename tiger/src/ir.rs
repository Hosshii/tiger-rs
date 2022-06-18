use crate::temp::{Label, Temp};

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Move(Box<Expr>, Box<Expr>),
    Expr(Box<Expr>),
    Jump(Box<Expr>, Vec<Label>),
    CJump(RelOp, Box<Expr>, Box<Expr>, Label, Label),
    Seq(Box<Stmt>, Box<Stmt>),
    Label(Label),
}

impl Stmt {
    pub fn seq(first: Stmt, second: Stmt, mut remain: Vec<Stmt>) -> Stmt {
        let mut v = Vec::with_capacity(remain.len() + 2);
        v.push(first);
        v.push(second);
        v.append(&mut remain);
        v.into_iter()
            .reduce(|accum, item| {
                let lhs = Box::new(accum);
                let rhs = Box::new(item);
                Stmt::Seq(lhs, rhs)
            })
            .unwrap()
    }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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
