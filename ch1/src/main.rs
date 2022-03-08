use std::cmp;

use self::{BinOp::*, Expr::*, Stmt::*};

fn main() {
    let prog = Compound(
        Box::new(Assign(
            "a".to_string(),
            Box::new(Op(Box::new(Num(5)), Plus, Box::new(Num(5)))),
        )),
        Box::new(Compound(
            Box::new(Assign(
                "b".to_string(),
                Box::new(Eseq(
                    Box::new(Print(vec![
                        Id("a".to_string()),
                        Op(Box::new(Id("a".to_string())), Plus, Box::new(Num(1))),
                    ])),
                    Box::new(Op(Box::new(Num(10)), Plus, Box::new(Id("a".to_string())))),
                )),
            )),
            Box::new(Print(vec![Id("b".to_string())])),
        )),
    );

    println!("{:?}", max_args(&prog))
}

fn max_args(s: &Stmt) -> Option<usize> {
    match s {
        Compound(lhs, rhs) => cmp::max(max_args(lhs), max_args(rhs)),
        Assign(id, exp) => max_args_expr(exp),
        Print(exprs) => Some(exprs.len()),
    }
}

fn max_args_expr(e: &Expr) -> Option<usize> {
    match e {
        Id(_) | Num(_) => None,
        Op(lhs, _, rhs) => cmp::max(max_args_expr(&*lhs), max_args_expr(&*rhs)),
        Eseq(stmt, expr) => cmp::max(max_args(&*stmt), max_args_expr(&*expr)),
    }
}

type Id = String;

enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
}

enum Stmt {
    Compound(Box<Stmt>, Box<Stmt>),
    Assign(Id, Box<Expr>),
    Print(Vec<Expr>),
}

enum Expr {
    Id(Id),
    Num(i32),
    Op(Box<Expr>, BinOp, Box<Expr>),
    Eseq(Box<Stmt>, Box<Expr>),
}
