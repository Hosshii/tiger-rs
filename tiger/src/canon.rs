use std::{collections::VecDeque, ops::Deref};

use crate::{
    ir::{Expr, Stmt},
    temp::{Label, Temp},
};

pub fn linearize(stmt: Stmt) -> Vec<Stmt> {
    fn commute(expr: &Expr, stmt: &Stmt) -> bool {
        matches!((expr, stmt), (Expr::Const(_), _) | (Expr::Name(_), _))
    }

    fn reorder1(expr: Expr) -> (Stmt, Expr) {
        do_expr(expr)
    }

    fn reorder2(a: Expr, b: Expr) -> (Stmt, (Expr, Expr)) {
        let (stmt, exprs) = reorder(VecDeque::from(vec![a, b]));
        assert_eq!(exprs.len(), 2);

        let mut iter = exprs.into_iter();
        let a = iter.next().unwrap();
        let b = iter.next().unwrap();

        (stmt, (a, b))
    }

    /// Take list of Expr and returns (Stmt, list of Expr).
    /// Stmt must be executed before list of Expr.
    fn reorder(mut exprs: VecDeque<Expr>) -> (Stmt, VecDeque<Expr>) {
        if exprs.is_empty() {
            return (Stmt::nop(), VecDeque::new());
        }

        let front = exprs.pop_front().unwrap();
        match front {
            front @ Expr::Call(_, _) => {
                let t = Temp::new();
                let front = Expr::ESeq(
                    Box::new(Stmt::Move(Box::new(Expr::Temp(t)), Box::new(front))),
                    Box::new(Expr::Temp(t)),
                );
                exprs.push_front(front);
                reorder(exprs)
            }
            front => {
                let (stmt, expr) = do_expr(front);
                let (stmt2, mut exprs) = reorder(exprs);
                if commute(&expr, &stmt2) {
                    exprs.push_front(expr);
                    (concat_stmt(stmt, stmt2), exprs)
                } else {
                    let t = Temp::new();
                    exprs.push_front(Expr::Temp(t));
                    (
                        stmt.concat(Stmt::Move(Box::new(Expr::Temp(t)), Box::new(expr)))
                            .concat(stmt2),
                        exprs,
                    )
                }
            }
        }
    }

    fn reorder_stmt1<F>(expr: Expr, build_fn: F) -> Stmt
    where
        F: FnOnce(Expr) -> Stmt,
    {
        let (stmt, expr) = reorder1(expr);
        stmt.concat(build_fn(expr))
    }

    fn reorder_stmt2<F>(expr1: Expr, expr2: Expr, build_fn: F) -> Stmt
    where
        F: FnOnce(Expr, Expr) -> Stmt,
    {
        let (stmt, (expr1, expr2)) = reorder2(expr1, expr2);
        stmt.concat(build_fn(expr1, expr2))
    }

    fn reorder_stmt<F>(exprs: VecDeque<Expr>, build_fn: F) -> Stmt
    where
        F: FnOnce(VecDeque<Expr>) -> Stmt,
    {
        let (stmt, exprs) = reorder(exprs);
        stmt.concat(build_fn(exprs))
    }

    fn reorder_expr1<F>(expr: Expr, build_fn: F) -> (Stmt, Expr)
    where
        F: FnOnce(Expr) -> Expr,
    {
        let (stmt, expr) = reorder1(expr);
        (stmt, build_fn(expr))
    }

    fn reorder_expr2<F>(expr1: Expr, expr2: Expr, build_fn: F) -> (Stmt, Expr)
    where
        F: FnOnce(Expr, Expr) -> Expr,
    {
        let (stmt, (a, b)) = reorder2(expr1, expr2);
        (stmt, build_fn(a, b))
    }

    fn reorder_expr<F>(exprs: VecDeque<Expr>, build_fn: F) -> (Stmt, Expr)
    where
        F: FnOnce(VecDeque<Expr>) -> Expr,
    {
        let (stmt, exprs) = reorder(exprs);
        (stmt, build_fn(exprs))
    }

    fn do_stmt(stmt: Stmt) -> Stmt {
        match stmt {
            Stmt::Move(dst, src)
                if matches!(
                    (dst.as_ref(), src.as_ref()),
                    (Expr::Temp(_), Expr::Call(_, _))
                        | (Expr::Temp(_), _)
                        | (Expr::Mem(_, _), _)
                        | (Expr::ESeq(_, _), _)
                ) =>
            {
                match (*dst, *src) {
                    (temp @ Expr::Temp(_), Expr::Call(expr, exprs)) => {
                        let mut exprs = VecDeque::from(exprs);
                        exprs.push_front(*expr);

                        reorder_stmt(exprs, |mut exprs| {
                            let first = exprs.pop_front().unwrap();
                            Stmt::Move(
                                Box::new(temp),
                                Box::new(Expr::Call(Box::new(first), Vec::from(exprs))),
                            )
                        })
                    }
                    (label @ Expr::Temp(_), src) => {
                        reorder_stmt1(src, |expr| Stmt::Move(Box::new(label), Box::new(expr)))
                    }
                    (Expr::Mem(expr, size), src) => reorder_stmt2(*expr, src, |dst, src| {
                        Stmt::Move(Box::new(Expr::Mem(Box::new(dst), size)), Box::new(src))
                    }),
                    (Expr::ESeq(stmt, exprs), src) => {
                        do_stmt(Stmt::Seq(stmt, Box::new(Stmt::Move(exprs, Box::new(src)))))
                    }
                    _ => unreachable!(),
                }
            }

            Stmt::Expr(e) => match *e {
                Expr::Call(name, args) => {
                    let mut args = VecDeque::from(args);
                    args.push_front(*name);
                    let args_len = args.len();

                    reorder_stmt(args, |mut exprs| {
                        assert_eq!(exprs.len(), args_len);

                        let label = exprs.pop_front().unwrap();
                        let args = Vec::from(exprs);
                        Stmt::Expr(Box::new(Expr::Call(Box::new(label), args)))
                    })
                }
                e => reorder_stmt1(e, |e| Stmt::Expr(Box::new(e))),
            },

            Stmt::Jump(e, labels) => reorder_stmt1(*e, |expr| Stmt::Jump(Box::new(expr), labels)),

            Stmt::CJump(op, lhs, rhs, t, f) => reorder_stmt2(*lhs, *rhs, |a, b| {
                Stmt::CJump(op, Box::new(a), Box::new(b), t, f)
            }),

            Stmt::Seq(lhs, rhs) => do_stmt(*lhs).concat(do_stmt(*rhs)),

            s => reorder_stmt(VecDeque::new(), |_| s),
        }
    }

    fn do_expr(expr: Expr) -> (Stmt, Expr) {
        match expr {
            Expr::BinOp(op, lhs, rhs) => reorder_expr2(*lhs, *rhs, |lhs, rhs| {
                Expr::BinOp(op, Box::new(lhs), Box::new(rhs))
            }),
            Expr::Mem(expr, size) => reorder_expr1(*expr, |expr| Expr::Mem(Box::new(expr), size)),
            Expr::Call(expr, exprs) => {
                let mut exprs = VecDeque::from(exprs);
                exprs.push_front(*expr);
                reorder_expr(exprs, |mut exprs| {
                    let name = exprs.pop_front().unwrap();
                    Expr::Call(Box::new(name), Vec::from(exprs))
                })
            }
            Expr::ESeq(stmt, expr) => {
                let stmt = do_stmt(*stmt);
                let (stmt2, e) = do_expr(*expr);
                (stmt.concat(stmt2), e)
            }
            e => reorder_expr(VecDeque::new(), |_| e),
        }
    }

    fn linear(mut list: Vec<Stmt>, stmt: Stmt) -> Vec<Stmt> {
        match stmt {
            Stmt::Seq(lhs, rhs) => linear(linear(list, *lhs), *rhs),
            other => {
                list.push(other);
                list
            }
        }
    }

    /// Concatenating two stmt. Ignore no_op stmt.
    fn concat_stmt(lhs: Stmt, rhs: Stmt) -> Stmt {
        match (lhs, rhs) {
            (Stmt::Expr(e), rhs) if matches!(*e, Expr::Const(_)) => rhs,
            (lhs, Stmt::Expr(e)) if matches!(*e, Expr::Const(_)) => lhs,
            (lhs, rhs) => Stmt::Seq(Box::new(lhs), Box::new(rhs)),
        }
    }

    trait Concat {
        fn concat(self, other: Self) -> Self;
    }

    impl Concat for Stmt {
        fn concat(self, other: Self) -> Self {
            concat_stmt(self, other)
        }
    }

    linear(Vec::new(), do_stmt(stmt))
}

pub fn basic_blocks(stmts: Vec<Stmt>) -> (Vec<Vec<Stmt>>, Label) {
    todo!()
}

pub fn trace_schedule(stmts: Vec<Vec<Stmt>>, label: Label) -> Vec<Stmt> {
    todo!()
}