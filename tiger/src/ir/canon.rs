use crate::ir::Stmt;

pub use blocks::basic_blocks;
pub use linearize::linearize;
pub use trace::trace_schedule;

mod linearize {
    use crate::{
        common::Temp,
        ir::{Expr, Stmt},
    };
    use std::collections::VecDeque;

    pub fn linearize(stmt: Stmt) -> Vec<Stmt> {
        linear(Vec::new(), do_stmt(stmt))
    }

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
}

type Block = Vec<Stmt>;

mod blocks {
    use crate::{
        common::Label,
        ir::{Expr, Stmt},
    };
    use std::collections::VecDeque;

    use super::Block;

    pub fn basic_blocks(stmts: VecDeque<Stmt>) -> (Vec<Block>, Label) {
        let done_label = Label::new();
        let blocks = BlockBuilder::basic_blocks(stmts, done_label.clone());
        (blocks, done_label)
    }

    struct BlockBuilder {
        iter: VecDeque<Stmt>,
        done: Label,
        result: Vec<Block>,
        cur_block: Block,
    }

    impl BlockBuilder {
        fn new(stmts: impl Into<VecDeque<Stmt>>, done_label: Label) -> Self {
            Self {
                iter: stmts.into(),
                done: done_label,
                result: Vec::default(),
                cur_block: Vec::default(),
            }
        }

        fn basic_blocks(stmts: impl Into<VecDeque<Stmt>>, done_label: Label) -> Vec<Block> {
            let mut builder = Self::new(stmts, done_label);
            builder.blocks();
            builder.result
        }

        fn next(&mut self) {
            match self.iter.pop_front() {
                None => {
                    self.iter.push_front(Stmt::Jump(
                        Box::new(Expr::Name(self.done.clone())),
                        vec![self.done.clone()],
                    ));
                    self.next();
                }
                Some(stmt @ (Stmt::CJump(_, _, _, _, _) | Stmt::Jump(_, _))) => {
                    self.cur_block.push(stmt);
                    self.end_block();
                }
                Some(Stmt::Label(label)) => {
                    self.iter
                        .push_front(Stmt::Jump(Box::new(Expr::Name(label.clone())), vec![label]));
                    self.next();
                }
                Some(s) => {
                    self.cur_block.push(s);
                    self.next();
                }
            }
        }

        fn end_block(&mut self) {
            let block = std::mem::take(&mut self.cur_block);
            self.result.push(block);
            self.blocks();
        }

        fn blocks(&mut self) {
            match self.iter.pop_front() {
                Some(label @ Stmt::Label(_)) => {
                    self.cur_block.push(label);
                    self.next();
                }
                Some(s) => {
                    self.iter.push_front(s);
                    self.iter.push_front(Stmt::Label(Label::new()));
                    self.blocks();
                }
                None => (),
            }
        }
    }
}

mod trace {
    use std::collections::HashMap;

    use crate::{
        common::Label,
        ir::{Expr, RelOp, Stmt},
    };

    use super::Block;

    pub fn trace_schedule(mut blocks: Vec<Block>, label: Label) -> Vec<Stmt> {
        let mut table = HashMap::new();
        for (idx, block) in blocks.iter().enumerate() {
            match block.first().expect("block should have at least 1 element") {
                Stmt::Label(label) => {
                    if table.insert(label.clone(), idx).is_some() {
                        panic!("exists same label in multiple place.")
                    }
                }
                _ => panic!("first element of block must be label."),
            }
        }

        let mut seen = vec![false; blocks.len()];

        let mut result = Vec::with_capacity(blocks.len());

        for idx in 0..blocks.len() {
            if seen[idx] {
                continue;
            }

            enter_block(&mut table, &mut seen, &mut result, &mut blocks, idx)
        }

        result.into_iter().flatten().collect()
    }

    fn enter_block(
        table: &mut HashMap<Label, usize>,
        seen: &mut [bool],
        result: &mut Vec<Block>,
        blocks: &mut Vec<Block>,
        idx: usize,
    ) {
        let mut block = std::mem::take(&mut blocks[idx]);
        seen[idx] = true;

        let next_idx = match block.pop().expect("block should have at least 1 element") {
            Stmt::CJump(op, lhs, rhs, t, f) => {
                if let Some(idx) = table.remove(&f) {
                    block.push(Stmt::CJump(op, lhs, rhs, t, f));
                    Some(idx)
                } else if let Some(idx) = table.remove(&t) {
                    // exchange label and cond
                    block.push(Stmt::CJump(negate_condition(op), lhs, rhs, f, t));
                    Some(idx)
                } else {
                    let new_label = Label::new();
                    block.push(Stmt::CJump(op, lhs, rhs, t, new_label.clone()));
                    block.push(Stmt::Label(new_label.clone()));
                    block.push(Stmt::Jump(
                        Box::new(Expr::Name(new_label.clone())),
                        vec![new_label],
                    ));
                    None
                }
            }
            Stmt::Jump(_, labels) => labels
                .iter()
                .fold(None, |acc, label| acc.or_else(|| table.remove(label))),
            _ => panic!("last element of block must be CJump or Jump"),
        };

        result.push(block);

        if let Some(next_idx) = next_idx {
            enter_block(table, seen, result, blocks, next_idx)
        }
    }

    fn negate_condition(op: RelOp) -> RelOp {
        match op {
            RelOp::Eq => RelOp::Ne,
            RelOp::Ge => RelOp::Lt,
            RelOp::Gt => RelOp::Le,
            RelOp::Le => RelOp::Gt,
            RelOp::Lt => RelOp::Ge,
            RelOp::Ne => RelOp::Eq,
            RelOp::Uge => RelOp::Ult,
            RelOp::Ugt => RelOp::Ule,
            RelOp::Ule => RelOp::Ugt,
            RelOp::Ult => RelOp::Uge,
        }
    }
}
