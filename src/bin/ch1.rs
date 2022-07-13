use std::cmp;

use self::{BinOp::*, Expr::*, Stmt::*};

fn main() {
    let prog = Compound(
        Box::new(Assign(
            "a".to_string(),
            Box::new(Op(Box::new(Num(5)), Plus, Box::new(Num(3)))),
        )),
        Box::new(Compound(
            Box::new(Assign(
                "b".to_string(),
                Box::new(Eseq(
                    Box::new(Print(cons![
                        Id("a".to_string()),
                        Op(Box::new(Id("a".to_string())), Minus, Box::new(Num(1))),
                    ])),
                    Box::new(Op(Box::new(Num(10)), Times, Box::new(Id("a".to_string())))),
                )),
            )),
            Box::new(Print(cons![Id("b".to_string())])),
        )),
    );

    println!("{:?}", max_args(&prog));
    interp_stmt(prog, ConsList::Nil);
}

fn max_args(s: &Stmt) -> Option<usize> {
    match s {
        Compound(lhs, rhs) => cmp::max(max_args(lhs), max_args(rhs)),
        Assign(_, exp) => max_args_expr(exp),
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
    #[allow(dead_code)]
    Div,
}

enum Stmt {
    Compound(Box<Stmt>, Box<Stmt>),
    Assign(Id, Box<Expr>),
    Print(ConsList<Expr>),
}

enum Expr {
    Id(Id),
    Num(i32),
    Op(Box<Expr>, BinOp, Box<Expr>),
    Eseq(Box<Stmt>, Box<Expr>),
}

//

#[derive(Clone, Debug, PartialEq, Eq)]
enum ConsList<T> {
    Nil,
    Cons(T, Box<ConsList<T>>),
}

impl<T> ConsList<T> {
    pub fn len(&self) -> usize {
        use ConsList::*;
        match self {
            Nil => 0,
            Cons(_, l) => 1 + l.len(),
        }
    }

    pub fn reverse(self) -> Self {
        return f(self, ConsList::Nil);

        fn f<T>(orig: ConsList<T>, reversed: ConsList<T>) -> ConsList<T> {
            match orig {
                ConsList::Nil => reversed,
                ConsList::Cons(x, xs) => f(*xs, ConsList::Cons(x, Box::new(reversed))),
            }
        }
    }
}

#[macro_export]
macro_rules! cons {
    () => (
        ConsList::Nil
    );
    ( $( $x:expr ),+ $(,)? ) => {
        {
            let list = ConsList::Nil;
            $(
                let list = ConsList::Cons($x, Box::new(list));
            )*
            list.reverse()
        }
    };
}

type Table = ConsList<(Id, i32)>;

fn interp_stmt(stmt: Stmt, table: Table) -> Table {
    use ConsList::*;

    match stmt {
        Compound(lhs, rhs) => {
            let table = interp_stmt(*lhs, table);
            interp_stmt(*rhs, table)
        }
        Assign(id, expr) => {
            let (value, table) = interp_expr(*expr, table);
            Cons((id, value), Box::new(table))
        }
        Print(exprs) => {
            fn print_exprs(exprs: ConsList<Expr>, table: Table) -> Table {
                match exprs {
                    ConsList::Nil => {
                        println!();
                        table
                    }
                    ConsList::Cons(expr, exprs) => {
                        let (value, table) = interp_expr(expr, table);
                        print!("{} ", value);
                        print_exprs(*exprs, table)
                    }
                }
            }
            print_exprs(exprs, table)
        }
    }
}

fn interp_expr(expr: Expr, table: Table) -> (i32, Table) {
    match expr {
        Id(id) => (lookup(id, table.clone()).unwrap(), table),
        Num(v) => (v, table),
        Op(lhs, op, rhs) => {
            let (lhs, table) = interp_expr(*lhs, table);
            let (rhs, table) = interp_expr(*rhs, table);
            match op {
                Plus => (lhs + rhs, table),
                Minus => (lhs - rhs, table),
                Times => (lhs * rhs, table),
                Div => (lhs / rhs, table),
            }
        }
        Eseq(stmt, expr) => {
            let table = interp_stmt(*stmt, table);
            interp_expr(*expr, table)
        }
    }
}

fn lookup(id: Id, table: Table) -> Option<i32> {
    match table {
        ConsList::Cons((_id, value), table) => {
            if id == _id {
                Some(value)
            } else {
                lookup(id, *table)
            }
        }
        ConsList::Nil => None,
    }
}
