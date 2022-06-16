use std::{fmt::Debug, sync::atomic::AtomicU32};

use crate::{
    frame::Frame,
    ir::{BinOp, Expr as IrExpr, RelOp, Stmt},
    temp::{Label, Temp},
};

type CxFn<'a> = dyn (FnOnce(Label, Label) -> Stmt) + 'a;

pub enum Expr {
    Ex(IrExpr),
    Nx(Stmt),
    Cx(Box<CxFn<'static>>),
}

impl Expr {
    fn unwrap_ex(self) -> IrExpr {
        match self {
            Expr::Ex(e) => e,
            Expr::Nx(stmt) => IrExpr::ESeq(Box::new(stmt), Box::new(IrExpr::Const(0))),
            Expr::Cx(cx_fn) => {
                let r = Temp::new();
                let t = Label::new();
                let f = Label::new();
                IrExpr::ESeq(
                    Box::new(Stmt::seq(
                        Stmt::Move(
                            Box::new(IrExpr::Temp(r.clone())),
                            Box::new(IrExpr::Const(1)),
                        ),
                        cx_fn(t.clone(), f.clone()),
                        vec![
                            Stmt::Label(f),
                            Stmt::Move(
                                Box::new(IrExpr::Temp(r.clone())),
                                Box::new(IrExpr::Const(0)),
                            ),
                            Stmt::Label(t),
                        ],
                    )),
                    Box::new(IrExpr::Temp(r)),
                )
            }
        }
    }

    fn unwrap_nx(self) -> Stmt {
        match self {
            Expr::Ex(e) => Stmt::Expr(Box::new(e)),
            Expr::Nx(s) => s,
            Expr::Cx(cx_fn) => {
                let t = Label::new();
                let f = Label::new();
                cx_fn(t, f)
            }
        }
    }

    fn unwrap_cx(self) -> Box<CxFn<'static>> {
        match self {
            Expr::Ex(e) => {
                let f = |t: Label, f: Label| {
                    // if e == 0 then goto t
                    // else goto f
                    Stmt::CJump(RelOp::Eq, Box::new(e), Box::new(IrExpr::Const(0)), t, f)
                };

                Box::new(f)
            }
            Expr::Nx(_) => unreachable!(),
            Expr::Cx(f) => f,
        }
    }
}

impl Expr {
    pub fn new() -> Self {
        todo!()
        // Self {}
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ex(ir) => ir.fmt(f),
            Expr::Nx(stm) => stm.fmt(f),
            Expr::Cx(_) => write!(f, "closure"),
        }
    }
}

pub type Access<F> = (Level<F>, <F as Frame>::Access);

pub fn new_level<F: Frame>(parent: Level<F>, name: Label, mut formals: Vec<bool>) -> Level<F> {
    // nested function
    if parent.parent.is_some() {
        // for static link
        formals.push(true);
    }
    let frame = F::new(name, formals);
    Level::new(parent, frame)
}

pub fn formals<F: Frame>(level: Level<F>) -> Vec<Access<F>> {
    level
        .frame
        .formals()
        .iter()
        .map(|access| (level.clone(), access.clone()))
        .collect()
}

pub fn alloc_local<F: Frame>(mut level: Level<F>, is_escape: bool) -> Access<F> {
    let frame = level.frame.alloc_local(is_escape);
    (level, frame)
}

pub fn simple_var<F: Frame>(access: Access<F>, mut fn_level: Level<F>) -> Expr {
    let mut mem = F::exp(access.1, IrExpr::Temp(F::fp()));
    let var_level = access.0;

    while var_level != fn_level {
        fn_level = *fn_level.parent.expect("function should have parent level");
        let access = fn_level
            .frame
            .formals()
            .last()
            .expect("static link")
            .clone();
        mem = F::exp(access, mem);
    }

    Expr::Ex(mem)
}

pub fn record_field<F: Frame>(var: Expr, field_index: usize) -> Expr {
    Expr::Ex(IrExpr::Mem(
        Box::new(IrExpr::BinOp(
            BinOp::Plus,
            Box::new(var.unwrap_ex()),
            Box::new(IrExpr::Const(F::WORD_SIZE as i64 * field_index as i64)),
        )),
        F::WORD_SIZE,
    ))
}

pub fn array_subscript<F: Frame>(var: Expr, subscript: Expr) -> Expr {
    Expr::Ex(IrExpr::Mem(
        Box::new(IrExpr::BinOp(
            BinOp::Plus,
            Box::new(var.unwrap_ex()),
            Box::new(IrExpr::BinOp(
                BinOp::Mul,
                Box::new(subscript.unwrap_ex()),
                Box::new(IrExpr::Const(F::WORD_SIZE as i64)),
            )),
        )),
        F::WORD_SIZE,
    ))
}

static LEVEL_GLOBAL: AtomicU32 = AtomicU32::new(1);

#[derive(Debug, Clone, Eq)]
pub struct Level<F: Frame> {
    current: u32, // unique value
    frame: F,
    parent: Option<Box<Level<F>>>,
}

impl<F: Frame> Level<F> {
    fn new(parent: Level<F>, frame: F) -> Self {
        let current = LEVEL_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let parent = Some(Box::new(parent));
        Self {
            current,
            frame,
            parent,
        }
    }

    pub fn outermost() -> Self {
        Self {
            current: 0,
            frame: F::new(Label::new(), vec![]),
            parent: None,
        }
    }
}

impl<F: Frame> PartialEq for Level<F> {
    fn eq(&self, other: &Self) -> bool {
        self.current == other.current
    }
}
