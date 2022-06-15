use std::{fmt::Debug, marker::PhantomData, sync::atomic::AtomicU32};

use crate::{
    frame::{Frame, X86 as X86Frame},
    ir::{Expr as IrExpr, RelOp, Stmt},
    temp::{Label, Temp},
};

pub trait Translate {
    type Level: Clone;
    type Access: Clone;

    fn outermost() -> Self::Level;
    fn new_level(parent: Self::Level, name: Label, formals: Vec<bool>) -> Self::Level;

    fn formals(level: Self::Level) -> Vec<Self::Access>;
    fn alloc_local(level: Self::Level, is_escape: bool) -> Self::Access;
}

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

pub struct X86(Translator<X86Frame>);

pub struct Translator<F: Frame> {
    _phantomdata: PhantomData<fn() -> F>,
}

impl<F: Frame> Translate for Translator<F> {
    type Level = Level<F>;
    type Access = (Self::Level, F::Access);

    fn outermost() -> Self::Level {
        Level::outermost()
    }

    fn new_level(parent: Self::Level, name: Label, mut formals: Vec<bool>) -> Self::Level {
        // nested function
        if parent.parent.is_some() {
            // for static link
            formals.push(true);
        }
        let frame = F::new(name, formals);
        Level::new(parent, frame)
    }

    fn formals(level: Self::Level) -> Vec<Self::Access> {
        level
            .frame
            .formals()
            .iter()
            .map(|access| (level.clone(), access.clone()))
            .collect()
    }

    fn alloc_local(mut level: Self::Level, is_escape: bool) -> Self::Access {
        let frame = level.frame.alloc_local(is_escape);
        (level, frame)
    }
}

static LEVEL_GLOBAL: AtomicU32 = AtomicU32::new(1);

#[derive(Debug, Clone)]
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
