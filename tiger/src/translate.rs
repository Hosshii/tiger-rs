use std::{collections::HashMap, fmt::Debug, sync::atomic::AtomicU32};

use crate::{
    frame::Frame,
    ir::{BinOp, Expr as IrExpr, RelOp, Stmt},
    symbol::Symbol,
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

// `access` in which variable is defined and `fn_level` in which variable is used.
pub fn simple_var<F: Frame>(access: Access<F>, mut fn_level: Level<F>) -> Expr {
    let access_link = calc_static_link(fn_level, access.0);
    let mem = F::exp(access.1, access_link);
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

pub fn num(v: i64) -> Expr {
    Expr::Ex(IrExpr::Const(v))
}

/// The length of argument must be greater than 1.
pub fn sequence(mut exprs: Vec<Expr>) -> Expr {
    assert!(exprs.len() > 1);

    let last = exprs.pop().unwrap();
    let last_1 = exprs.pop().unwrap();

    let stmt =
        exprs
            .into_iter()
            .rev()
            .fold(Stmt::Expr(Box::new(last_1.unwrap_ex())), |acc, cur| {
                Stmt::Seq(
                    Box::new(Stmt::Expr(Box::new(cur.unwrap_ex()))),
                    Box::new(acc),
                )
            });

    let e_seq = IrExpr::ESeq(Box::new(stmt), Box::new(last.unwrap_ex()));

    Expr::Ex(e_seq)
}

pub fn fn_call<F: Frame>(
    fn_label: Label,
    fn_level: Level<F>,
    cur_level: Level<F>,
    args: Vec<Expr>,
) -> Expr {
    // recursion
    let link = calc_static_link(cur_level, *fn_level.parent.expect("fn must have parent"));
    let mut args: Vec<_> = args.into_iter().map(|v| v.unwrap_ex()).collect();
    args.push(link);
    Expr::Ex(IrExpr::Call(Box::new(IrExpr::Name(fn_label)), args))
}

/// Calculate static link that is used by `cur_level` to access `ancestor_level`.
/// `ancestor_level` must be ancestor of `cur_level`.
fn calc_static_link<F: Frame>(mut cur_level: Level<F>, ancestor_level: Level<F>) -> IrExpr {
    let mut link = IrExpr::Temp(F::fp());
    while cur_level != ancestor_level {
        let link_access = cur_level.frame.formals().last().expect("static link");
        link = F::exp(link_access.clone(), link);
        cur_level = *(cur_level
            .parent
            .expect("cur_level is not descendants of parent"));
    }
    link
}

pub struct Translator {
    string_literal_map: HashMap<Symbol, Label>,
}

impl Translator {
    pub fn new() -> Self {
        Self {
            string_literal_map: HashMap::new(),
        }
    }

    pub fn string_literal(&mut self, string: String) -> Expr {
        let sym = Symbol::new(string);
        let label = match self.string_literal_map.get(&sym) {
            Some(label) => label.clone(),
            None => {
                let label = Label::new();
                self.string_literal_map.insert(sym, label.clone());
                label
            }
        };
        Expr::Nx(Stmt::Label(label))
    }
}

impl Default for Translator {
    fn default() -> Self {
        Self::new()
    }
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
