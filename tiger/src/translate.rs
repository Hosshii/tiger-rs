use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc, sync::atomic::AtomicU32};

use crate::{
    frame::{Fragment, Frame},
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
                        Stmt::Move(Box::new(IrExpr::Temp(r)), Box::new(IrExpr::Const(1))),
                        cx_fn(t.clone(), f.clone()),
                        vec![
                            Stmt::Label(f),
                            Stmt::Move(Box::new(IrExpr::Temp(r)), Box::new(IrExpr::Const(0))),
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
    if parent.inner.parent.is_some() {
        // for static link
        formals.push(true);
    }
    let frame = F::new(name, formals);
    Level::new(parent, frame)
}

pub fn alloc_local<F: Frame>(level: Level<F>, is_escape: bool) -> Access<F> {
    let frame = level.inner.frame.borrow_mut().alloc_local(is_escape);
    (level, frame)
}

// `access` in which variable is defined and `fn_level` in which variable is used.
pub fn simple_var<F: Frame>(access: Access<F>, fn_level: &Level<F>) -> Expr {
    let access_link = calc_static_link(fn_level, &access.0);
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
    fn_level: &Level<F>,
    cur_level: &Level<F>,
    args: Vec<Expr>,
) -> Expr {
    let link = calc_static_link(
        cur_level,
        // if fn_level is outermost, use cur_level so that FP is returned.
        fn_level.inner.parent.as_deref().unwrap_or(cur_level),
    );
    let mut args: Vec<_> = args.into_iter().map(|v| v.unwrap_ex()).collect();
    args.push(link);
    Expr::Ex(IrExpr::Call(Box::new(IrExpr::Name(fn_label)), args))
}

/// Calculate static link that is used by `cur_level` to access `ancestor_level`.
/// `ancestor_level` must be ancestor of `cur_level`.
fn calc_static_link<F: Frame>(mut cur_level: &Level<F>, ancestor_level: &Level<F>) -> IrExpr {
    let mut link = IrExpr::Temp(F::fp());

    if cur_level == ancestor_level {
        return link;
    }

    while cur_level != ancestor_level {
        let frame = cur_level.inner.frame.borrow();
        let link_access = frame.formals().last().expect("static link");
        link = F::exp(link_access.clone(), link);

        let parent = cur_level
            .inner
            .parent
            .as_ref()
            .expect("cur_level is not descendants of parent");
        cur_level = parent;
    }
    link
}

pub fn bin_op(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::Ex(IrExpr::BinOp(
        op,
        Box::new(lhs.unwrap_ex()),
        Box::new(rhs.unwrap_ex()),
    ))
}

pub fn rel_op(op: RelOp, lhs: Expr, rhs: Expr) -> Expr {
    let f = move |t: Label, f: Label| -> Stmt {
        Stmt::CJump(
            op,
            Box::new(lhs.unwrap_ex()),
            Box::new(rhs.unwrap_ex()),
            t,
            f,
        )
    };
    Expr::Cx(Box::new(f))
}

/// `op` must be Eq or Ne.
pub fn string_eq<F: Frame>(op: RelOp, lhs: Expr, rhs: Expr) -> Expr {
    // stringEqual returns 0 if equal and 1 if not equal.
    let result = F::extern_call("stringEqual", vec![lhs.unwrap_ex(), rhs.unwrap_ex()]);

    let result = match op {
        RelOp::Eq => result,
        RelOp::Ne => IrExpr::BinOp(BinOp::Minus, Box::new(IrExpr::Const(1)), Box::new(result)),
        _ => panic!("unexpected op"),
    };

    let f = |t: Label, f: Label| -> Stmt {
        Stmt::CJump(
            RelOp::Eq,
            Box::new(IrExpr::Const(0)),
            Box::new(result),
            t,
            f,
        )
    };
    Expr::Cx(Box::new(f))
}

/// `op` must be one of the le, lt, ge, gt.
pub fn string_ord<F: Frame>(op: RelOp, lhs: Expr, rhs: Expr) -> Expr {
    let num = match op {
        RelOp::Le => 1,
        RelOp::Lt => 2,
        RelOp::Ge => 3,
        RelOp::Gt => 4,
        _ => panic!("unexpected op"),
    };

    // `stringOrd` take op as const.
    // It returns 0 if condition is satisfied, and 1 if not.
    let result = F::extern_call(
        "stringOrd",
        vec![IrExpr::Const(num), lhs.unwrap_ex(), rhs.unwrap_ex()],
    );

    let f = |t: Label, f: Label| -> Stmt {
        Stmt::CJump(
            RelOp::Eq,
            Box::new(IrExpr::Const(0)),
            Box::new(result),
            t,
            f,
        )
    };
    Expr::Cx(Box::new(f))
}

pub fn neg(e: Expr) -> Expr {
    Expr::Ex(IrExpr::BinOp(
        BinOp::Minus,
        Box::new(IrExpr::Const(0)),
        Box::new(e.unwrap_ex()),
    ))
}

/// Create record.
/// For simplicity, the size of each record field must be 1 WORD.
pub fn record_creation<F: Frame>(exprs: Vec<Expr>) -> Expr {
    let ptr = Temp::new();
    let record_size = exprs.len();

    // allocRecord take size of record.
    let allocated = F::extern_call("allocRecord", vec![IrExpr::Const(record_size as i64)]);

    // move allocated ptr to reg.
    let init = Stmt::Move(Box::new(IrExpr::Temp(ptr)), Box::new(allocated));

    let sequence = exprs
        .into_iter()
        .enumerate()
        .fold(init, |acc, (idx, expr)| {
            Stmt::Seq(
                Box::new(acc),
                Box::new(Stmt::Move(
                    Box::new(IrExpr::Mem(
                        Box::new(IrExpr::BinOp(
                            BinOp::Plus,
                            Box::new(IrExpr::Temp(ptr)),
                            Box::new(IrExpr::Const(idx as i64)),
                        )),
                        F::WORD_SIZE,
                    )),
                    Box::new(expr.unwrap_ex()),
                )),
            )
        });

    Expr::Ex(IrExpr::ESeq(
        Box::new(sequence),
        Box::new(IrExpr::Temp(ptr)),
    ))
}

pub fn array_creation<F: Frame>(len: Expr, init: Expr) -> Expr {
    let ptr = Temp::new();
    let len_var = Temp::new();
    let move_len = Stmt::Move(Box::new(IrExpr::Temp(len_var)), Box::new(len.unwrap_ex()));

    // allocRecord take size of record.
    let allocated = F::extern_call("allocRecord", vec![IrExpr::Temp(len_var)]);

    // move allocated ptr to reg.
    let move_result = Stmt::Move(Box::new(IrExpr::Temp(ptr)), Box::new(allocated));

    // while i < len {
    //   allocated[i] = init;
    //   i++
    // }
    let test = IrExpr::Temp(Temp::new());
    let cond = {
        let test = test.clone();

        move |t: Label, f: Label| -> Stmt {
            Stmt::CJump(
                RelOp::Lt,
                Box::new(test),
                Box::new(IrExpr::Temp(len_var)),
                t,
                f,
            )
        }
    };
    let cond = Expr::Cx(Box::new(cond));

    let body = Stmt::Seq(
        Box::new(Stmt::Move(
            Box::new(IrExpr::Mem(
                Box::new(IrExpr::BinOp(
                    BinOp::Plus,
                    Box::new(IrExpr::Temp(ptr)),
                    Box::new(IrExpr::BinOp(
                        BinOp::Mul,
                        Box::new(test.clone()),
                        Box::new(IrExpr::Const(F::WORD_SIZE as i64)),
                    )),
                )),
                F::WORD_SIZE,
            )),
            Box::new(init.unwrap_ex()),
        )),
        Box::new(Stmt::Expr(Box::new(IrExpr::BinOp(
            BinOp::Plus,
            Box::new(test),
            Box::new(IrExpr::Const(1)),
        )))),
    );
    let body = Expr::Nx(body);

    let end_label = Label::new();
    let init = while_expr(cond, body, end_label.clone());

    let expr = IrExpr::ESeq(
        Box::new(Stmt::Seq(
            Box::new(move_len),
            Box::new(Stmt::Seq(
                Box::new(move_result),
                Box::new(Stmt::Seq(
                    Box::new(init.unwrap_nx()),
                    Box::new(Stmt::Label(end_label)),
                )),
            )),
        )),
        Box::new(IrExpr::Temp(ptr)),
    );

    Expr::Ex(expr)
}

pub fn assign(lhs: Expr, rhs: Expr) -> Expr {
    Expr::Nx(Stmt::Move(
        Box::new(lhs.unwrap_ex()),
        Box::new(rhs.unwrap_ex()),
    ))
}

pub fn if_expr<F: Frame>(level: &mut Level<F>, cond: Expr, then: Expr, els: Option<Expr>) -> Expr {
    let t = Label::new();
    let f = Label::new();
    let merge = Label::new();
    let access = level.inner.frame.borrow_mut().alloc_local(false);
    let result = F::exp(access, IrExpr::Temp(F::fp()));

    let true_stmt = Stmt::Seq(
        Box::new(Stmt::Label(t.clone())),
        Box::new(Stmt::Seq(
            Box::new(Stmt::Move(
                Box::new(result.clone()),
                Box::new(then.unwrap_ex()),
            )),
            Box::new(Stmt::Jump(
                Box::new(IrExpr::Name(merge.clone())),
                vec![merge.clone()],
            )),
        )),
    );

    let els = els.unwrap_or_else(unit);
    let false_stmt = Stmt::Seq(
        Box::new(Stmt::Label(f.clone())),
        Box::new(Stmt::Seq(
            Box::new(Stmt::Move(
                Box::new(result.clone()),
                Box::new(els.unwrap_ex()),
            )),
            Box::new(Stmt::Jump(
                Box::new(IrExpr::Name(merge.clone())),
                vec![merge],
            )),
        )),
    );

    let stmt = cond.unwrap_cx()(t, f);
    Expr::Ex(IrExpr::ESeq(
        Box::new(Stmt::Seq(
            Box::new(stmt),
            Box::new(Stmt::Seq(Box::new(true_stmt), Box::new(false_stmt))),
        )),
        Box::new(result),
    ))
}

pub fn while_expr(cond: Expr, body: Expr, break_label: Label) -> Expr {
    let test = Label::new();
    let body_label = Label::new();

    let cond = cond.unwrap_cx()(body_label.clone(), break_label);

    let body = Stmt::seq(
        Stmt::Label(body_label),
        Stmt::Expr(Box::new(body.unwrap_ex())),
        vec![Stmt::Jump(
            Box::new(IrExpr::Name(test.clone())),
            vec![test.clone()],
        )],
    );

    Expr::Nx(Stmt::seq(Stmt::Label(test), cond, vec![body]))
}

pub fn break_expr(break_label: Label) -> Expr {
    Expr::Nx(Stmt::Jump(
        Box::new(IrExpr::Name(break_label.clone())),
        vec![break_label],
    ))
}

pub fn let_expr(decls: Vec<Expr>, body: Expr) -> Expr {
    let mut decls: Vec<_> = decls.into_iter().map(|v| v.unwrap_nx()).collect();
    let decls = match decls.len() {
        0 => Stmt::Expr(Box::new(IrExpr::Const(0))),
        1 => decls.swap_remove(0),
        _ => {
            let tail = decls.split_off(2);
            let second = decls.pop().unwrap();
            let first = decls.pop().unwrap();
            Stmt::seq(first, second, tail)
        }
    };

    let expr = IrExpr::ESeq(Box::new(decls), Box::new(body.unwrap_ex()));

    Expr::Ex(expr)
}

pub fn var_decl<F: Frame>(access: Access<F>, value: Expr) -> Expr {
    let lhs = F::exp(access.1, IrExpr::Temp(F::fp()));
    Expr::Nx(Stmt::Move(Box::new(lhs), Box::new(value.unwrap_ex())))
}

pub fn unit() -> Expr {
    Expr::Nx(Stmt::Expr(Box::new(IrExpr::Const(0))))
}

pub struct Translator<F: Frame> {
    string_literal_map: HashMap<Symbol, Label>,
    fragments: Vec<Fragment<F>>,
}

impl<F: Frame> Translator<F> {
    pub fn new() -> Self {
        Self {
            string_literal_map: HashMap::new(),
            fragments: Vec::new(),
        }
    }

    pub fn string_literal(&mut self, string: String) -> Expr {
        let sym = Symbol::new(string.clone());
        let label = match self.string_literal_map.get(&sym) {
            Some(label) => label.clone(),
            None => {
                let label = Label::new();
                self.string_literal_map.insert(sym, label.clone());
                self.fragments.push(Fragment::String(label.clone(), string));
                label
            }
        };
        Expr::Nx(Stmt::Label(label))
    }

    pub fn proc_entry_exit(&mut self, level: Level<F>, body: Expr) {
        let body = Stmt::Move(Box::new(IrExpr::Temp(F::rv())), Box::new(body.unwrap_ex()));

        self.fragments.push(Fragment::Proc(body, level.inner.frame))
    }

    pub fn get_result(self) -> Vec<Fragment<F>> {
        self.fragments
    }
}

impl<F: Frame> Default for Translator<F> {
    fn default() -> Self {
        Self::new()
    }
}

static LEVEL_GLOBAL: AtomicU32 = AtomicU32::new(1);

#[derive(Debug, Eq)]
pub struct Level<F: Frame> {
    inner: LevelInner<F>,
}

impl<F: Frame> Level<F> {
    pub fn new(parent: Level<F>, frame: F) -> Self {
        Self {
            inner: LevelInner::new(parent, frame),
        }
    }

    pub fn outermost() -> Self {
        Self {
            inner: LevelInner::outermost(),
        }
    }

    pub fn formals(&self) -> Vec<Access<F>> {
        self.inner
            .frame
            .borrow()
            .formals()
            .iter()
            .map(|access| (self.clone(), access.clone()))
            .collect()
    }
}

impl<F: Frame> PartialEq for Level<F> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<F: Frame> Clone for Level<F> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

#[derive(Debug, Eq)]
struct LevelInner<F: Frame> {
    current: u32, // unique value
    frame: Rc<RefCell<F>>,
    parent: Option<Box<Level<F>>>,
}

impl<F: Frame> LevelInner<F> {
    fn new(parent: Level<F>, frame: F) -> Self {
        let current = LEVEL_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let parent = Some(Box::new(parent));
        let frame = Rc::new(RefCell::new(frame));
        Self {
            current,
            frame,
            parent,
        }
    }

    pub fn outermost() -> Self {
        Self {
            current: 0,
            frame: Rc::new(RefCell::new(F::new(Label::new(), vec![]))),
            parent: None,
        }
    }
}

impl<F: Frame> PartialEq for LevelInner<F> {
    fn eq(&self, other: &Self) -> bool {
        self.current == other.current
    }
}

impl<F: Frame> Clone for LevelInner<F> {
    fn clone(&self) -> Self {
        Self {
            current: self.current,
            frame: self.frame.clone(),
            parent: self.parent.clone(),
        }
    }
}
