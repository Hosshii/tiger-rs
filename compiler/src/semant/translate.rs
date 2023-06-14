use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc, sync::atomic::AtomicU32};

use crate::{
    common::{Label, Symbol, Temp},
    frame::{Fragment, Frame},
    ir::{BinOp, Expr as IrExpr, RelOp, Stmt},
};

use super::{
    builtin::Builtin,
    ctx::{FnId, TyCtx, VarId},
    hir::{Decl, Expr as HirExpr, ExprKind, LValue, Operator, VarDecl},
    types::{Type, TypeId},
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
                    Box::new(Stmt::seq(vec![
                        Stmt::Move(Box::new(IrExpr::Temp(r)), Box::new(IrExpr::Const(1))),
                        cx_fn(t.clone(), f.clone()),
                        Stmt::Label(f),
                        Stmt::Move(Box::new(IrExpr::Temp(r)), Box::new(IrExpr::Const(0))),
                        Stmt::Label(t),
                    ])),
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
                    // if e != 0 then goto t
                    // else goto f
                    Stmt::CJump(RelOp::Ne, Box::new(e), Box::new(IrExpr::Const(0)), t, f)
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

fn new_level<F: Frame>(parent: Level<F>, name: Label, mut formals: Vec<bool>) -> Level<F> {
    // for static link
    formals.push(true);

    let frame = F::new(name, formals);
    Level::new(parent, frame)
}

fn alloc_local<F: Frame>(level: Level<F>, is_escape: bool) -> Access<F> {
    let frame = level.inner.frame.borrow_mut().alloc_local(is_escape);
    (level, frame)
}

// `access` in which variable is defined and `fn_level` in which variable is used.
fn simple_var<F: Frame>(access: Access<F>, fn_level: &Level<F>) -> Expr {
    let access_link = calc_static_link(fn_level, &access.0);
    let mem = F::exp(access.1, access_link);
    Expr::Ex(mem)
}

/// Returns Mem(record.field)
// `var` is pointer to TigerRecord.
// So, var[0] is (*var).data.offset(0)
fn record_field<F: Frame>(var: Expr, field_index: usize) -> Expr {
    // (*var).data
    let var_data = IrExpr::Mem(
        Box::new(IrExpr::BinOp(
            BinOp::Plus,
            Box::new(var.unwrap_ex()),
            Box::new(IrExpr::Const(F::WORD_SIZE as i64)),
        )),
        F::WORD_SIZE,
    );

    Expr::Ex(IrExpr::Mem(
        Box::new(IrExpr::BinOp(
            BinOp::Plus,
            Box::new(var_data),
            Box::new(IrExpr::Const(F::WORD_SIZE as i64 * field_index as i64)),
        )),
        F::WORD_SIZE,
    ))
}

/// Returns Mem(Var(offset))
// `var` is pointer to TigerArray.
// So, var[0] is (*var).data.offset(0)
fn array_subscript<F: Frame>(var: Expr, subscript: Expr) -> Expr {
    // (*var).data
    let var_data = IrExpr::Mem(
        Box::new(IrExpr::BinOp(
            BinOp::Plus,
            Box::new(var.unwrap_ex()),
            Box::new(IrExpr::Const(F::WORD_SIZE as i64)),
        )),
        F::WORD_SIZE,
    );

    // var_data[subscript]
    Expr::Ex(IrExpr::Mem(
        Box::new(IrExpr::BinOp(
            BinOp::Plus,
            Box::new(var_data),
            Box::new(IrExpr::BinOp(
                BinOp::Mul,
                Box::new(subscript.unwrap_ex()),
                Box::new(IrExpr::Const(F::WORD_SIZE as i64)),
            )),
        )),
        F::WORD_SIZE,
    ))
}

fn num(v: i64) -> Expr {
    Expr::Ex(IrExpr::Const(v))
}

fn sequence(mut exprs: Vec<Expr>) -> Expr {
    if exprs.is_empty() {
        nop()
    } else {
        let last = exprs.pop().unwrap();
        let stmts = exprs
            .into_iter()
            .map(|v| Stmt::Expr(Box::new(v.unwrap_ex())))
            .collect();
        let stmt = Stmt::seq(stmts);
        let e_seq = IrExpr::ESeq(Box::new(stmt), Box::new(last.unwrap_ex()));

        Expr::Ex(e_seq)
    }
}

fn fn_call<F: Frame>(
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
        let link_access = frame
            .formals()
            .last()
            .expect("static link does not exists in formals");
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

fn bin_op(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::Ex(IrExpr::BinOp(
        op,
        Box::new(lhs.unwrap_ex()),
        Box::new(rhs.unwrap_ex()),
    ))
}

fn rel_op(op: RelOp, lhs: Expr, rhs: Expr) -> Expr {
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
fn string_eq<F: Frame>(op: RelOp, lhs: Expr, rhs: Expr) -> Expr {
    // stringEqual returns 1 if equal and 0 if not equal.
    let result = F::extern_call("stringEqual", vec![lhs.unwrap_ex(), rhs.unwrap_ex()]);

    let result = match op {
        RelOp::Eq => result,
        RelOp::Ne => IrExpr::BinOp(BinOp::Minus, Box::new(IrExpr::Const(1)), Box::new(result)),
        _ => panic!("unexpected op"),
    };

    let f = |t: Label, f: Label| -> Stmt {
        Stmt::CJump(
            RelOp::Eq,
            Box::new(IrExpr::Const(1)),
            Box::new(result),
            t,
            f,
        )
    };
    Expr::Cx(Box::new(f))
}

/// `op` must be one of the le, lt, ge, gt.
fn string_ord<F: Frame>(op: RelOp, lhs: Expr, rhs: Expr) -> Expr {
    let num = match op {
        RelOp::Le => 1,
        RelOp::Lt => 2,
        RelOp::Ge => 3,
        RelOp::Gt => 4,
        _ => panic!("unexpected op"),
    };

    // `stringOrd` take op as const.
    // It returns 1 if condition is satisfied, and 0 if not.
    let result = F::extern_call(
        "stringOrd",
        vec![IrExpr::Const(num), lhs.unwrap_ex(), rhs.unwrap_ex()],
    );

    let f = |t: Label, f: Label| -> Stmt {
        Stmt::CJump(
            RelOp::Eq,
            Box::new(IrExpr::Const(1)),
            Box::new(result),
            t,
            f,
        )
    };
    Expr::Cx(Box::new(f))
}

fn neg(e: Expr) -> Expr {
    Expr::Ex(IrExpr::BinOp(
        BinOp::Minus,
        Box::new(IrExpr::Const(0)),
        Box::new(e.unwrap_ex()),
    ))
}

/// Create record.
/// For simplicity, the size of each record field must be 1 WORD.
fn record_creation<F: Frame>(exprs: Vec<Expr>) -> Expr {
    let record_temp = Temp::new();
    let record_size = exprs.len();

    let stmt = Stmt::Move(
        Box::new(IrExpr::Temp(record_temp)),
        Box::new(F::extern_call(
            "allocRecord",
            vec![IrExpr::Const(record_size as i64)],
        )),
    );

    let sequence = exprs
        .into_iter()
        .enumerate()
        .fold(stmt, |acc, (idx, expr)| {
            Stmt::Seq(
                Box::new(acc),
                Box::new(Stmt::Move(
                    Box::new(
                        record_field::<F>(Expr::Ex(IrExpr::Temp(record_temp)), idx).unwrap_ex(),
                    ),
                    Box::new(expr.unwrap_ex()),
                )),
            )
        });

    Expr::Ex(IrExpr::ESeq(
        Box::new(sequence),
        Box::new(IrExpr::Temp(record_temp)),
    ))
}

/// `arrtype [size] of init`
///  Will be converted as follows.
/// ```tiger
/// let
///     var arr := initArray(size);
///     var idx := 0;
/// in
///    while (idx < size) do (
///        arr[test] := init;
///        idx := idx + 1;
///    );
///    arr
/// end
/// ```
///
/// The type of `arr` is `&TigerArray`.
fn array_creation<F: Frame>(len: Expr, init: Expr) -> Expr {
    // mov len_temp, len()
    // mov init_temp, init()
    // mov arr_temp, allocArray(len_temp)
    let len_temp = Temp::new();
    let init_temp = Temp::new();
    let arr_temp = Temp::new();

    let stmt = Stmt::seq(vec![
        Stmt::Move(Box::new(IrExpr::Temp(len_temp)), Box::new(len.unwrap_ex())),
        Stmt::Move(
            Box::new(IrExpr::Temp(init_temp)),
            Box::new(init.unwrap_ex()),
        ),
        Stmt::Move(
            Box::new(IrExpr::Temp(arr_temp)),
            Box::new(F::extern_call("initArray", vec![IrExpr::Temp(len_temp)])),
        ),
    ]);

    // idx := 0
    let idx = IrExpr::Temp(Temp::new());
    let stmt = Stmt::seq(vec![
        stmt,
        Stmt::Move(Box::new(idx.clone()), Box::new(IrExpr::Const(0))),
    ]);

    // while idx < len {
    //   allocated_data[i] = init;
    //   idx++
    // }
    let cond = {
        let test = idx.clone();

        move |t: Label, f: Label| -> Stmt {
            Stmt::CJump(
                RelOp::Lt,
                Box::new(test),
                Box::new(IrExpr::Temp(len_temp)),
                t,
                f,
            )
        }
    };
    let cond = Expr::Cx(Box::new(cond));

    let body = Stmt::Seq(
        Box::new(Stmt::Move(
            Box::new(
                array_subscript::<F>(Expr::Ex(IrExpr::Temp(arr_temp)), Expr::Ex(idx.clone()))
                    .unwrap_ex(),
            ),
            Box::new(IrExpr::Temp(init_temp)),
        )),
        Box::new(Stmt::Move(
            Box::new(idx.clone()),
            Box::new(IrExpr::BinOp(
                BinOp::Plus,
                Box::new(idx),
                Box::new(IrExpr::Const(1)),
            )),
        )),
    );
    let body = Expr::Nx(body);

    let end_label = Label::new();
    let init = while_expr(cond, body, end_label);

    let expr = IrExpr::ESeq(
        Box::new(Stmt::seq(vec![stmt, init.unwrap_nx()])),
        Box::new(IrExpr::Temp(arr_temp)),
    );

    Expr::Ex(expr)
}

fn assign(lhs: Expr, rhs: Expr) -> Expr {
    Expr::Nx(Stmt::Move(
        Box::new(lhs.unwrap_ex()),
        Box::new(rhs.unwrap_ex()),
    ))
}

fn if_expr<F: Frame>(level: &mut Level<F>, cond: Expr, then: Expr, els: Option<Expr>) -> Expr {
    let t = Label::new();
    let f = Label::new();
    let merge = Label::new();
    let access = level.inner.frame.borrow_mut().alloc_local(false);
    let result = F::exp(access, IrExpr::Temp(F::fp()));

    let true_stmt = Stmt::seq(vec![
        Stmt::Label(t.clone()),
        Stmt::Move(Box::new(result.clone()), Box::new(then.unwrap_ex())),
        Stmt::Jump(Box::new(IrExpr::Name(merge.clone())), vec![merge.clone()]),
    ]);

    let els = els.unwrap_or_else(nop);
    let false_stmt = Stmt::seq(vec![
        Stmt::Label(f.clone()),
        Stmt::Move(Box::new(result.clone()), Box::new(els.unwrap_ex())),
        Stmt::Jump(Box::new(IrExpr::Name(merge.clone())), vec![merge.clone()]),
    ]);

    let stmt = cond.unwrap_cx()(t, f);
    Expr::Ex(IrExpr::ESeq(
        Box::new(Stmt::seq(vec![
            stmt,
            Stmt::Seq(Box::new(true_stmt), Box::new(false_stmt)),
            Stmt::Label(merge),
        ])),
        Box::new(result),
    ))
}

fn while_expr(cond: Expr, body: Expr, break_label: Label) -> Expr {
    let test = Label::new();
    let body_label = Label::new();

    let cond = cond.unwrap_cx()(body_label.clone(), break_label.clone());

    let body = Stmt::seq(vec![
        Stmt::Label(body_label),
        Stmt::Expr(Box::new(body.unwrap_ex())),
        Stmt::Jump(Box::new(IrExpr::Name(test.clone())), vec![test.clone()]),
        Stmt::Label(break_label),
    ]);

    Expr::Nx(Stmt::seq(vec![Stmt::Label(test), cond, body]))
}

fn break_expr(break_label: Label) -> Expr {
    Expr::Nx(Stmt::Jump(
        Box::new(IrExpr::Name(break_label.clone())),
        vec![break_label],
    ))
}

fn let_expr(decls: Vec<Expr>, body: Expr) -> Expr {
    let decls: Vec<_> = decls.into_iter().map(|v| v.unwrap_nx()).collect();
    let decls = Stmt::seq(decls);

    let expr = IrExpr::ESeq(Box::new(decls), Box::new(body.unwrap_ex()));

    Expr::Ex(expr)
}

fn var_decl<F: Frame>(access: Access<F>, value: Expr) -> Expr {
    let lhs = F::exp(access.1, IrExpr::Temp(F::fp()));
    Expr::Nx(Stmt::Move(Box::new(lhs), Box::new(value.unwrap_ex())))
}

fn nop() -> Expr {
    Expr::Nx(Stmt::nop())
}

pub fn translate<F: Frame>(tcx: &TyCtx, expr: &HirExpr, main_name: &str) -> Vec<Fragment<F>> {
    let mut translator = Translator::new(tcx).with_builtin();
    // if body returns int, then use that value as exit code.
    // otherwise use 0 as exit code.
    let mut main_level = Level::outermost_with_name(Label::with_named_fn(main_name.to_string()));

    let body = translator.trans_expr(expr, &mut main_level, None);
    let expr = if expr.ty == TypeId::int() {
        body
    } else {
        let seq = vec![body, num(0)];
        sequence(seq)
    };
    translator.proc_entry_exit(main_level, expr);

    translator.get_result()
}

pub struct Translator<'tcx, F: Frame> {
    string_literal_map: HashMap<Symbol, Label>,
    fragments: Vec<Fragment<F>>,

    var_env: HashMap<VarId, Access<F>>,
    pub(super) fn_env: HashMap<FnId, Level<F>>,
    tcx: &'tcx TyCtx,
}

impl<'tcx, F: Frame> Translator<'tcx, F> {
    fn new(tcx: &'tcx TyCtx) -> Self {
        Self {
            string_literal_map: HashMap::new(),
            fragments: Vec::new(),

            var_env: HashMap::new(),
            fn_env: HashMap::new(),
            tcx,
        }
    }

    fn string_literal(&mut self, string: String) -> Expr {
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
        Expr::Ex(IrExpr::Name(label))
    }

    fn proc_entry_exit(&mut self, level: Level<F>, body: Expr) {
        let body = Stmt::Move(Box::new(IrExpr::Temp(F::rv())), Box::new(body.unwrap_ex()));

        self.fragments.push(Fragment::Proc(body, level.inner.frame))
    }

    fn get_result(self) -> Vec<Fragment<F>> {
        self.fragments
    }

    fn trans_expr(
        &mut self,
        expr: &HirExpr,
        level: &mut Level<F>,
        break_label: Option<&Label>,
    ) -> Expr {
        match &expr.kind {
            ExprKind::LValue(lvar, _) => self.trans_lvalue(lvar, level, break_label),
            ExprKind::Nil(_) => num(0),
            ExprKind::Sequence(exprs, _) => {
                let exprs = exprs
                    .iter()
                    .map(|v| self.trans_expr(v, level, break_label))
                    .collect::<Vec<_>>();
                sequence(exprs)
            }
            ExprKind::Int(n, _) => num(*n as i64),
            ExprKind::Str(lit, _) => self.string_literal(lit.to_string()),
            ExprKind::FuncCall(_, fn_id, args, _) => {
                let e = self.tcx.fn_(*fn_id);
                let args = args
                    .iter()
                    .map(|v| self.trans_expr(v, level, break_label))
                    .collect::<Vec<_>>();
                let fn_level = self.fn_env.get(fn_id).expect("fn level not found");
                fn_call(e.label().clone(), fn_level, level, args)
            }
            // int only
            ExprKind::Op(
                op @ (Operator::Plus
                | Operator::Minus
                | Operator::Mul
                | Operator::Div
                | Operator::And
                | Operator::Or),
                lhs,
                rhs,
                _,
            ) => {
                let lhs = self.trans_expr(lhs, level, break_label);
                let rhs = self.trans_expr(rhs, level, break_label);
                bin_op((*op).try_into().unwrap(), lhs, rhs)
            }
            // int and string
            ExprKind::Op(
                op @ (Operator::Le | Operator::Lt | Operator::Ge | Operator::Gt),
                lhs,
                rhs,
                _,
            ) => {
                let lhs_ty = self.tcx.type_(lhs.ty);
                let rhs_ty = self.tcx.type_(rhs.ty);
                let lhs = self.trans_expr(lhs, level, break_label);
                let rhs = self.trans_expr(rhs, level, break_label);
                match (lhs_ty, rhs_ty) {
                    (Type::Int, Type::Int) => rel_op((*op).try_into().unwrap(), lhs, rhs),
                    (Type::String, Type::String) => {
                        string_ord::<F>((*op).try_into().unwrap(), lhs, rhs)
                    }
                    _ => unreachable!("ICE"),
                }
            }
            // compare two type
            ExprKind::Op(op @ (Operator::Eq | Operator::Neq), lhs, rhs, _) => {
                let ty = lhs.ty;
                let lhs = self.trans_expr(lhs, level, break_label);
                let rhs = self.trans_expr(rhs, level, break_label);
                if ty == TypeId::string() {
                    string_eq::<F>((*op).try_into().unwrap(), lhs, rhs)
                } else {
                    rel_op((*op).try_into().unwrap(), lhs, rhs)
                }
            }
            ExprKind::Neg(e, _) => {
                let e = self.trans_expr(e, level, break_label);
                neg(e)
            }
            ExprKind::RecordCreation(_, field, _) => {
                let exprs = field
                    .iter()
                    .map(|v| self.trans_expr(&v.expr, level, break_label))
                    .collect::<Vec<_>>();
                record_creation::<F>(exprs)
            }
            ExprKind::ArrayCreation { size, init, .. } => {
                let size = self.trans_expr(size, level, break_label);
                let init = self.trans_expr(init, level, break_label);
                array_creation::<F>(size, init)
            }
            ExprKind::Assign(lvalue, expr, _) => {
                let lhs = self.trans_lvalue(lvalue, level, break_label);
                let rhs = self.trans_expr(expr, level, break_label);
                assign(lhs, rhs)
            }
            ExprKind::If {
                cond, then, els, ..
            } => {
                let cond = self.trans_expr(cond, level, break_label);
                let then = self.trans_expr(then, level, break_label);
                let els = els.as_ref().map(|v| self.trans_expr(v, level, break_label));
                if_expr(level, cond, then, els)
            }
            ExprKind::While(cond, body, _) => {
                let cond = self.trans_expr(cond, level, break_label);

                let inner_break_label = Label::new();
                let body = self.trans_expr(body, level, Some(&inner_break_label));
                while_expr(cond, body, inner_break_label)
            }
            ExprKind::Break(_) => break_expr(break_label.expect("break label not found").clone()),
            ExprKind::Let(decls, exprs, _) => {
                let decls = decls
                    .iter()
                    .flat_map(|v| self.trans_decl(v, level, break_label))
                    .collect::<Vec<_>>();

                // TODO: duplicate of ExprKind::Sequence
                let exprs = exprs
                    .iter()
                    .map(|v| self.trans_expr(v, level, break_label))
                    .collect::<Vec<_>>();
                let exprs = sequence(exprs);

                let_expr(decls, exprs)
            }
        }
    }

    fn trans_lvalue(
        &mut self,
        lvar: &LValue,
        level: &mut Level<F>,
        break_label: Option<&Label>,
    ) -> Expr {
        match lvar {
            LValue::Var(_, var_id, _, _) => {
                let access = self.var_env.get(var_id).expect("var access not found");
                simple_var(access.clone(), level)
            }
            LValue::RecordField {
                record,
                field_index,
                ..
            } => {
                let var = self.trans_lvalue(record, level, break_label);
                record_field::<F>(var, *field_index)
            }
            LValue::Array { array, index, .. } => {
                let var = self.trans_lvalue(array, level, break_label);
                let subscript = self.trans_expr(index, level, break_label);
                array_subscript::<F>(var, subscript)
            }
        }
    }

    fn trans_decl(
        &mut self,
        decl: &Decl,
        parent_level: &mut Level<F>,
        break_label: Option<&Label>,
    ) -> Option<Expr> {
        match decl {
            Decl::Type(_) => None,
            Decl::Var(VarDecl(_, var_id, is_escape, _, _, expr, _)) => {
                let expr = self.trans_expr(expr, parent_level, break_label);
                let access = alloc_local(parent_level.clone(), *is_escape);
                self.var_env.insert(*var_id, access.clone());
                Some(var_decl(access, expr))
            }
            Decl::Func(fn_decls) => {
                // Define fn_level.
                for decl in fn_decls {
                    let formals_is_escape: Vec<_> =
                        decl.params.iter().map(|p| p.is_escape).collect();
                    let e = self.tcx.fn_(decl.fn_id);
                    let level =
                        new_level(parent_level.clone(), e.label().clone(), formals_is_escape);
                    self.fn_env.insert(decl.fn_id, level);
                }

                for decl in fn_decls {
                    let mut level = self
                        .fn_env
                        .get(&decl.fn_id)
                        .expect("fn level not found")
                        .clone();
                    for (param, access) in decl.params.iter().zip(level.formals()) {
                        self.var_env.insert(param.var_id, access);
                    }

                    let body = self.trans_expr(&decl.body, &mut level, break_label);
                    self.proc_entry_exit(level, body);
                }
                None
            }
        }
    }
}

static LEVEL_GLOBAL: AtomicU32 = AtomicU32::new(1);

#[derive(Debug, Eq)]
pub struct Level<F: Frame> {
    inner: LevelInner<F>,
}

impl<F: Frame> Level<F> {
    fn new(parent: Level<F>, frame: F) -> Self {
        Self {
            inner: LevelInner::new(parent, frame),
        }
    }

    pub fn outermost() -> Self {
        Self {
            inner: LevelInner::outermost(),
        }
    }

    fn outermost_with_name(name: Label) -> Self {
        Self {
            inner: LevelInner::outermost_with_name(name),
        }
    }

    fn formals(&self) -> Vec<Access<F>> {
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

    fn outermost() -> Self {
        Self::outermost_with_name(Label::new())
    }

    fn outermost_with_name(name: Label) -> Self {
        Self {
            current: 0,
            frame: Rc::new(RefCell::new(F::new(name, vec![]))),
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
