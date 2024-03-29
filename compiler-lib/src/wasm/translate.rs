use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    common::Label,
    semant::{
        builtin::{IsBuiltin, BUILTIN_FUNCS as SEMANT_BUILDIN_FUNCS},
        ctx::{FnId, TyCtx, VarId},
        hir::{
            Decl, Expr as HirExpr, ExprKind, LValue as HirLValue, Operator as HirOperator,
            Program as HirProgram, VarDecl,
        },
        types::{Type, TypeId},
    },
    wasm::{
        ast::{InlineFuncExport, TypeUse},
        builtin::{STORE_I32, STORE_I64},
    },
};

use super::{
    ast::{
        BinOp, BlockType, CvtOp, Expr, Func, Global, GlobalType, Import, ImportKind, Index,
        IndexNumber, Instruction, Limits, Memory, Module, ModuleBuilder, Mut, Name, NumType,
        Operator, TestOp, ValType,
    },
    builtin::{
        ALLOC_RECORD, ALLOC_STRING, BUILTIN_FUNCS, INIT_ARRAY, LOAD_I32, LOAD_I64, STRING_EQ,
        STRING_ORD,
    },
    concat_exprs,
    frame::{Access as FrameAccess, Frame, Size},
    nop, ExprType, StackType, FRAME_ADDR, FRAME_PTR, JS_OBJECT_NAME, MAIN_SYMBOL, STACK_ADDR,
    STACK_PTR,
};

pub fn translate(tcx: &TyCtx, hir: &HirProgram) -> Module {
    let mut wasm = Wasm::new(tcx);
    let outermost_level = Level::outermost_with_name(MAIN_SYMBOL);
    let expr = wasm.trans_expr(hir, outermost_level.clone(), BreakContext::new().entered());
    let expr = if hir.ty == TypeId::INT {
        expr
    } else {
        sequence(vec![expr, num(0, NumType::I32)])
    };
    wasm.proc_entry_exit(outermost_level, StackType::nop(), expr);

    let funcs = wasm.funcs;
    let globals = vec![
        Global {
            name: Some(Name::new(STACK_PTR.to_string())),
            ty: GlobalType {
                ty: ValType::Num(NumType::I32),
                m: Mut::Var,
            },
            init: num(STACK_ADDR, NumType::I32).val,
        },
        Global {
            name: Some(Name::new(FRAME_PTR.to_string())),
            ty: GlobalType {
                ty: ValType::Num(NumType::I32),
                m: Mut::Var,
            },
            init: num(FRAME_ADDR, NumType::I32).val,
        },
    ];

    let memory = Memory {
        name: None,
        ty: Limits { min: 16, max: None },
    };

    let imports = BUILTIN_FUNCS
        .keys()
        .map(|&name| Import {
            module: JS_OBJECT_NAME.to_string(),
            name: name.to_string(),
            kind: ImportKind::Func(
                Some(Index::Name(name.into())),
                TypeUse::Inline(BUILTIN_FUNCS[name].clone()),
            ),
        })
        .collect();

    let builder = ModuleBuilder::new();

    builder
        .add_funcs(funcs)
        .add_globals(globals)
        .add_memory(memory)
        .add_imports(imports)
        .build()
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Level(Rc<RefCell<LevelInner>>);

impl Level {
    fn new(parent_level: Level, name: Label, mut formals: Vec<(bool, ValType)>) -> Level {
        // for static link
        formals.insert(0, (true, ValType::Num(NumType::I32)));
        Level(Rc::new(RefCell::new(LevelInner::new(
            parent_level,
            name,
            formals,
        ))))
    }

    fn outermost() -> Self {
        Level(Rc::new(RefCell::new(LevelInner::outermost())))
    }

    fn outermost_with_name(name: impl Into<String>) -> Self {
        Level(Rc::new(RefCell::new(LevelInner::outermost_with_name(name))))
    }

    fn formals(self) -> Vec<Access> {
        self.0
            .borrow()
            .formals()
            .iter()
            .map(|access| (access.clone(), self.clone()))
            .collect()
    }

    fn frame(&self) -> Ref<Frame> {
        Ref::map(self.0.borrow(), |v| &v.frame)
    }

    fn frame_mut(&self) -> RefMut<Frame> {
        RefMut::map(self.0.borrow_mut(), |v| &mut v.frame)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct LevelInner {
    frame: Frame,
    parent: Option<Level>,
}

impl LevelInner {
    fn outermost() -> Self {
        Self {
            frame: Frame::new(Label::new(), vec![]),
            parent: None,
        }
    }

    fn outermost_with_name(name: impl Into<String>) -> Self {
        Self {
            frame: Frame::new(Label::NamedFn(name.into()), vec![]),
            parent: None,
        }
    }

    fn new(parent: Level, name: Label, formals: Vec<(bool, ValType)>) -> Self {
        Self {
            frame: Frame::new(name, formals),
            parent: Some(parent),
        }
    }

    fn formals(&self) -> &[FrameAccess] {
        self.frame.formals()
    }
}

type Access = (FrameAccess, Level);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BreakContext {
    depth: IndexNumber,
}

impl BreakContext {
    fn new() -> Self {
        Self { depth: 0 }
    }

    // Op::Blockがある時はこれを呼ぶ
    fn entered(self) -> Self {
        Self {
            depth: self.depth + 1,
        }
    }

    fn into_expr(self) -> ExprType {
        ExprType::new(
            Expr::Op(Operator::Br(Index::Index(self.depth as IndexNumber))),
            StackType::nop(),
        )
    }
}

struct Wasm<'tcx> {
    var_env: HashMap<VarId, Access>,
    fn_env: HashMap<FnId, Level>,
    tcx: &'tcx TyCtx,

    funcs: Vec<Func>,
}

impl<'tcx> Wasm<'tcx> {
    pub fn new(tcx: &'tcx TyCtx) -> Self {
        let mut fn_env = HashMap::new();
        // TODO
        for (_, id, _, _) in SEMANT_BUILDIN_FUNCS {
            fn_env.insert(*id, Level::outermost());
        }

        Self {
            var_env: HashMap::new(),
            fn_env,
            tcx,
            funcs: Vec::new(),
        }
    }

    fn trans_expr(&mut self, expr: &HirExpr, level: Level, bcx: BreakContext) -> ExprType {
        match &expr.kind {
            ExprKind::LValue(lvalue, _) => self.load_lvalue(lvalue, level, bcx),
            ExprKind::Nil(_) => num(0, NumType::I32).add_comment("mil"),
            ExprKind::Sequence(exprs, _) => {
                let exprs = exprs
                    .iter()
                    .map(|expr| self.trans_expr(expr, level.clone(), bcx.entered()))
                    .collect();
                sequence(exprs).add_comment("seq")
            }
            ExprKind::Int(v, _) => num(*v as i64, NumType::I32).add_comment("int"),
            ExprKind::Str(s, _) => {
                let mut frame = level.frame_mut();
                string_literal(&mut frame, s.as_str())
            }
            ExprKind::FuncCall(_, fn_id, args, _) => {
                let entry = self.tcx.fn_(*fn_id);
                let ret_ty = self.tcx.type_(entry.result());
                let ret_ty = convert_ty(ret_ty).into_iter().collect();
                let ret_ty = StackType::new(vec![], ret_ty);

                let args_expr = args
                    .iter()
                    .map(|a| {
                        let e = self.trans_expr(a, level.clone(), bcx);
                        e.val
                    })
                    .collect();
                let fn_level = self.fn_env.get(fn_id).expect("level not found");

                fn_call(entry.label(), fn_level.clone(), level, ret_ty, args_expr)
                    .add_comment("fn call")
            }
            ExprKind::Op(
                op @ (HirOperator::Plus
                | HirOperator::Minus
                | HirOperator::Mul
                | HirOperator::Div
                | HirOperator::And
                | HirOperator::Or),
                lhs,
                rhs,
                _,
            ) => {
                let lhs = self.trans_expr(lhs, level.clone(), bcx);
                let rhs = self.trans_expr(rhs, level, bcx);
                bin_op((*op).try_into().unwrap(), lhs, rhs).add_comment("bin op")
            }
            // int and string
            ExprKind::Op(
                op @ (HirOperator::Le | HirOperator::Lt | HirOperator::Ge | HirOperator::Gt),
                lhs,
                rhs,
                _,
            ) => {
                let lhs_ty = self.tcx.type_(lhs.ty);
                let rhs_ty = self.tcx.type_(rhs.ty);
                let lhs = self.trans_expr(lhs, level.clone(), bcx);
                let rhs = self.trans_expr(rhs, level, bcx);
                match (lhs_ty, rhs_ty) {
                    (Type::Int, Type::Int) => bin_op((*op).try_into().unwrap(), lhs, rhs),
                    (Type::String, Type::String) => string_ord((*op).try_into().unwrap(), lhs, rhs),
                    _ => unreachable!("ICE"),
                }
            }
            // compare two type
            ExprKind::Op(op @ (HirOperator::Eq | HirOperator::Neq), lhs, rhs, _) => {
                let ty = lhs.ty;
                let lhs = self.trans_expr(lhs, level.clone(), bcx);
                let rhs = self.trans_expr(rhs, level, bcx);
                if ty == TypeId::STRING {
                    string_eq((*op).try_into().unwrap(), lhs, rhs)
                } else {
                    bin_op((*op).try_into().unwrap(), lhs, rhs)
                }
            }

            ExprKind::Neg(expr, _) => {
                let expr = self.trans_expr(expr, level, bcx);
                assert!(expr.ty.is_const_1());
                let ValType::Num(ty) = expr.ty.result()[0];
                bin_op(BinOp::Sub, num(0, ty), expr)
            }
            ExprKind::RecordCreation(_, field, _) => {
                let exprs = field
                    .iter()
                    .map(|v| self.trans_expr(&v.expr, level.clone(), bcx))
                    .collect::<Vec<_>>();
                record_creation(&mut level.frame_mut(), exprs)
            }
            ExprKind::ArrayCreation { size, init, .. } => {
                let size = self.trans_expr(size, level.clone(), bcx);
                let init = self.trans_expr(init, level.clone(), bcx);
                let mut frame = level.frame_mut();
                array_creation(&mut frame, size, init).add_comment("array creation")
            }
            ExprKind::Assign(lvalue, expr, _) => {
                let expr = self.trans_expr(expr, level.clone(), bcx);
                self.store_lvalue(lvalue, level, expr, bcx)
                    .add_comment("assign")
            }
            ExprKind::If {
                cond, then, els, ..
            } => {
                let mut cond = self.trans_expr(cond, level.clone(), bcx);
                let then = self.trans_expr(then, level.clone(), bcx.entered());
                let els = els
                    .as_deref()
                    .map(|els| self.trans_expr(els, level, bcx.entered()));

                if cond.ty.is_const_1_i64() {
                    cond = i64_2_i32(cond);
                }

                if_expr(cond, then, els).add_comment("if expr")
            }
            ExprKind::While(cond, instr, _) => {
                let cond = self.trans_expr(cond, level.clone(), bcx);
                let body = self.trans_expr(instr, level, BreakContext::new().entered());
                while_expr(cond, body).add_comment("while expr")
            }
            ExprKind::Break(_) => bcx.into_expr().add_comment("break"),
            ExprKind::Let(decls, exprs, _) => {
                let mut decls = decls
                    .iter()
                    .flat_map(|v| self.trans_decl(v, level.clone(), bcx.entered()))
                    .collect::<Vec<_>>();

                let exprs = exprs
                    .iter()
                    .map(|v| self.trans_expr(v, level.clone(), bcx.entered()))
                    .collect::<Vec<_>>();
                let exprs = sequence(exprs);

                decls.push(exprs);

                concat_exprs(decls).add_comment("expr seq")
            }
        }
    }

    fn store_lvalue(
        &mut self,
        lvalue: &HirLValue,
        level: Level,
        expr: ExprType,
        bcx: BreakContext,
    ) -> ExprType {
        match lvalue {
            HirLValue::Var(_, var_id, _, _) => {
                let (access, ancestor_level) = self.var_env.get(var_id).expect("access not found");
                let env = calc_static_link(level.clone(), ancestor_level.clone());
                let frame = level.frame();
                frame.store2access(access, env, expr)
            }
            HirLValue::RecordField {
                record,
                field_index,
                ..
            } => {
                let var = self.load_lvalue(record, level, bcx);
                expr.assert_ty(StackType::const_1_i32());

                Frame::extern_call(
                    STORE_I32,
                    vec![record_field(var, *field_index), expr],
                    vec![],
                )
            }
            HirLValue::Array {
                array,
                index,
                array_type,
                ..
            } => {
                let Type::Array { ty, ..} = self.tcx.type_(*array_type) else {
                    unreachable!("not array");
                };
                let ty = self.tcx.type_(*ty);
                let ValType::Num(num_ty) = convert_ty(ty).unwrap();

                let var = self.load_lvalue(array, level.clone(), bcx);
                let subscript = self.trans_expr(index, level, bcx);

                let store_fn = match num_ty {
                    NumType::I32 => STORE_I32,
                    NumType::I64 => STORE_I64,
                    _ => unimplemented!(),
                };

                assert_eq!(expr.ty.result()[0], ValType::Num(num_ty));
                Frame::extern_call(
                    store_fn,
                    vec![array_subscript(num_ty, var, subscript), expr],
                    vec![],
                )
            }
        }
    }

    fn load_lvalue(&mut self, lvalue: &HirLValue, level: Level, bcx: BreakContext) -> ExprType {
        match lvalue {
            HirLValue::Var(_, var_id, _, _) => {
                let (access, ancestor_level) = self.var_env.get(var_id).expect("access not found");
                let env = calc_static_link(level.clone(), ancestor_level.clone());
                let frame = level.frame();

                frame.get_access_content(access, env)
            }
            HirLValue::RecordField {
                record,
                field_index,
                ..
            } => {
                let var = self.load_lvalue(record, level, bcx);
                Frame::extern_call(
                    LOAD_I32,
                    vec![record_field(var, *field_index)],
                    vec![ValType::Num(NumType::I32)],
                )
            }
            HirLValue::Array {
                array,
                index,
                array_type,
                ..
            } => {
                let Type::Array { ty, ..} = self.tcx.type_(*array_type) else {
                    unreachable!("not array");
                };
                let ty = self.tcx.type_(*ty);
                let val_type @ ValType::Num(num_ty) = convert_ty(ty).unwrap();

                let var = self.load_lvalue(array, level.clone(), bcx);
                let subscript = self.trans_expr(index, level, bcx);

                let load_fn = match num_ty {
                    NumType::I32 => LOAD_I32,
                    NumType::I64 => LOAD_I64,
                    _ => unimplemented!(),
                };
                Frame::extern_call(
                    load_fn,
                    vec![array_subscript(num_ty, var, subscript)],
                    vec![val_type],
                )
            }
        }
    }

    fn trans_decl(
        &mut self,
        decl: &Decl,
        parent_level: Level,
        bcx: BreakContext,
    ) -> Option<ExprType> {
        match decl {
            Decl::Type(_) => None,
            Decl::Var(VarDecl(_, var_id, is_escape, _, ty_id, expr, _)) => {
                let expr = self.trans_expr(expr, parent_level.clone(), bcx);

                let mut frame = parent_level.frame_mut();
                let ty = self.tcx.type_(*ty_id);
                let access = frame.alloc_local(*is_escape, convert_ty(ty).unwrap());

                let base_addr = Frame::fp();
                let store_expr = frame.store2access(&access, base_addr, expr);
                drop(frame);

                self.var_env.insert(*var_id, (access, parent_level));
                Some(store_expr.add_comment("decl var"))
            }
            Decl::Func(fn_decls) => {
                for decl in fn_decls {
                    let formals_is_escape: Vec<_> = decl
                        .params
                        .iter()
                        .map(|p| {
                            let ty = self.tcx.type_(p.type_id);
                            let is_escape = p.is_escape;
                            (is_escape, convert_ty(ty).unwrap())
                        })
                        .collect();
                    let entry = self.tcx.fn_(decl.fn_id);
                    let level = Level::new(
                        parent_level.clone(),
                        entry.label().clone(),
                        formals_is_escape,
                    );
                    self.fn_env.insert(decl.fn_id, level);
                }

                for decl in fn_decls {
                    let level = self
                        .fn_env
                        .get(&decl.fn_id)
                        .expect("level not found")
                        .clone();
                    for (param, access) in decl
                        .params
                        .iter()
                        // skip static link
                        .zip(level.clone().formals().into_iter().skip(1))
                    {
                        self.var_env.insert(param.var_id, access);
                    }

                    let body = self.trans_expr(&decl.body, level.clone(), bcx);

                    let mut args_ty: Vec<_> = decl
                        .params
                        .iter()
                        .map(|p| convert_ty(self.tcx.type_(p.type_id)).unwrap())
                        .collect();
                    // add static link
                    args_ty.insert(0, ValType::Num(NumType::I32));
                    let args_ty = StackType::new(args_ty, vec![]);

                    self.proc_entry_exit(level, args_ty, body);
                }

                None
            }
        }
    }

    fn proc_entry_exit(&mut self, level: Level, args_ty: StackType, body: ExprType) {
        let frame = level.frame();
        let body = frame.proc_entry_exit1(body);
        let body = frame.proc_entry_exit3(body);

        let name = Name::from(format_label(level.frame().name()));
        let export = match level.frame().name() {
            Label::NamedFn(name) => Some(InlineFuncExport { name: name.into() }),
            _ => None,
        };
        // arg_ty has only arg type.
        // so this is unfallable.
        let ty = args_ty.composition(&body.ty).expect("type error");
        let ty = TypeUse::Inline(ty.into());
        let locals = level.frame().ast_locals();

        // prologue

        self.funcs.push(Func {
            name: Some(name),
            ty,
            export,
            locals,
            instr: vec![Instruction::Expr(body.val)],
        });
    }
}

fn format_label(label: &Label) -> String {
    match label {
        Label::Num(_) | Label::Fn(_, _) => {
            format!("L.{}", label)
        }
        Label::NamedFn(s) => {
            // Calling function named `exit` is not working correctry.
            // So rename it to `tiger_exit`.
            if s == "exit" {
                "tiger_exit".to_string()
            } else {
                format!("{}", label)
            }
        }
    }
}

fn calc_static_link(mut cur_level: Level, ancestor_level: Level) -> ExprType {
    let mut link = Frame::fp();
    if cur_level == ancestor_level {
        return link;
    }

    while cur_level != ancestor_level {
        let frame = cur_level.frame();
        let link_access = frame.env();
        link = frame.get_access_content(link_access.expect("cur_level is outer most"), link);
        let parent = cur_level
            .0
            .borrow()
            .parent
            .as_ref()
            .expect("cur_level is root")
            .clone();
        drop(frame);

        cur_level = parent;
    }

    link
}

fn num(v: i64, ty: NumType) -> ExprType {
    ExprType::new_const_1(Expr::Op(Operator::Const(ty, v)), ValType::Num(ty))
}

fn bin_op(op: BinOp, lhs: ExprType, rhs: ExprType) -> ExprType {
    lhs.assert_eq_ty(&rhs);
    assert!(lhs.ty.is_const_1());
    assert!(rhs.ty.is_const_1());

    let ValType::Num(operand_ty) = lhs.ty.result()[0];

    let result_ty = match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::DivSigned => operand_ty,
        BinOp::Eq
        | BinOp::Ne
        | BinOp::LessThanSigned
        | BinOp::LessOrEqualSigned
        | BinOp::GreaterThanSigned
        | BinOp::GreaterOrEqualSigned => NumType::I32,
    };

    ExprType::new_const_(
        Expr::OpExpr(Operator::Bin(operand_ty, op), vec![lhs.val, rhs.val]),
        vec![ValType::Num(result_ty)],
    )
}

fn string_ord(op: BinOp, lhs: ExprType, rhs: ExprType) -> ExprType {
    let n = match op {
        BinOp::LessOrEqualSigned => 1,
        BinOp::LessThanSigned => 2,
        BinOp::GreaterOrEqualSigned => 3,
        BinOp::GreaterThanSigned => 4,
        _ => panic!("unexpected op"),
    };

    Frame::extern_call(
        STRING_ORD,
        vec![num(n, NumType::I32), lhs, rhs],
        vec![ValType::Num(NumType::I32)],
    )
}

fn string_eq(op: BinOp, lhs: ExprType, rhs: ExprType) -> ExprType {
    let call = Frame::extern_call(STRING_EQ, vec![lhs, rhs], vec![ValType::Num(NumType::I32)]);
    match op {
        BinOp::Eq => call,
        BinOp::Ne => bin_op(BinOp::Sub, num(1, NumType::I32), call),
        _ => unreachable!("ICE"),
    }
}

fn if_expr(cond: ExprType, then: ExprType, els: Option<ExprType>) -> ExprType {
    cond.assert_ty(StackType::const_1_i32());
    if let Some(ref els) = els {
        then.assert_eq_ty(els);
    }

    let expr = Expr::If(
        None,
        BlockType(TypeUse::Inline(then.ty.clone().into())),
        vec![cond.val],
        vec![Instruction::Expr(then.val)],
        els.map(|v| vec![Instruction::Expr(v.val)]),
    );

    ExprType::new(expr, then.ty)
}

fn while_expr(cond: ExprType, body: ExprType) -> ExprType {
    cond.assert_ty(StackType::const_1_i32());
    let block_ty = BlockType(TypeUse::Inline(body.ty.clone().into()));
    let stack_type = body.ty;

    let body = Expr::Block(
        None,
        block_ty.clone(),
        vec![Instruction::Expr(Expr::Loop(
            None,
            block_ty,
            vec![
                Instruction::Expr(Expr::OpExpr(
                    Operator::BrIf(Index::from(1)),
                    vec![Expr::OpExpr(
                        Operator::Test(NumType::I32, TestOp::Eqz),
                        vec![cond.val],
                    )],
                )),
                Instruction::Expr(body.val),
                Instruction::Op(Operator::Br(Index::from(0))),
            ],
        ))],
    );

    ExprType::new(body, stack_type)
}

/// discard result of exprs except last one.
fn sequence(mut exprs: Vec<ExprType>) -> ExprType {
    if exprs.is_empty() {
        return nop();
    }

    let last = exprs.pop().unwrap();
    let mut exprs = exprs
        .into_iter()
        .map(|expr| {
            if expr.ty.is_const_1() {
                drop_(expr)
            } else if expr.ty.is_nop() {
                expr
            } else {
                unimplemented!()
            }
        })
        .collect::<Vec<_>>();
    exprs.push(last);

    concat_exprs(exprs)
}

fn drop_(expr: ExprType) -> ExprType {
    let expr = Expr::OpExpr(Operator::Drop, vec![expr.val]);
    ExprType::new(expr, StackType::nop())
}

fn i64_2_i32(expr: ExprType) -> ExprType {
    expr.assert_ty(StackType::const_1_i64());

    let expr = Expr::OpExpr(
        Operator::Convert(NumType::I32, NumType::I64, CvtOp::Wrap, None),
        vec![expr.val],
    );
    ExprType::new_const_1_i32(expr)
}

fn convert_ty(ty: &Type) -> Option<ValType> {
    match ty {
        Type::String | Type::Record { .. } | Type::Array { .. } | Type::Nil | Type::Int => {
            Some(ValType::Num(NumType::I32))
        }
        Type::Unit => None,
    }
}

fn fn_call(
    label: &Label,
    fn_level: Level,
    cur_level: Level,
    ret_ty: StackType,
    mut args: Vec<Expr>,
) -> ExprType {
    if !label.is_builtin() {
        // 関数には、その関数の親のフレームへのポインタが渡される。
        // 関数の宣言されたレベル(fn_level)の親のレベルを渡す。
        // 一番外側だったら、自分自身のレベルを渡す。
        let parent_level = fn_level
            .0
            .borrow()
            .parent
            .clone()
            .unwrap_or(cur_level.clone());
        let link = calc_static_link(cur_level, parent_level);
        link.assert_ty(StackType::const_1_i32());
        args.insert(0, link.val);
    }

    let expr = Expr::OpExpr(
        Operator::Call(Index::Name(format_label(label).into())),
        args,
    );
    ExprType::new(expr, ret_ty)
}

/// `arrtype [size] of init`
///  Will be converted as follows.
/// ```tiger
/// let
///     var arr := initArray(size);
///     var idx := 0;
/// in
///    while (idx < size) do (
///        arr[idx] := init;
///        idx := idx + 1;
///    );
///    arr
/// end
/// ```
///
/// (local $size i32) (local $arr i32) (local $init (typeof `init`)) (local $idx i32)
///     local.set $size `size`
///     local.set $arr (call $initArray
///         (i32.mul (local.get $size) (word_size_of `arrtype`)))
///     local.set $init `init`
///     local.set $idx (i32.const 0)
///     while (i32.lt_u (local.get $idx) (local.get $size)) do (
///         call $store_`type of init` (array_subscript, init)
///         local.set $idx (i32.add (local.get $idx) (i32.const 1))
///
/// The type of `arr` is `&TigerArray`.
fn array_creation(frame: &mut Frame, size: ExprType, init: ExprType) -> ExprType {
    size.assert_ty(StackType::const_1_i32());

    let mut exprs = Vec::new();

    // initialize `size: i32`
    let size_num_ty = NumType::I32;
    let (size_access, store2size) = frame.init_local(false, size);
    exprs.push(store2size);

    // initialize `init`
    let init_ty @ ValType::Num(init_num_ty) = init.ty.result()[0];
    let init_access = frame.alloc_local(false, init_ty);
    let store2init = frame.store2access(&init_access, Frame::fp(), init);
    exprs.push(store2init);

    // initialize `word_size: i32`
    // word size of `init`
    let (word_size_access, store2word_size) =
        frame.init_local(false, num(init_ty.word_size() as i64, NumType::I32));
    exprs.push(store2word_size);

    // initialize `arr: *TigerArray`,
    let arr = Frame::extern_call(
        INIT_ARRAY,
        vec![bin_op(
            BinOp::Mul,
            frame.get_access_content(&word_size_access, Frame::fp()),
            frame.get_access_content(&size_access, Frame::fp()),
        )],
        vec![ValType::Num(NumType::I32)],
    );
    let (arr_access, store2arr) = frame.init_local(false, arr);
    exprs.push(store2arr);

    // initialize `idx`: i32
    let idx_num_ty = NumType::I32;
    let (idx_access, store2idx) = frame.init_local(false, num(0, idx_num_ty));
    exprs.push(store2idx);
    assert_eq!(idx_num_ty, size_num_ty);

    // idx < size
    let cond = bin_op(
        BinOp::LessThanSigned,
        frame.get_access_content(&idx_access, Frame::fp()),
        frame.get_access_content(&size_access, Frame::fp()),
    );

    //     arr[idx] := init;
    let load_arr = frame.get_access_content(&arr_access, Frame::fp());
    let load_idx = frame.get_access_content(&idx_access, Frame::fp());
    let ValType::Num(init_ty) = init_ty;

    let store_fn = match init_ty {
        NumType::I32 => STORE_I32,
        NumType::I64 => STORE_I64,
        _ => unimplemented!(),
    };

    let body1 = Frame::extern_call(
        store_fn,
        vec![
            array_subscript(init_num_ty, load_arr, load_idx),
            frame.get_access_content(&init_access, Frame::fp()),
        ],
        vec![],
    );

    // idx := idx + 1;
    let body2 = frame.store2access(
        &idx_access,
        Frame::fp(),
        bin_op(
            BinOp::Add,
            frame.get_access_content(&idx_access, Frame::fp()),
            num(1, idx_num_ty),
        ),
    );

    let body = ExprType::new(concat_exprs(vec![body1, body2]).val, StackType::nop());

    let w = while_expr(cond, body);
    exprs.push(w);

    let load_arr = frame.get_access_content(&arr_access, Frame::fp());
    exprs.push(load_arr);

    concat_exprs(exprs)
}

/// `arr_content_ty` is type of `arr[idx]`.
/// `var` is address of array.
/// `subscript` is index of array.
/// return address of `arr[idx]` on stack top
///
/// pub struct TigerArray {
///     len: TigerInt,
///     data: *mut TigerInt,
/// }
fn array_subscript(arr_content_ty: NumType, var: ExprType, subscript: ExprType) -> ExprType {
    var.assert_ty(StackType::const_1_i32());
    subscript.assert_ty(StackType::const_1_i32());

    // addr of `TigerArray.data`
    let addr_of_data = bin_op(BinOp::Add, var, num(4, NumType::I32));

    // `data`: *mut TIgerInt
    let data = Frame::extern_call(
        LOAD_I32,
        vec![addr_of_data],
        vec![ValType::Num(NumType::I32)],
    );

    // data + subscript * WORD_SIZE * data_word_size
    // ex) data + subscript * 4 * 2 // i64

    bin_op(
        BinOp::Add,
        data,
        bin_op(
            BinOp::Mul,
            subscript,
            num(
                (Frame::WORD_SIZE * arr_content_ty.word_size()) as i64,
                NumType::I32,
            ),
        ),
    )
}

/// `exprs` is address of record fields.
/// for simplicity, all fields are i32.
/// return address of record
fn record_creation(frame: &mut Frame, exprs: Vec<ExprType>) -> ExprType {
    let record_size: usize = exprs
        .iter()
        .map(|v| {
            assert!(v.ty.is_const_1_i32());
            v.ty.result()[0].word_size()
        })
        .sum();
    let record_size = num(record_size as i64, NumType::I32);

    // TODO: ret_tyをBUILTIN[ALLOC_RECORD]とかにしたい
    let record = Frame::extern_call(
        ALLOC_RECORD,
        vec![record_size],
        vec![ValType::Num(NumType::I32)],
    );

    let (record_access, store2record) = frame.init_local(false, record);

    let mut result = vec![store2record];

    let init = exprs.into_iter().enumerate().map(|(idx, e)| {
        let field_addr = record_field(frame.get_access_content(&record_access, Frame::fp()), idx);
        Frame::extern_call(STORE_I32, vec![field_addr, e], vec![])
    });
    result.extend(init);

    let load_record = frame.get_access_content(&record_access, Frame::fp());
    result.push(load_record);

    concat_exprs(result)
}

/// `var` is address of record.
/// returns addr of `var.field_index`
/// for simplicity, all fields are i32.
fn record_field(var: ExprType, field_index: usize) -> ExprType {
    var.assert_ty(StackType::const_1_i32());

    // addr of `TigerArray.data`
    let addr_of_data = bin_op(BinOp::Add, var, num(Frame::WORD_SIZE as i64, NumType::I32));

    bin_op(
        BinOp::Add,
        addr_of_data,
        num(
            (Frame::WORD_SIZE * NumType::I32.word_size() * field_index) as i64,
            NumType::I32,
        ),
    )
}

/// addr of string
/// pub struct TigerSting {
///     len: TigerInt,
///     data: *mut u8,
/// }
fn string_literal(frame: &mut Frame, s: &str) -> ExprType {
    let mut exprs = Vec::new();

    // *TigerString
    let addr = Frame::extern_call(
        ALLOC_STRING,
        vec![num(s.as_bytes().len() as i64, NumType::I32)],
        vec![ValType::Num(NumType::I32)],
    );
    let (addr_access, store) = frame.init_local(false, addr);
    exprs.push(store);

    // TigerString.len = s.len()
    let store_len = Frame::extern_call(
        STORE_I32,
        vec![
            frame.get_access_content(&addr_access, Frame::fp()),
            num(s.chars().count() as i64, NumType::I32),
        ],
        vec![],
    );
    exprs.push(store_len);

    // `(*TigerString).data`
    let addr_of_data = bin_op(
        BinOp::Add,
        frame.get_access_content(&addr_access, Frame::fp()),
        num(Frame::WORD_SIZE as i64, NumType::I32),
    );

    // `data`: *mut TIgerInt
    let data = Frame::extern_call(
        LOAD_I32,
        vec![addr_of_data],
        vec![ValType::Num(NumType::I32)],
    );
    let (data_access, store) = frame.init_local(false, data);
    exprs.push(store);

    let inits = s.as_bytes().iter().enumerate().map(|(idx, v)| {
        // TigerString.data[i] = s[i]
        Frame::extern_call(
            STORE_I32,
            vec![
                bin_op(
                    BinOp::Add,
                    frame.get_access_content(&data_access, Frame::fp()),
                    num(idx as i64, NumType::I32),
                ),
                num(*v as i64, NumType::I32),
            ],
            vec![],
        )
    });

    exprs.extend(inits);
    exprs.push(frame.get_access_content(&addr_access, Frame::fp()));

    concat_exprs(exprs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_composition() {
        let test_cases = [
            (
                StackType::new(
                    vec![ValType::Num(NumType::I32), ValType::Num(NumType::I64)],
                    vec![ValType::Num(NumType::I64)],
                ),
                StackType::new(
                    vec![ValType::Num(NumType::I64)],
                    vec![ValType::Num(NumType::F64)],
                ),
                StackType::new(
                    vec![ValType::Num(NumType::I32), ValType::Num(NumType::I64)],
                    vec![ValType::Num(NumType::F64)],
                ),
            ),
            (
                StackType::new(
                    vec![ValType::Num(NumType::I32)],
                    vec![
                        ValType::Num(NumType::I64),
                        ValType::Num(NumType::F32),
                        ValType::Num(NumType::F64),
                    ],
                ),
                StackType::new(
                    vec![ValType::Num(NumType::F32), ValType::Num(NumType::F64)],
                    vec![ValType::Num(NumType::F32)],
                ),
                StackType::new(
                    vec![ValType::Num(NumType::I32)],
                    vec![ValType::Num(NumType::I64), ValType::Num(NumType::F32)],
                ),
            ),
            (
                StackType::new(
                    vec![ValType::Num(NumType::I32)],
                    vec![ValType::Num(NumType::I64)],
                ),
                StackType::new(
                    vec![
                        ValType::Num(NumType::F32),
                        ValType::Num(NumType::F64),
                        ValType::Num(NumType::I64),
                    ],
                    vec![ValType::Num(NumType::F32)],
                ),
                StackType::new(
                    vec![
                        ValType::Num(NumType::F32),
                        ValType::Num(NumType::F64),
                        ValType::Num(NumType::I32),
                    ],
                    vec![ValType::Num(NumType::F32)],
                ),
            ),
        ];

        for (lhs, rhs, expected) in test_cases.iter() {
            let result = lhs.composition(rhs).unwrap();
            assert_eq!(result, *expected);
        }
    }
}
