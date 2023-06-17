//! sp -> stack_ptr
//! fp -> frame_ptr
//! address is 32bit
//! args are 64bit except first arg which means static link.

mod ast;
pub mod encode;
mod frame;
pub mod rewrite;
mod watencoder;
pub use encode::Encoder as WasmEncoder;
pub use watencoder::Encoder as WatEncoder;
mod builtin;

use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    common::Label,
    semant::{
        ctx::{FnId, TyCtx, VarId},
        hir::{
            Decl, Expr as HirExpr, ExprKind, LValue as HirLValue, Program as HirProgram, VarDecl,
        },
        types::Type,
    },
    wasm::{
        ast::{InlineFuncExport, TypeUse},
        builtin::{STORE_I32, STORE_I64},
    },
};

use self::{
    ast::{
        BinOp, BlockType, CvtOp, Expr, Func, FuncType, Global, GlobalType, Import, ImportKind,
        Index, Instruction, Limits, Memory, Module, ModuleBuilder, Mut, Name, NumType, Operator,
        Param, TestOp, ValType,
    },
    builtin::{BUILTIN_FUNCS, INIT_ARRAY, LOAD_I32, LOAD_I64},
    frame::{Access as FrameAccess, Frame, Size},
};

type ExprType = WithType<Expr>;

#[derive(Debug, Clone)]
struct WithType<T> {
    ty: StackType,
    val: T,
}

impl ExprType {
    pub fn new(expr: Expr, ty: StackType) -> Self {
        Self { val: expr, ty }
    }

    pub fn new_const_(expr: Expr, result: Vec<ValType>) -> Self {
        Self {
            val: expr,
            ty: StackType::const_(result),
        }
    }

    pub fn new_const_1(expr: Expr, result: ValType) -> Self {
        Self {
            val: expr,
            ty: StackType::new_const_1(result),
        }
    }

    pub fn new_const_1_i32(expr: Expr) -> Self {
        Self::new_const_(expr, vec![ValType::Num(NumType::I32)])
    }

    pub fn new_const_1_i64(expr: Expr) -> Self {
        Self::new_const_(expr, vec![ValType::Num(NumType::I64)])
    }

    pub fn assert_ty(&self, ty: StackType) {
        assert_eq!(self.ty, ty);
    }

    pub fn assert_eq_ty(&self, other: &Self) {
        assert_eq!(self.ty, other.ty);
    }

    pub fn add_comment(mut self, comment: impl Into<String>) -> Self {
        self.val = self.val.add_comment(comment);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackType {
    arg: Vec<ValType>,
    result: Vec<ValType>,
}

impl StackType {
    pub fn new(arg: Vec<ValType>, result: Vec<ValType>) -> Self {
        Self { arg, result }
    }

    pub fn const_(result: Vec<ValType>) -> Self {
        Self::new(Vec::new(), result)
    }

    pub fn new_const_1(result: ValType) -> Self {
        Self::new(Vec::new(), vec![result])
    }

    pub fn nop() -> Self {
        Self {
            arg: Vec::new(),
            result: Vec::new(),
        }
    }

    pub fn composition(&self, other: &Self) -> Result<Self, ()> {
        // type check
        let is_same_type = self
            .result
            .iter()
            .rev()
            .zip(other.arg.iter().rev())
            .all(|(a, b)| a == b);

        if !is_same_type {
            return Err(());
        }

        let arg_stick_out = other.arg.len().saturating_sub(self.result.len());
        let arg: Vec<ValType> = other.arg[..arg_stick_out]
            .iter()
            .chain(self.arg.iter())
            .cloned()
            .collect();

        let result_stick_out = self.result.len().saturating_sub(other.arg.len());
        let result: Vec<ValType> = self.result[..result_stick_out]
            .iter()
            .chain(other.result.iter())
            .cloned()
            .collect();

        Ok(Self { arg, result })
    }

    pub fn const_1_i32() -> StackType {
        StackType::new(vec![], vec![ValType::Num(NumType::I32)])
    }

    pub fn is_const_1_i64(&self) -> bool {
        self.is_const_1() && self.result[0] == ValType::Num(NumType::I64)
    }

    pub fn const_1_i64() -> StackType {
        StackType::new(vec![], vec![ValType::Num(NumType::I64)])
    }

    pub fn is_const_1(&self) -> bool {
        self.arg.is_empty() && self.result.len() == 1
    }

    pub fn is_nop(&self) -> bool {
        self.arg.is_empty() && self.result.is_empty()
    }
}

impl From<StackType> for FuncType {
    fn from(value: StackType) -> Self {
        Self {
            params: value.arg.into_iter().map(Into::into).collect(),
            result: value.result.into_iter().map(Into::into).collect(),
        }
    }
}

const MAIN_SYMBOL: &str = "_start";

const STACK_PTR: &str = "stack_ptr";
const STACK_ADDR: i64 = 1048576;

const FRAME_PTR: &str = "frame_ptr";
const FRAME_ADDR: i64 = 0;

const JS_OBJECT_NAME: &str = "env";

pub fn translate(tcx: &TyCtx, hir: &HirProgram) -> Module {
    let mut wasm = Wasm::new(tcx);
    let outermost_level = Level::outermost_with_name(MAIN_SYMBOL);
    let expr = wasm.trans_expr(hir, outermost_level.clone());
    wasm.proc_entry_exit(outermost_level, StackType::nop(), expr);

    let funcs = wasm.funcs;
    let globals = vec![
        Global {
            name: Some(Name::new(STACK_PTR.to_string())),
            ty: GlobalType {
                ty: ValType::Num(NumType::I32),
                m: Mut::Var,
            },
            init: Expr::Op(Operator::Const(NumType::I32, STACK_ADDR)),
        },
        Global {
            name: Some(Name::new(FRAME_PTR.to_string())),
            ty: GlobalType {
                ty: ValType::Num(NumType::I32),
                m: Mut::Var,
            },
            init: Expr::Op(Operator::Const(NumType::I32, FRAME_ADDR)),
        },
    ];

    let memory = Memory {
        name: None,
        ty: Limits { min: 16, max: None },
    };

    let builtin = [INIT_ARRAY, LOAD_I32, LOAD_I64, STORE_I32, STORE_I64];

    let imports = builtin
        .map(|name| Import {
            module: JS_OBJECT_NAME.to_string(),
            name: name.to_string(),
            kind: ImportKind::Func(
                Some(Index::Name(name.into())),
                TypeUse::Inline(BUILTIN_FUNCS[name].clone()),
            ),
        })
        .to_vec();

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

struct Wasm<'tcx> {
    var_env: HashMap<VarId, Access>,
    fn_env: HashMap<FnId, Level>,
    tcx: &'tcx TyCtx,

    funcs: Vec<Func>,
}

impl<'tcx> Wasm<'tcx> {
    pub fn new(tcx: &'tcx TyCtx) -> Self {
        Self {
            var_env: HashMap::new(),
            fn_env: HashMap::new(),
            tcx,
            funcs: Vec::new(),
        }
    }

    fn trans_expr(&mut self, expr: &HirExpr, level: Level) -> ExprType {
        match &expr.kind {
            ExprKind::LValue(lvalue, _) => self.load_lvalue(lvalue, level),
            ExprKind::Nil(_) => todo!(),
            ExprKind::Sequence(exprs, _) => {
                let exprs = exprs
                    .iter()
                    .map(|expr| self.trans_expr(expr, level.clone()))
                    .collect();
                sequence(exprs).add_comment("seq")
            }
            ExprKind::Int(v, _) => num_i64(*v as i64).add_comment("int"),
            ExprKind::Str(_, _) => todo!(),
            ExprKind::FuncCall(_, fn_id, args, _) => {
                let entry = self.tcx.fn_(*fn_id);
                let ret_ty = self.tcx.type_(entry.result());
                let ret_ty = StackType::new(vec![], vec![convert_ty(ret_ty)]);

                let args_expr = args
                    .iter()
                    .map(|a| {
                        let e = self.trans_expr(a, level.clone());
                        e.val
                    })
                    .collect();
                let fn_level = self.fn_env.get(fn_id).expect("level not found");

                fn_call(entry.label(), fn_level.clone(), level, ret_ty, args_expr)
                    .add_comment("fn call")
            }
            ExprKind::Op(op, lhs, rhs, _) => {
                let lhs = self.trans_expr(lhs, level.clone());
                let rhs = self.trans_expr(rhs, level);
                bin_op((*op).try_into().unwrap(), lhs, rhs).add_comment("bin op")
            }

            ExprKind::Neg(_, _) => todo!(),
            ExprKind::RecordCreation(_, _, _) => todo!(),
            ExprKind::ArrayCreation { size, init, .. } => {
                let size = self.trans_expr(size, level.clone());
                let size = i64_2_i32(size);
                let init = self.trans_expr(init, level.clone());
                let mut frame = level.frame_mut();
                array_creation(&mut frame, size, init).add_comment("array creation")
            }
            ExprKind::Assign(lvalue, expr, _) => {
                let expr = self.trans_expr(expr, level.clone());
                self.store_lvalue(lvalue, level, expr).add_comment("assign")
            }
            ExprKind::If {
                cond, then, els, ..
            } => {
                let mut cond = self.trans_expr(cond, level.clone());
                let then = self.trans_expr(then, level.clone());
                let els = els.as_deref().map(|els| self.trans_expr(els, level));

                if cond.ty.is_const_1_i64() {
                    cond = i64_2_i32(cond);
                }

                if_expr(cond, then, els).add_comment("if expr")
            }
            ExprKind::While(cond, instr, _) => {
                let cond = self.trans_expr(cond, level.clone());
                let body = self.trans_expr(instr, level);
                while_expr(cond, body).add_comment("while expr")
            }
            ExprKind::Break(_) => todo!(),
            ExprKind::Let(decls, exprs, _) => {
                let mut decls = decls
                    .iter()
                    .flat_map(|v| self.trans_decl(v, level.clone()))
                    .collect::<Vec<_>>();

                let exprs = exprs
                    .iter()
                    .map(|v| self.trans_expr(v, level.clone()))
                    .collect::<Vec<_>>();
                let exprs = sequence(exprs);

                decls.push(exprs);

                concat_exprs(decls).add_comment("expr seq")
            }
        }
    }

    fn store_lvalue(&mut self, lvalue: &HirLValue, level: Level, expr: ExprType) -> ExprType {
        match lvalue {
            HirLValue::Var(_, var_id, _, _) => {
                let (access, ancestor_level) = self.var_env.get(var_id).expect("access not found");
                let env = calc_static_link(level.clone(), ancestor_level.clone());
                let frame = level.frame();
                frame.store2access(access, env, expr)
            }
            HirLValue::RecordField { .. } => todo!(),
            HirLValue::Array { .. } => todo!(),
        }
    }

    fn load_lvalue(&mut self, lvalue: &HirLValue, level: Level) -> ExprType {
        match lvalue {
            HirLValue::Var(_, var_id, type_id, _) => {
                let (access, ancestor_level) = self.var_env.get(var_id).expect("access not found");
                let env = calc_static_link(level.clone(), ancestor_level.clone());
                let frame = level.frame();
                let ty = self.tcx.type_(*type_id);
                let ValType::Num(ty) = convert_ty(ty);
                frame.get_access_content(access, env, ty)
            }
            HirLValue::RecordField { .. } => todo!(),
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
                let val_type @ ValType::Num(num_ty) = convert_ty(ty);

                let var = self.load_lvalue(array, level.clone());
                let subscript = self.trans_expr(index, level);
                let subscript = i64_2_i32(subscript);

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

    fn trans_decl(&mut self, decl: &Decl, parent_level: Level) -> Option<ExprType> {
        match decl {
            Decl::Type(_) => None,
            Decl::Var(VarDecl(_, var_id, is_escape, _, ty_id, expr, _)) => {
                let expr = self.trans_expr(expr, parent_level.clone());

                let mut frame = parent_level.frame_mut();
                let ty = self.tcx.type_(*ty_id);
                let access = frame.alloc_local(*is_escape, convert_ty(ty));

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
                            (is_escape, convert_ty(ty))
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

                    let body = self.trans_expr(&decl.body, level.clone());

                    let mut args_ty: Vec<_> = decl
                        .params
                        .iter()
                        .map(|p| convert_ty(self.tcx.type_(p.type_id)))
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
                "_tiger_exit".to_string()
            } else {
                format!("{}", label)
            }
        }
    }
}

fn _load_i64(addr: ExprType) -> ExprType {
    addr.assert_ty(StackType::const_1_i32());
    let expr = Expr::OpExpr(Operator::Load(NumType::I64), vec![addr.val]);

    let ty = StackType::new(
        vec![ValType::Num(NumType::I32)],
        vec![ValType::Num(NumType::I64)],
    );
    ExprType::new(expr, ty)
}

fn calc_static_link(mut cur_level: Level, ancestor_level: Level) -> ExprType {
    let mut link = Frame::fp();
    if cur_level == ancestor_level {
        return link;
    }

    while cur_level != ancestor_level {
        let frame = cur_level.frame();
        let link_access = frame.env();
        link = frame.get_access_content(link_access, link, NumType::I32);
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

fn num_i64(v: i64) -> ExprType {
    ExprType::new_const_1_i64(Expr::Op(Operator::Const(NumType::I64, v)))
}

fn bin_op(op: BinOp, lhs: ExprType, rhs: ExprType) -> ExprType {
    lhs.assert_eq_ty(&rhs);
    assert!(lhs.ty.is_const_1());
    assert!(rhs.ty.is_const_1());

    let ValType::Num(operand_ty) = lhs.ty.result[0];

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

fn nop() -> ExprType {
    ExprType::new(Expr::Op(Operator::Nop), StackType::nop())
}

fn _store_i64(addr: ExprType, val: ExprType) -> ExprType {
    addr.assert_ty(StackType::const_1_i32());
    val.assert_ty(StackType::const_1_i64());
    ExprType::new(
        Expr::OpExpr(Operator::Store(NumType::I64), vec![addr.val, val.val]),
        StackType::new(
            vec![ValType::Num(NumType::I32), ValType::Num(NumType::I64)],
            vec![],
        ),
    )
}

fn concat_exprs(exprs: Vec<ExprType>) -> ExprType {
    let len = exprs.len();
    let mut iter = exprs.into_iter();
    let Some(first) = iter.next() else {
        return nop();
    };

    let mut init = Vec::with_capacity(len);
    init.push(first.val);
    let seq_result = iter.try_fold((init, first.ty), |(mut exprs, ty), x| {
        let ty = ty.composition(&x.ty)?;
        exprs.push(x.val);
        Ok::<(Vec<Expr>, StackType), ()>((exprs, ty))
    });

    let Ok((exprs, ty)) = seq_result else {
        panic!("type error");
    };

    let instructions = exprs.into_iter().map(Instruction::Expr).collect();
    let params = ty
        .arg
        .clone()
        .into_iter()
        .map(|v| Param::new(v, None))
        .collect();
    let results = ty.result.clone().into_iter().map(Into::into).collect();

    let expr = Expr::Block(
        None,
        BlockType(TypeUse::Inline(FuncType::new(params, results))),
        instructions,
    );

    ExprType::new(expr, ty)
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

fn convert_ty(ty: &Type) -> ValType {
    match ty {
        Type::Int => ValType::Num(NumType::I64),
        Type::String | Type::Record { .. } | Type::Array { .. } | Type::Nil => {
            ValType::Num(NumType::I32)
        }
        // TODO: unit の扱い
        Type::Unit => todo!(),
    }
}

fn size(ty: &ValType) -> usize {
    match ty {
        ValType::Num(NumType::I32) => 4,
        ValType::Num(NumType::I64) => 8,
        _ => unimplemented!(),
    }
}

fn push(expr: ExprType) -> ExprType {
    assert!(expr.ty.is_const_1());
    let ValType::Num(expr_ty) = expr.ty.result[0];

    let sub_stack_ptr = bin_stack_ptr(
        ExprType::new_const_1_i32(Expr::Op(Operator::Const(
            NumType::I32,
            size(&expr.ty.result[0]) as i64,
        ))),
        BinOp::Sub,
    );

    let store_expr = Expr::OpExpr(
        Operator::Store(expr_ty),
        vec![
            Expr::Op(Operator::GlobalGet(Index::Name(STACK_PTR.into()))),
            expr.val,
        ],
    );
    let store_expr = ExprType::new(store_expr, StackType::nop());

    concat_exprs(vec![sub_stack_ptr, store_expr])
}

fn pop(ty: NumType) -> ExprType {
    let load_expr = Expr::OpExpr(
        Operator::Load(ty),
        vec![Expr::Op(Operator::GlobalGet(Index::Name(STACK_PTR.into())))],
    );
    let load_expr = ExprType::new(load_expr, StackType::const_(vec![ValType::Num(ty)]));

    let add_stack_ptr = bin_stack_ptr(
        ExprType::new_const_1_i32(Expr::Op(Operator::Const(
            NumType::I32,
            size(&ValType::Num(ty)) as i64,
        ))),
        BinOp::Add,
    );

    concat_exprs(vec![load_expr, add_stack_ptr])
}

// $sp <- $sp `op` expr
fn bin_stack_ptr(expr: ExprType, op: BinOp) -> ExprType {
    assert!(expr.ty.is_const_1());

    let sub_stack_ptr = Expr::OpExpr(
        Operator::GlobalSet(Index::Name(STACK_PTR.into())),
        vec![Expr::OpExpr(
            Operator::Bin(NumType::I32, op),
            vec![
                Expr::Op(Operator::GlobalGet(Index::Name(STACK_PTR.into()))),
                expr.val,
            ],
        )],
    );
    ExprType::new(sub_stack_ptr, StackType::nop())
}

fn fn_call(
    label: &Label,
    fn_level: Level,
    cur_level: Level,
    ret_ty: StackType,
    mut args: Vec<Expr>,
) -> ExprType {
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
    let size_ty = size.ty.result[0];
    let ValType::Num(size_num_ty) = size_ty;
    let (size_access, store2size) = frame.init_local(false, size);
    exprs.push(store2size);

    let (word_size_access, store2word_size) = frame.init_local(
        false,
        frame.get_access_content(&size_access, Frame::fp(), size_num_ty),
    );
    exprs.push(store2word_size);

    // initialize `init`
    let init_ty @ ValType::Num(init_num_ty) = init.ty.result[0];
    let init_access = frame.alloc_local(false, init_ty);
    let store2init = frame.store2access(&init_access, Frame::fp(), init);
    exprs.push(store2init);

    // initialize `arr: *TigerArray`,
    let arr = Frame::extern_call(
        INIT_ARRAY,
        vec![bin_op(
            BinOp::Mul,
            frame.get_access_content(&word_size_access, Frame::fp(), size_num_ty),
            ExprType::new_const_1_i32(Expr::Op(Operator::Const(
                size_num_ty,
                init_num_ty.word_size() as i64,
            ))),
        )],
        vec![ValType::Num(NumType::I32)],
    );
    let (arr_access, store2arr) = frame.init_local(false, arr);
    exprs.push(store2arr);

    // initialize `idx`: i32
    let idx_num_ty = NumType::I32;
    let idx_ty = ValType::Num(idx_num_ty);
    let (idx_access, store2idx) = frame.init_local(
        false,
        ExprType::new_const_1_i32(Expr::Op(Operator::Const(idx_num_ty, 0))),
    );
    exprs.push(store2idx);
    assert_eq!(idx_num_ty, size_num_ty);

    // idx < size
    let cond = bin_op(
        BinOp::LessThanSigned,
        frame.get_access_content(&idx_access, Frame::fp(), idx_num_ty),
        frame.get_access_content(&size_access, Frame::fp(), size_num_ty),
    );

    //     arr[idx] := init;
    let load_arr = frame.get_access_content(&arr_access, Frame::fp(), NumType::I32);
    let load_idx = frame.get_access_content(&idx_access, Frame::fp(), idx_num_ty);
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
            frame.get_access_content(&init_access, Frame::fp(), init_ty),
        ],
        vec![],
    );

    // idx := idx + 1;
    let body2 = frame.store2access(
        &idx_access,
        Frame::fp(),
        bin_op(
            BinOp::Add,
            frame.get_access_content(&idx_access, Frame::fp(), idx_num_ty),
            ExprType::new_const_1(Expr::Op(Operator::Const(idx_num_ty, 1)), idx_ty),
        ),
    );

    let body = ExprType::new(concat_exprs(vec![body1, body2]).val, StackType::nop());

    let w = while_expr(cond, body);
    exprs.push(w);

    let load_arr = frame.get_access_content(&arr_access, Frame::fp(), NumType::I32);
    exprs.push(load_arr);

    concat_exprs(exprs)
}

/// `arr_content_ty` is type of `arr[idx]`.
/// `var` is address of array.
/// `subscript` is index of array.
/// push address of `arr[idx]` on stack top
///
/// pub struct TigerArray {
///     len: TigerInt,
///     data: *mut TigerInt,
/// }
fn array_subscript(arr_content_ty: NumType, var: ExprType, subscript: ExprType) -> ExprType {
    var.assert_ty(StackType::const_1_i32());
    subscript.assert_ty(StackType::const_1_i32());

    // addr of `TigerArray.data`
    let addr_of_data = bin_op(
        BinOp::Add,
        var,
        ExprType::new_const_1_i32(Expr::Op(Operator::Const(NumType::I32, 4))),
    );

    // `data`: *mut TIgerInt
    let data = Frame::extern_call(
        LOAD_I32,
        vec![addr_of_data],
        vec![ValType::Num(NumType::I32)],
    );

    // data + subscript * 8

    let e = Expr::OpExpr(
        Operator::Bin(NumType::I32, BinOp::Add),
        vec![
            data.val,
            Expr::OpExpr(
                Operator::Bin(NumType::I32, BinOp::Mul),
                vec![
                    subscript.val,
                    Expr::Op(Operator::Const(
                        NumType::I32,
                        8 * arr_content_ty.word_size() as i64,
                    )),
                ],
            ),
        ],
    );

    ExprType::new(e, StackType::const_1_i32())
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
