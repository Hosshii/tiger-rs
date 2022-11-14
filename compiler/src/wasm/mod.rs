mod ast;
mod encode;
pub mod rewrite;
pub use encode::Encoder;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    common::Label,
    semant::{
        ctx::{TyCtx, VarId},
        hir::{
            Decl, Expr as HirExpr, ExprKind, LValue as HirLValue, Operator as HirOperator,
            Program as HirProgram, VarDecl,
        },
    },
    wasm::ast::{InlineFuncExport, TypeUse},
};

use self::ast::{
    BinOp, BlockType, Expr, Func, FuncType, Index, Instruction, Module, ModuleBuilder, Name,
    NumType, Operator, ValType, WasmResult,
};

const MAIN_SYMBOL: &str = "_start";

pub fn translate(tcx: &TyCtx, hir: &HirProgram) -> Module {
    let mut wasm = Wasm::new();
    let expr = wasm.trans_expr(hir, Rc::new(Level::outermost_with_name(MAIN_SYMBOL)));

    let instr = Instruction::Expr(expr);
    let func = Func::new(
        Some(Name::new(MAIN_SYMBOL.to_string())),
        TypeUse::Inline(FuncType::new(
            vec![],
            vec![WasmResult::new(ValType::Num(NumType::I64))],
        )),
        Some(InlineFuncExport::new(Name::new(MAIN_SYMBOL.to_string()))),
        vec![],
        vec![instr],
    );

    let builder = ModuleBuilder::new();

    builder.add_func(func).build()
}

const STACK_PTR: &str = "stack_ptr";

#[derive(Debug, PartialEq, Eq)]
struct Stack(usize);

#[derive(Debug, PartialEq, Eq)]
struct Local(usize);

/// Stack(x) means
/// fp - x
#[derive(Debug, PartialEq, Eq, Clone)]
struct Memory {
    address: usize,
}

#[derive(Debug, PartialEq, Eq)]
struct Frame {
    name: Label,
    pointer: usize,
    local_count: usize,
    env: Stack,
}

impl Frame {
    pub fn new(name: Label) -> Self {
        Self {
            name,
            pointer: 0,
            local_count: 0,
            env: Stack(0),
        }
    }

    fn alloc_stack(&mut self, size: usize) -> Stack {
        let ptr = self.pointer;
        self.pointer += size;
        Stack(ptr)
    }

    fn alloc_local(&mut self, count: usize) -> Local {
        let num = self.local_count;
        self.local_count += count;
        Local(num)
    }

    fn env(&self) -> &Stack {
        &self.env
    }

    fn fp(&self) -> Local {
        Local(0)
    }

    fn exp(access: &Stack, base_addr: Expr) -> Expr {
        Expr::OpExpr(
            Operator::Bin(NumType::I64, BinOp::Sub),
            vec![
                base_addr,
                Expr::Op(Operator::Const(NumType::I64, access.0 as i64)),
            ],
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Level {
    frame: RefCell<Frame>,
    parent: Option<Box<Level>>,
}

impl Level {
    fn outermost_with_name(name: impl Into<String>) -> Self {
        Self {
            frame: RefCell::new(Frame::new(Label::NamedFn(name.into()))),
            parent: None,
        }
    }
}

struct Wasm {
    var_env: HashMap<VarId, (Stack, Rc<Level>)>,
}

impl Wasm {
    pub fn new() -> Self {
        Self {
            var_env: HashMap::new(),
        }
    }

    fn trans_expr(&mut self, expr: &HirExpr, level: Rc<Level>) -> Expr {
        match &expr.kind {
            ExprKind::LValue(lvalue, _) => {
                let lvalue = self.trans_lvalue(lvalue, level);
                load(lvalue)
            }
            ExprKind::Nil(_) => todo!(),
            ExprKind::Sequence(_, _) => todo!(),
            ExprKind::Int(v, _) => num(*v as i64),
            ExprKind::Str(_, _) => todo!(),
            ExprKind::FuncCall(_, _, _, _) => todo!(),
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
                let lhs = self.trans_expr(lhs, level.clone());
                let rhs = self.trans_expr(rhs, level);
                bin_op((*op).try_into().unwrap(), lhs, rhs)
            }
            ExprKind::Op(_, _, _, _) => todo!(),
            ExprKind::Neg(_, _) => todo!(),
            ExprKind::RecordCreation(_, _, _) => todo!(),
            ExprKind::ArrayCreation {
                type_ident,
                size,
                init,
                pos,
            } => todo!(),
            ExprKind::Assign(lvalue, expr, _) => {
                let lvalue = self.trans_lvalue(lvalue, level.clone());
                let expr = self.trans_expr(expr, level);
                Expr::OpExpr(Operator::Store(NumType::I64), vec![lvalue, expr])
            }
            ExprKind::If {
                cond,
                then,
                els,
                pos,
            } => todo!(),
            ExprKind::While(_, _, _) => todo!(),
            ExprKind::Break(_) => todo!(),
            ExprKind::Let(decls, exprs, _) => {
                let decls = decls
                    .iter()
                    .flat_map(|v| self.trans_decl(v, level.clone()))
                    .collect::<Vec<_>>();

                let mut exprs = exprs
                    .iter()
                    .map(|v| self.trans_expr(v, level.clone()))
                    .collect::<Vec<_>>();

                let last = exprs.pop().unwrap_or_else(nop);
                todo!()
            }
        }
    }

    // Put the address of lvalue on the stack top.
    fn trans_lvalue(&mut self, lvalue: &HirLValue, level: Rc<Level>) -> Expr {
        match lvalue {
            HirLValue::Var(_, var_id, _, _) => {
                let (access, ancestor_level) = self.var_env.get(var_id).expect("access not found");
                let env = calc_static_link(level.as_ref(), ancestor_level);
                Frame::exp(access, env)
            }
            HirLValue::RecordField {
                record,
                record_type,
                field_ident,
                field_index,
                field_type,
                pos,
            } => todo!(),
            HirLValue::Array {
                array,
                array_type,
                index,
                index_type,
                pos,
            } => todo!(),
        }
    }

    fn trans_decl(&mut self, decl: &Decl, level: Rc<Level>) -> Option<Expr> {
        match decl {
            Decl::Type(_) => todo!(),
            Decl::Var(VarDecl(_, var_id, _, _, _, expr, _)) => {
                let expr = self.trans_expr(expr, level.clone());
                let stack = level.frame.borrow_mut().alloc_stack(8);
                let base_addr = local_to_expr(&level.frame.borrow().fp());
                let addr = Frame::exp(&stack, base_addr);

                self.var_env.insert(*var_id, (stack, level.clone()));

                Some(store(addr, expr))
            }
            Decl::Func(_) => todo!(),
        }
    }
}

fn load(addr: Expr) -> Expr {
    Expr::OpExpr(Operator::Load(NumType::I64), vec![addr])
}

fn sp() -> Expr {
    Expr::Op(Operator::GlobalGet(Index::Name(STACK_PTR.to_string())))
}

fn calc_static_link<'a>(mut cur_level: &'a Level, ancestor_level: &'a Level) -> Expr {
    let mut link = sp();
    if cur_level == ancestor_level {
        return link;
    }

    while cur_level != ancestor_level {
        let frame = cur_level.frame.borrow();
        let link_access = frame.env();
        link = Frame::exp(link_access, link);
        let parent = ancestor_level.parent.as_deref().expect("cur_level is root");
        cur_level = parent;
    }

    link
}

fn num(v: i64) -> Expr {
    Expr::Op(Operator::Const(NumType::I64, v))
}

fn bin_op(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::OpExpr(Operator::Bin(NumType::I64, op), vec![lhs, rhs])
}

fn nop() -> Expr {
    Expr::Op(Operator::Const(NumType::I64, 0))
}

fn store(addr: Expr, val: Expr) -> Expr {
    Expr::OpExpr(Operator::Store(NumType::I32), vec![addr, val])
}

fn local_to_expr(local: &Local) -> Expr {
    Expr::Op(Operator::LocalGet(Index::Index(local.0 as u32)))
}

fn expr_seq(mut instr: Vec<Expr>) -> Expr {
    // Expr::Block(None, BlockType::Num(NumType::I64), instr)
    todo!()
}
