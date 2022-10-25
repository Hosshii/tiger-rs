mod ast;
mod encode;
pub mod rewrite;
pub use encode::Encoder;

use std::{collections::HashMap, rc::Rc};

use crate::{
    semant::{
        ctx::{TyCtx, VarId},
        hir::{Expr as HirExpr, ExprKind, LValue as HirLValue, Program as HirProgram},
    },
    wasm::ast::TypeUse,
};

use self::ast::{
    BinOp, Expr, Func, FuncType, FuncTypeDef, Index, Instruction, Module, ModuleBuilder, Name,
    NumType, Operator, ValType, WasmResult,
};

const MAIN_SYMBOL: &str = "__start";

pub fn translate(tcx: &TyCtx, hir: &HirProgram) -> Module {
    let mut wasm = Wasm::new();
    let expr = wasm.trans_expr(hir);

    let instr = Instruction::Expr(expr);
    let func = Func::new(
        Some(Name::new(MAIN_SYMBOL.to_string())),
        TypeUse::Inline(FuncType::new(
            vec![],
            vec![WasmResult::new(ValType::Num(NumType::I64))],
        )),
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

trait LValue<T> {
    fn addr(&self);
    fn load(&self);
    fn store(&self, val: T);
}

#[derive(Debug, PartialEq, Eq)]
struct Frame {
    pointer: usize,
    local_count: usize,
    env: Stack,
}

impl Frame {
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
    frame: Frame,
    parent: Option<Box<Level>>,
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

    fn trans_expr(&mut self, expr: &HirExpr) -> Expr {
        match &expr.kind {
            ExprKind::LValue(_, _) => todo!(),
            ExprKind::Nil(_) => todo!(),
            ExprKind::Sequence(_, _) => todo!(),
            ExprKind::Int(v, _) => num(*v as i64),
            ExprKind::Str(_, _) => todo!(),
            ExprKind::FuncCall(_, _, _, _) => todo!(),
            ExprKind::Op(_, _, _, _) => todo!(),
            ExprKind::Neg(_, _) => todo!(),
            ExprKind::RecordCreation(_, _, _) => todo!(),
            ExprKind::ArrayCreation {
                type_ident,
                size,
                init,
                pos,
            } => todo!(),
            ExprKind::Assign(_, _, _) => todo!(),
            ExprKind::If {
                cond,
                then,
                els,
                pos,
            } => todo!(),
            ExprKind::While(_, _, _) => todo!(),
            ExprKind::Break(_) => todo!(),
            ExprKind::Let(_, _, _) => todo!(),
        }
    }

    // Put the address of lvalue on the stack top.
    fn trans_lvalue(&mut self, lvalue: &HirLValue, level: &Level) -> Expr {
        match lvalue {
            HirLValue::Var(_, var_id, _, _) => {
                let (access, ancestor_level) = self.var_env.get(var_id).expect("access not found");
                let env = calc_static_link(level, ancestor_level);
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
        let link_access = cur_level.frame.env();
        link = Frame::exp(link_access, link);
        let parent = ancestor_level.parent.as_deref().expect("cur_level is root");
        cur_level = parent;
    }

    link
}

fn num(v: i64) -> Expr {
    Expr::Op(Operator::Const(NumType::I64, v))
}
