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
    NumType, Operator, Param, ValType, WasmResult,
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

    pub fn new_const_1_i32(expr: Expr) -> Self {
        Self::new_const_(expr, vec![ValType::Num(NumType::I32)])
    }

    pub fn new_const_1_i64(expr: Expr) -> Self {
        Self::new_const_(expr, vec![ValType::Num(NumType::I64)])
    }

    pub fn assert<F>(&self, f: F)
    where
        F: FnOnce(&Self) -> bool,
    {
        if !f(self) {
            panic!("assertion failed {:?}", self)
        }
    }

    pub fn is_const_1(ret_ty: ValType) -> impl FnOnce(&Self) -> bool {
        move |expr_type: &Self| {
            let arg = &expr_type.ty.arg;
            let result = &expr_type.ty.result;
            arg.is_empty() && result.len() == 1 && result[0] == ret_ty
        }
    }

    pub fn is_const_1_i32() -> impl FnOnce(&Self) -> bool {
        Self::is_const_1(ValType::Num(NumType::I32))
    }

    pub fn is_const_1_i64() -> impl FnOnce(&Self) -> bool {
        Self::is_const_1(ValType::Num(NumType::I64))
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
        Self {
            arg: Vec::new(),
            result,
        }
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
}

const MAIN_SYMBOL: &str = "_start";

pub fn translate(tcx: &TyCtx, hir: &HirProgram) -> Module {
    let mut wasm = Wasm::new();
    let expr = wasm.trans_expr(hir, Rc::new(Level::outermost_with_name(MAIN_SYMBOL)));

    let instr = Instruction::Expr(expr.val);
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

    // i32
    fn fp(&self) -> Local {
        Local(0)
    }

    fn exp(access: &Stack, base_addr: ExprType) -> ExprType {
        base_addr.assert(ExprType::is_const_1_i32());

        let expr = Expr::OpExpr(
            Operator::Bin(NumType::I32, BinOp::Sub),
            vec![
                base_addr.val,
                Expr::Op(Operator::Const(NumType::I32, access.0 as i64)),
            ],
        );
        ExprType::new_const_1_i32(expr)
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

    fn trans_expr(&mut self, expr: &HirExpr, level: Rc<Level>) -> ExprType {
        match &expr.kind {
            ExprKind::LValue(lvalue, _) => {
                let lvalue = self.trans_lvalue(lvalue, level);
                lvalue.assert(ExprType::is_const_1(ValType::Num(NumType::I32)));
                load_i64(lvalue)
            }
            ExprKind::Nil(_) => todo!(),
            ExprKind::Sequence(_, _) => todo!(),
            ExprKind::Int(v, _) => num_i64(*v as i64),
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
                bin_op_i64((*op).try_into().unwrap(), lhs, rhs)
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
                store_i64(lvalue, expr)
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
                let mut decls = decls
                    .iter()
                    .flat_map(|v| self.trans_decl(v, level.clone()))
                    .collect::<Vec<_>>();

                let mut exprs = exprs
                    .iter()
                    .map(|v| self.trans_expr(v, level.clone()))
                    .collect::<Vec<_>>();

                decls.append(&mut exprs);

                expr_seq(decls)
            }
        }
    }

    // Put the address of lvalue on the stack top.
    fn trans_lvalue(&mut self, lvalue: &HirLValue, level: Rc<Level>) -> ExprType {
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

    fn trans_decl(&mut self, decl: &Decl, level: Rc<Level>) -> Option<ExprType> {
        match decl {
            Decl::Type(_) => todo!(),
            Decl::Var(VarDecl(_, var_id, _, _, _, expr, _)) => {
                let expr = self.trans_expr(expr, level.clone());
                let stack = level.frame.borrow_mut().alloc_stack(8);
                let base_addr = local_to_i32expr(&level.frame.borrow().fp());
                let addr = Frame::exp(&stack, base_addr);

                self.var_env.insert(*var_id, (stack, level));

                Some(store_i64(addr, expr))
            }
            Decl::Func(_) => todo!(),
        }
    }
}

fn load_i64(addr: ExprType) -> ExprType {
    addr.assert(ExprType::is_const_1_i32());
    let expr = Expr::OpExpr(Operator::Load(NumType::I64), vec![addr.val]);

    let ty = StackType::new(
        vec![ValType::Num(NumType::I32)],
        vec![ValType::Num(NumType::I64)],
    );
    ExprType::new(expr, ty)
}

fn sp() -> ExprType {
    ExprType::new_const_1_i32(Expr::Op(Operator::GlobalGet(Index::Name(
        STACK_PTR.to_string(),
    ))))
}

fn calc_static_link<'a>(mut cur_level: &'a Level, ancestor_level: &'a Level) -> ExprType {
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

fn num_i64(v: i64) -> ExprType {
    ExprType::new_const_1_i64(Expr::Op(Operator::Const(NumType::I64, v)))
}

fn bin_op_i64(op: BinOp, lhs: ExprType, rhs: ExprType) -> ExprType {
    lhs.assert(ExprType::is_const_1_i64());
    rhs.assert(ExprType::is_const_1_i64());

    ExprType::new_const_1_i64(Expr::OpExpr(
        Operator::Bin(NumType::I64, op),
        vec![lhs.val, rhs.val],
    ))
}

fn nop() -> ExprType {
    ExprType::new(Expr::Op(Operator::Nop), StackType::nop())
}

fn store_i64(addr: ExprType, val: ExprType) -> ExprType {
    addr.assert(ExprType::is_const_1_i32());
    val.assert(ExprType::is_const_1_i64());
    ExprType::new(
        Expr::OpExpr(Operator::Store(NumType::I64), vec![addr.val, val.val]),
        StackType::new(
            vec![ValType::Num(NumType::I32), ValType::Num(NumType::I64)],
            vec![],
        ),
    )
}

// Type of Local must be i64
fn local_to_i64expr(local: &Local) -> ExprType {
    ExprType::new_const_1_i64(Expr::Op(Operator::LocalGet(Index::Index(local.0 as u32))))
}

// Type of Local must be i32
fn local_to_i32expr(local: &Local) -> ExprType {
    ExprType::new_const_1_i32(Expr::Op(Operator::LocalGet(Index::Index(local.0 as u32))))
}

fn expr_seq(exprs: Vec<ExprType>) -> ExprType {
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
