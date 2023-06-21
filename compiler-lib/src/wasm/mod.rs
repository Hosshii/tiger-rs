//! sp -> stack_ptr
//! fp -> frame_ptr
//! address is 32bit
//! args are 64bit except first arg which means static link.

mod ast;
mod builtin;
pub mod encode;
mod frame;
mod rewrite;
mod translate;
mod watencoder;

pub use encode::Encoder as WasmEncoder;
pub use translate::translate;
pub use watencoder::Encoder as WatEncoder;

use crate::wasm::ast::TypeUse;

use self::{
    ast::{
        BinOp, BlockType, Expr, FuncType, Index, Instruction, NumType, Operator, Param, ValType,
    },
    frame::Size,
};

const MAIN_SYMBOL: &str = "_start";

const STACK_PTR: &str = "stack_ptr";
const STACK_ADDR: i64 = 1048576;

const FRAME_PTR: &str = "frame_ptr";
const FRAME_ADDR: i64 = 0;

const JS_OBJECT_NAME: &str = "env";

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

    pub fn arg(&self) -> &[ValType] {
        &self.arg
    }

    pub fn result(&self) -> &[ValType] {
        &self.result
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

    pub fn is_const_1_i32(&self) -> bool {
        self.is_const_1() && self.result[0] == ValType::Num(NumType::I32)
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

fn nop() -> ExprType {
    ExprType::new(Expr::Op(Operator::Nop), StackType::nop())
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
        .arg()
        .iter()
        .copied()
        .map(|v| Param::new(v, None))
        .collect();
    let results = ty.result().iter().copied().map(Into::into).collect();

    let expr = Expr::Block(
        None,
        BlockType(TypeUse::Inline(FuncType::new(params, results))),
        instructions,
    );

    ExprType::new(expr, ty)
}

fn push(expr: ExprType) -> ExprType {
    assert!(expr.ty.is_const_1());
    let ValType::Num(expr_ty) = expr.ty.result()[0];

    let sub_stack_ptr = bin_stack_ptr(
        ExprType::new_const_1_i32(Expr::Op(Operator::Const(
            NumType::I32,
            expr.ty.result()[0].byte_size() as i64,
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
            ty.byte_size() as i64,
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
