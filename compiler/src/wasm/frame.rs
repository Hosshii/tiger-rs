use crate::common::Label;

use super::{
    ast::{BinOp, Expr, Index, Name, NumType, Operator},
    ExprType, StackType, FRAME_PTR, STACK_PTR,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Local(usize);

impl Local {
    pub fn new(idx: usize) -> Self {
        Self(idx)
    }
}

impl From<Local> for Index {
    fn from(value: Local) -> Self {
        Index::Index(value.0 as u32)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Frame {
    name: Label,
    pointer: usize,
    local_count: usize, // 0 indexed
    param_count: usize,
    env: Access,
}

impl Frame {
    const WORD_SIZE: usize = 4;

    pub fn new(name: Label, param_count: usize) -> Self {
        Self {
            name,
            pointer: 0,
            local_count: 0,
            param_count,
            env: Access::InLocal(Local::new(0)),
        }
    }

    pub fn alloc_local(&mut self, is_escape: bool) -> Access {
        if is_escape {
            self.pointer += Self::WORD_SIZE;
            Access::InFrame(self.pointer)
        } else {
            let count = self.local_count;
            self.local_count += 1;
            Access::InLocal(Local::new(count + self.param_count))
        }
    }

    pub fn env(&self) -> &Access {
        &self.env
    }

    // i32
    pub(super) fn fp() -> ExprType {
        ExprType::new_const_1_i32(Expr::Op(Operator::GlobalGet(Index::Name(Name(
            FRAME_PTR.to_string(),
        )))))
    }

    pub(super) fn sp() -> ExprType {
        ExprType::new_const_1_i32(Expr::Op(Operator::GlobalGet(Index::Name(Name(
            STACK_PTR.to_string(),
        )))))
    }

    pub(super) fn get_access_content(access: &Access, base_addr: ExprType) -> ExprType {
        base_addr.assert(ExprType::is_const_1_i32());

        let expr = match access {
            Access::InFrame(val) => Expr::OpExpr(
                Operator::Bin(NumType::I32, BinOp::Sub),
                vec![
                    base_addr.val,
                    Expr::Op(Operator::Const(NumType::I32, *val as i64)),
                ],
            ),
            Access::InLocal(local) => Expr::Op(Operator::LocalGet((*local).into())),
        };

        ExprType::new_const_1_i64(expr)
    }

    pub(super) fn store2access(access: &Access, base_addr: ExprType, expr: ExprType) -> ExprType {
        base_addr.assert(ExprType::is_const_1_i32());
        expr.assert(ExprType::is_const_1_i64());

        let expr = match access {
            Access::InFrame(val) => Expr::OpExpr(
                Operator::Store(NumType::I64),
                vec![
                    Expr::OpExpr(
                        Operator::Bin(NumType::I32, BinOp::Sub),
                        vec![
                            base_addr.val,
                            Expr::Op(Operator::Const(NumType::I32, *val as i64)),
                        ],
                    ),
                    expr.val,
                ],
            ),
            Access::InLocal(local) => {
                Expr::OpExpr(Operator::LocalSet((*local).into()), vec![expr.val])
            }
        };

        ExprType::new(expr, StackType::nop())
    }

    /// count of local variables
    pub fn local_count(&self) -> usize {
        self.local_count
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Access {
    InFrame(usize),
    InLocal(Local),
}
