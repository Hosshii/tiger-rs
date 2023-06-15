use crate::common::Label;

use super::{
    ast::{BinOp, Expr, Index, Local as AstLocal, Name, NumType, Operator, ValType},
    ExprType, StackType, FRAME_PTR, STACK_PTR,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Local(usize);

impl Local {
    pub fn new(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Param(usize);

impl Param {
    pub fn new(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Frame {
    name: Label,
    pointer: usize,
    local_count: usize, // 0 indexed
    formals: Vec<Access>,
    env: Access,
}

impl Frame {
    const WORD_SIZE: usize = 4;

    // 0 param is env
    pub fn new(name: Label, formals: Vec<bool>) -> Self {
        let mut frame = Self {
            name,
            pointer: 0,
            local_count: 0,
            formals: vec![],
            env: Access::Param(Param::new(0)),
        };
        let formals = formals
            .into_iter()
            .enumerate()
            .map(|(idx, v)| frame.alloc_param(idx, v))
            .collect();
        frame.formals = formals;
        frame
    }

    pub fn alloc_local(&mut self, is_escape: bool) -> Access {
        if is_escape {
            self.pointer += Self::WORD_SIZE;
            Access::Frame(self.pointer)
        } else {
            let count = self.local_count;
            self.local_count += 1;
            Access::Local(Local::new(count))
        }
    }

    pub fn alloc_param(&mut self, index: usize, is_escape: bool) -> Access {
        if is_escape {
            self.pointer += Self::WORD_SIZE;
            Access::Frame(self.pointer)
        } else {
            Access::Param(Param::new(index))
        }
    }

    fn param_count(&self) -> usize {
        self.formals.len()
    }

    pub fn env(&self) -> &Access {
        &self.env
    }

    pub fn formals(&self) -> &[Access] {
        self.formals.as_slice()
    }

    pub fn name(&self) -> &Label {
        &self.name
    }

    pub fn ast_locals(&self) -> Vec<AstLocal> {
        (0..self.local_count)
            .map(|_| AstLocal {
                type_: ValType::Num(NumType::I64),
                name: None,
            })
            .collect()
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

    fn local_as_index(&self, local: &Local) -> Index {
        Index::Index(local.0 as u32 + self.param_count() as u32)
    }

    fn param_as_index(&self, param: &Param) -> Index {
        Index::Index(param.0 as u32)
    }

    pub(super) fn get_access_content(&self, access: &Access, base_addr: ExprType) -> ExprType {
        base_addr.assert_ty(StackType::const_1_i32());

        let expr = match access {
            Access::Frame(val) => Expr::OpExpr(
                Operator::Bin(NumType::I32, BinOp::Sub),
                vec![
                    base_addr.val,
                    Expr::Op(Operator::Const(NumType::I32, *val as i64)),
                ],
            ),
            Access::Local(local) => Expr::Op(Operator::LocalGet(self.local_as_index(local))),
            Access::Param(param) => Expr::Op(Operator::LocalGet(self.param_as_index(param))),
        };

        ExprType::new_const_1_i64(expr)
    }

    pub(super) fn store2access(
        &self,
        access: &Access,
        base_addr: ExprType,
        expr: ExprType,
    ) -> ExprType {
        base_addr.assert_ty(StackType::const_1_i32());
        expr.assert_ty(StackType::const_1_i64());

        let expr = match access {
            Access::Frame(val) => Expr::OpExpr(
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
            Access::Local(local) => Expr::OpExpr(
                Operator::LocalSet(self.local_as_index(local)),
                vec![expr.val],
            ),
            Access::Param(param) => Expr::OpExpr(
                Operator::LocalSet(self.param_as_index(param)),
                vec![expr.val],
            ),
        };

        ExprType::new(expr, StackType::nop())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Access {
    Frame(usize),
    Local(Local),
    Param(Param),
}
