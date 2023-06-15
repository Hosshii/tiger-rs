use crate::common::Label;

use super::{
    ast::{BinOp, Expr, Index, Local as AstLocal, Name, NumType, Operator, ValType},
    ExprType, StackType, FRAME_PTR, STACK_PTR,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Local(usize, ValType);

impl Local {
    pub fn new(idx: usize, ty: ValType) -> Self {
        Self(idx, ty)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Param(usize, ValType);

impl Param {
    pub fn new(idx: usize, ty: ValType) -> Self {
        Self(idx, ty)
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
    pub fn new(name: Label, formals: Vec<(bool, ValType)>) -> Self {
        let mut frame = Self {
            name,
            pointer: 0,
            local_count: 0,
            formals: vec![],
            env: Access::Param(Param::new(0, ValType::Num(NumType::I32))),
        };
        let formals = formals
            .into_iter()
            .enumerate()
            .map(|(idx, (is_escape, ty))| frame.alloc_param(idx, is_escape, ty))
            .collect();
        frame.formals = formals;
        frame
    }

    pub fn alloc_local(&mut self, is_escape: bool, ty: ValType) -> Access {
        if is_escape {
            self.pointer += super::size(&ty);
            Access::Frame(self.pointer, ty)
        } else {
            let count = self.local_count;
            self.local_count += 1;
            Access::Local(Local::new(count, ty))
        }
    }

    pub fn alloc_param(&mut self, index: usize, is_escape: bool, ty: ValType) -> Access {
        if is_escape {
            self.pointer += super::size(&ty);
            Access::Frame(self.pointer, ty)
        } else {
            Access::Param(Param::new(index, ty))
        }
    }

    pub fn aligned_ptr(&self) -> usize {
        (self.pointer + 16)
            .checked_sub(self.pointer % 16)
            .expect("ovefrlow")
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

    pub(super) fn get_access_content(
        &self,
        access: &Access,
        base_addr: ExprType,
        ret_ty: NumType,
    ) -> ExprType {
        base_addr.assert_ty(StackType::const_1_i32());

        let expr = match access {
            Access::Frame(val, ValType::Num(ty)) => {
                assert_eq!(ty, &ret_ty);

                Expr::OpExpr(
                    Operator::Load(ret_ty),
                    vec![Expr::OpExpr(
                        Operator::Bin(NumType::I32, BinOp::Sub),
                        vec![
                            base_addr.val,
                            Expr::Op(Operator::Const(NumType::I32, *val as i64)),
                        ],
                    )],
                )
            }
            Access::Local(local) => {
                assert_eq!(local.1, ValType::Num(ret_ty));
                Expr::Op(Operator::LocalGet(self.local_as_index(local)))
            }
            Access::Param(param) => {
                assert_eq!(param.1, ValType::Num(ret_ty));
                Expr::Op(Operator::LocalGet(self.param_as_index(param)))
            }
        }
        .add_comment("get_access_content");

        ExprType::new_const_(expr, vec![ValType::Num(ret_ty)])
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
            Access::Frame(val, ValType::Num(n)) => Expr::OpExpr(
                Operator::Store(*n),
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
        }
        .add_comment("store2access");

        ExprType::new(expr, StackType::nop())
    }

    /// move escaped arg into stack
    pub(super) fn proc_entry_exit1(&self, expr: ExprType) -> ExprType {
        let exprs = self
            .formals()
            .iter()
            .enumerate()
            .flat_map(|(idx, access)| {
                if let Access::Frame(n, ValType::Num(ty)) = access {
                    let e = Expr::OpExpr(
                        Operator::Store(*ty),
                        vec![
                            Expr::OpExpr(
                                Operator::Bin(NumType::I32, BinOp::Sub),
                                vec![
                                    Self::fp().val,
                                    Expr::Op(Operator::Const(NumType::I32, *n as i64)),
                                ],
                            ),
                            Expr::Op(Operator::LocalGet(
                                self.param_as_index(&Param::new(idx, ValType::Num(*ty))),
                            )),
                        ],
                    );
                    Some(ExprType::new(e, StackType::nop()))
                } else {
                    None
                }
            })
            .chain([expr])
            .collect();
        super::expr_seq(exprs).add_comment("proc_entry_exit1")
    }

    /// prologue and epilogue
    pub(super) fn proc_entry_exit3(&self, expr: ExprType) -> ExprType {
        super::expr_seq(vec![
            // prologue
            // push rpb
            super::push(Self::fp()),
            // mov rbp, rsp
            ExprType::new(
                Expr::OpExpr(
                    Operator::GlobalSet(Index::Name(FRAME_PTR.into())),
                    vec![Self::sp().val],
                ),
                StackType::nop(),
            ),
            // sub rsp, aligned_ptr
            super::bin_stack_ptr(
                ExprType::new_const_1_i32(Expr::Op(Operator::Const(
                    NumType::I32,
                    self.aligned_ptr() as i64,
                ))),
                BinOp::Sub,
            ),
            // body
            expr,
            // epilogue
            // add rsp, aligned_ptr
            super::bin_stack_ptr(
                ExprType::new_const_1_i32(Expr::Op(Operator::Const(
                    NumType::I32,
                    self.aligned_ptr() as i64,
                ))),
                BinOp::Add,
            ),
            // mov rsp, rbp
            ExprType::new(
                Expr::OpExpr(
                    Operator::GlobalSet(Index::Name(STACK_PTR.into())),
                    vec![Self::sp().val],
                ),
                StackType::nop(),
            ),
            // pop rbp
            ExprType::new(
                Expr::OpExpr(
                    Operator::GlobalSet(Index::Name(FRAME_PTR.into())),
                    vec![super::pop(NumType::I32).val],
                ),
                StackType::nop(),
            ),
        ])
        .add_comment("proc_entry_exit3")
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Access {
    Frame(usize, ValType),
    Local(Local),
    Param(Param),
}
