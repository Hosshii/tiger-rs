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
    locals: Vec<Local>,
    formals: Vec<Access>,
    env: Access,
}

impl Frame {
    pub const WORD_SIZE: usize = 4;

    // 0 param is env
    pub fn new(name: Label, formals: Vec<(bool, ValType)>) -> Self {
        let mut frame = Self {
            name,
            pointer: 0,
            locals: Vec::new(),
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
            self.pointer += ty.byte_size();
            Access::Frame(self.pointer, ty)
        } else {
            let count = self.locals.len();
            let local = Local::new(count, ty);
            self.locals.push(local);
            Access::Local(local)
        }
    }

    pub fn alloc_param(&mut self, index: usize, is_escape: bool, ty: ValType) -> Access {
        if is_escape {
            self.pointer += &ty.byte_size();
            Access::Frame(self.pointer, ty)
        } else {
            Access::Param(Param::new(index, ty))
        }
    }

    pub(super) fn init_local(&mut self, is_escape: bool, expr: ExprType) -> (Access, ExprType) {
        let access = self.alloc_local(is_escape, expr.ty.result()[0]);
        let expr = self.store2access(&access, Self::fp(), expr);
        (access, expr)
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
        self.locals
            .iter()
            .map(|local| AstLocal {
                type_: local.1,
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
            Access::Frame(val, ValType::Num(ty)) => Expr::OpExpr(
                Operator::Load(*ty),
                vec![Expr::OpExpr(
                    Operator::Bin(NumType::I32, BinOp::Sub),
                    vec![
                        base_addr.val,
                        Expr::Op(Operator::Const(NumType::I32, *val as i64)),
                    ],
                )],
            ),
            Access::Local(local) => Expr::Op(Operator::LocalGet(self.local_as_index(local))),
            Access::Param(param) => Expr::Op(Operator::LocalGet(self.param_as_index(param))),
        }
        .add_comment("get_access_content");

        ExprType::new_const_(expr, vec![access.type_()])
    }

    pub(super) fn store2access(
        &self,
        access: &Access,
        base_addr: ExprType,
        expr: ExprType,
    ) -> ExprType {
        base_addr.assert_ty(StackType::const_1_i32());
        assert!(expr.ty.is_const_1());

        let expr = match access {
            Access::Frame(val, val_ty @ ValType::Num(n)) => {
                expr.assert_ty(StackType::const_(vec![*val_ty]));
                Expr::OpExpr(
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
                )
            }
            Access::Local(local) => {
                assert_eq!(local.1, expr.ty.result()[0]);
                Expr::OpExpr(
                    Operator::LocalSet(self.local_as_index(local)),
                    vec![expr.val],
                )
            }
            Access::Param(param) => {
                assert_eq!(param.1, expr.ty.result()[0]);
                Expr::OpExpr(
                    Operator::LocalSet(self.param_as_index(param)),
                    vec![expr.val],
                )
            }
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
            .chain([expr.add_comment("proc_entry_exit1 body")])
            .collect();
        super::concat_exprs(exprs).add_comment("proc_entry_exit1")
    }

    /// prologue and epilogue
    pub(super) fn proc_entry_exit3(&self, expr: ExprType) -> ExprType {
        super::concat_exprs(vec![
            // prologue
            // push rpb
            super::push(Self::fp()).add_comment("prologue start"),
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
            )
            .add_comment("prologue end"),
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
            )
            .add_comment("epilogue start"),
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
            )
            .add_comment("epilogue end"),
        ])
        .add_comment("proc_entry_exit3")
    }

    pub(super) fn extern_call(name: &str, args: Vec<ExprType>, ret_ty: Vec<ValType>) -> ExprType {
        let expr = Expr::OpExpr(
            Operator::Call(Index::Name(name.into())),
            args.into_iter().map(|e| e.val).collect(),
        );
        ExprType::new(expr, StackType::const_(ret_ty))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Access {
    Frame(usize, ValType),
    Local(Local),
    Param(Param),
}

impl Access {
    pub fn type_(&self) -> ValType {
        match self {
            Access::Frame(_, ty) => *ty,
            Access::Local(local) => local.1,
            Access::Param(param) => param.1,
        }
    }
}

pub trait Size {
    fn byte_size(&self) -> usize;
    fn word_size(&self) -> usize {
        assert!(self.byte_size() % Frame::WORD_SIZE == 0);
        self.byte_size() / Frame::WORD_SIZE
    }
}

impl Size for NumType {
    fn byte_size(&self) -> usize {
        match self {
            NumType::I32 => 4,
            NumType::I64 => 8,
            _ => unimplemented!(),
        }
    }
}

impl Size for ValType {
    fn byte_size(&self) -> usize {
        match self {
            ValType::Num(ty) => ty.byte_size(),
        }
    }
}
