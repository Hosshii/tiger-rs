use crate::{
    common::Positions,
    lexer::{self},
    parser::ast::{Operator as AstOp, TypeIdent as AstTypeIdent},
};

use super::{
    ctx::{FnId, VarId},
    types::TypeId,
};

pub type Decls = Vec<Decl>;
pub type Ident = lexer::Ident;
pub type StringLiteral = lexer::StringLiteral;

pub type Program = Expr;

#[derive(Debug, PartialEq, Eq)]
pub enum Decl {
    Type(Vec<TypeDecl>),
    Var(VarDecl),
    Func(Vec<FuncDecl>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeDecl {
    pub id: TypeIdent,
    pub type_id: TypeId,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Id(TypeIdent, TypeId, Positions),
    Fields(TypeFields, TypeId, Positions),
    Array(TypeIdent, TypeId, Positions),
}

pub type TypeFields = Vec<TypeField>;

#[derive(Debug, PartialEq, Eq)]
pub struct TypeField {
    pub ident: Ident,
    pub is_escape: bool,
    pub type_ident: TypeIdent,
    pub type_id: TypeId,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarDecl(
    pub Ident,
    pub VarId,
    pub bool,
    pub Option<TypeIdent>,
    pub TypeId,
    pub Expr,
    pub Positions,
);

pub type Params = Vec<Param>;

#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub ident: Ident,
    pub var_id: VarId,
    pub is_escape: bool,
    pub type_ident: TypeIdent,
    pub type_id: TypeId,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncDecl {
    pub name: Ident,
    pub fn_id: FnId,
    pub params: Params,
    pub ret_type: Option<TypeIdent>,
    pub re_type_id: TypeId,
    pub body: Expr,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeIdent(pub Ident);
impl From<AstTypeIdent> for TypeIdent {
    fn from(i: AstTypeIdent) -> Self {
        Self(i.0)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LValue {
    Var(Ident, VarId, TypeId, Positions),
    RecordField {
        record: Box<LValue>,
        record_type: TypeId,
        field_ident: Ident,
        field_index: usize,
        field_type: TypeId,
        pos: Positions,
    },
    Array {
        array: Box<LValue>,
        array_type: TypeId,
        index: Box<Expr>,
        index_type: TypeId,
        pos: Positions,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: TypeId,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    LValue(LValue, Positions),
    Nil(Positions),
    Sequence(Vec<Expr>, Positions),
    Int(u64, Positions),
    Str(StringLiteral, Positions),
    FuncCall(Ident, FnId, Vec<Expr>, Positions),
    Op(Operator, Box<Expr>, Box<Expr>, Positions),
    Neg(Box<Expr>, Positions),
    RecordCreation(TypeIdent, RecordFields, Positions),
    ArrayCreation {
        type_ident: TypeIdent,
        size: Box<Expr>,
        init: Box<Expr>,
        pos: Positions,
    },
    Assign(LValue, Box<Expr>, Positions),
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Option<Box<Expr>>,
        pos: Positions,
    },
    While(Box<Expr>, Box<Expr>, Positions),
    Break(Positions),
    Let(Decls, Vec<Expr>, Positions),
}

impl Expr {
    pub fn pos(&self) -> Positions {
        match &self.kind {
            ExprKind::LValue(_, pos) => *pos,
            ExprKind::Nil(pos) => *pos,
            ExprKind::Sequence(_, pos) => *pos,
            ExprKind::Int(_, pos) => *pos,
            ExprKind::Str(_, pos) => *pos,
            ExprKind::FuncCall(_, _, _, pos) => *pos,
            ExprKind::Op(_, _, _, pos) => *pos,
            ExprKind::Neg(_, pos) => *pos,
            ExprKind::RecordCreation(_, _, pos) => *pos,
            ExprKind::ArrayCreation { pos, .. } => *pos,
            ExprKind::Assign(_, _, pos) => *pos,
            ExprKind::If {
                cond: _,
                then: _,
                els: _,
                pos,
            } => *pos,
            ExprKind::While(_, _, pos) => *pos,
            ExprKind::Break(pos) => *pos,
            ExprKind::Let(_, _, pos) => *pos,
        }
    }
}

pub type RecordFields = Vec<RecordField>;

// not equal LValue::RecordField
#[derive(Debug, PartialEq, Eq)]
pub struct RecordField {
    pub ident: Ident,
    pub field_type: TypeId,
    pub expr: Expr,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,

    Eq,
    Neq,
    Ge,
    Gt,
    Le,
    Lt,

    And,
    Or,
}

impl From<AstOp> for Operator {
    fn from(op: AstOp) -> Self {
        match op {
            AstOp::Plus => Self::Plus,
            AstOp::Minus => Self::Minus,
            AstOp::Mul => Self::Mul,
            AstOp::Div => Self::Div,
            AstOp::Eq => Self::Eq,
            AstOp::Neq => Self::Neq,
            AstOp::Ge => Self::Ge,
            AstOp::Gt => Self::Gt,
            AstOp::Le => Self::Le,
            AstOp::Lt => Self::Lt,
            AstOp::And => Self::And,
            AstOp::Or => Self::Or,
        }
    }
}
