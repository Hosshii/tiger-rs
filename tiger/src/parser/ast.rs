use crate::{
    common::Positions,
    lexer::{self},
};

pub type Decls = Vec<Decl>;
pub type Ident = lexer::Ident;
pub type StringLiteral = lexer::StringLiteral;

#[derive(Debug, PartialEq, Eq)]
pub enum Program {
    Expr(Expr),
    Decls(Decls),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Decl {
    Type(Vec<TypeDecl>),
    Var(VarDecl),
    Func(Vec<FuncDecl>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeDecl {
    pub id: TypeIdent,
    pub ty: Type,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Id(TypeIdent, Positions),
    Fields(TypeFields, Positions),
    Array(TypeIdent, Positions),
}

pub type TypeFields = Vec<TypeField>;

#[derive(Debug, PartialEq, Eq)]
pub struct TypeField {
    pub id: Ident,
    pub is_escape: bool,
    pub type_id: TypeIdent,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarDecl(
    pub Ident,
    pub bool,
    pub Option<TypeIdent>,
    pub Expr,
    pub Positions,
);

#[derive(Debug, PartialEq, Eq)]
pub struct FuncDecl {
    pub name: Ident,
    pub params: TypeFields,
    pub ret_type: Option<TypeIdent>,
    pub body: Expr,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeIdent(pub Ident);

#[derive(Debug, PartialEq, Eq)]
pub enum LValue {
    Var(Ident, Positions),
    RecordField(Box<LValue>, Ident, Positions),
    Array(Box<LValue>, Box<Expr>, Positions),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    LValue(LValue, Positions),
    Nil(Positions),
    Sequence(Vec<Expr>, Positions),
    Int(u64, Positions),
    Str(StringLiteral, Positions),
    FuncCall(Ident, Vec<Expr>, Positions),
    Op(Operator, Box<Expr>, Box<Expr>, Positions),
    Neg(Box<Expr>, Positions),
    RecordCreation(TypeIdent, RecordFields, Positions),
    ArrayCreation {
        type_id: TypeIdent,
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
    For {
        id: Ident,
        is_escape: bool,
        from: Box<Expr>,
        to: Box<Expr>,
        then: Box<Expr>,
        pos: Positions,
    },
    Break(Positions),
    Let(Decls, Vec<Expr>, Positions),
}

impl Expr {
    pub fn pos(&self) -> Positions {
        match self {
            Expr::LValue(_, pos) => *pos,
            Expr::Nil(pos) => *pos,
            Expr::Sequence(_, pos) => *pos,
            Expr::Int(_, pos) => *pos,
            Expr::Str(_, pos) => *pos,
            Expr::FuncCall(_, _, pos) => *pos,
            Expr::Op(_, _, _, pos) => *pos,
            Expr::Neg(_, pos) => *pos,
            Expr::RecordCreation(_, _, pos) => *pos,
            Expr::ArrayCreation { pos, .. } => *pos,
            Expr::Assign(_, _, pos) => *pos,
            Expr::If {
                cond: _,
                then: _,
                els: _,
                pos,
            } => *pos,
            Expr::While(_, _, pos) => *pos,
            Expr::For {
                id: _,
                is_escape: _,
                from: _,
                to: _,
                then: _,
                pos,
            } => *pos,
            Expr::Break(pos) => *pos,
            Expr::Let(_, _, pos) => *pos,
        }
    }
}

pub type RecordFields = Vec<RecordField>;

// not equal LValue::RecordField
#[derive(Debug, PartialEq, Eq)]
pub struct RecordField {
    pub id: Ident,
    pub expr: Expr,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq)]
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
