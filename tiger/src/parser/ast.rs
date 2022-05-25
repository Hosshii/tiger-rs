use crate::{
    lexer::{self},
    position::Position,
};

pub type Decls = Vec<Decl>;
pub type Ident = lexer::Ident;
pub type StringLiteral = lexer::StringLiteral;

pub type Positions = (Position, Position);

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
    pub type_id: TypeIdent,
    pub pos: Positions,
}

#[derive(Debug, PartialEq, Eq)]
pub enum VarDecl {
    Short(Ident, Expr, Positions),
    Long(Ident, TypeIdent, Expr, Positions),
}

#[derive(Debug, PartialEq, Eq)]
pub enum FuncDecl {
    Short(Ident, TypeFields, Expr, Positions),
    Long(Ident, TypeFields, TypeIdent, Expr, Positions),
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
    LValue(LValue),
    Nil(Positions),
    Sequence(Vec<Expr>),
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
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>, Positions),
    IfThen(Box<Expr>, Box<Expr>, Positions),
    While(Box<Expr>, Box<Expr>, Positions),
    For(Ident, Box<Expr>, Box<Expr>, Box<Expr>, Positions),
    Break(Positions),
    Let(Decls, Vec<Expr>, Positions),
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
