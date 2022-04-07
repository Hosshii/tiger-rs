use std::collections::HashMap;

use crate::lexer::{self};

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
    Type(TypeDecl),
    Var(VarDecl),
    Func(FuncDecl),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeDecl {
    pub id: TypeIdent,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Id(TypeIdent),
    Fields(TypeFields),
    Array(TypeIdent),
}

pub type TypeFields = Vec<TypeField>;

#[derive(Debug, PartialEq, Eq)]
pub struct TypeField {
    pub id: Ident,
    pub type_id: TypeIdent,
}

#[derive(Debug, PartialEq, Eq)]
pub enum VarDecl {
    Short(Ident, Expr),
    Long(Ident, TypeIdent, Expr),
}

#[derive(Debug, PartialEq, Eq)]
pub enum FuncDecl {
    Short(Ident, TypeFields, Expr),
    Long(Ident, TypeFields, TypeIdent, Expr),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BuiltinTypeIdent {
    Int,
    String,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeIdent {
    Builtin(BuiltinTypeIdent),
    User(Ident),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LValue {
    Var(Ident),
    RecordField(Box<LValue>, Ident),
    Array(Box<LValue>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    LValue(LValue),
    Nil,
    Sequence(Vec<Expr>),
    Int(u64),
    Str(StringLiteral),
    FuncCall(Ident, Vec<Expr>),
    Op(Operator, Box<Expr>, Box<Expr>),
    RecordCreation(TypeIdent, RecordFields),
    ArrayCreation {
        type_id: TypeIdent,
        size: Box<Expr>,
        init: Box<Expr>,
    },
    Assign(LValue, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    IfThen(Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    For(Ident, Box<Expr>, Box<Expr>, Box<Expr>),
    Break,
    Let(Decls, Vec<Expr>),
}

pub type RecordFields = Vec<RecordField>;

// not equal LValue::RecordField
#[derive(Debug, PartialEq, Eq)]
pub struct RecordField {
    pub id: Ident,
    pub expr: Expr,
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
