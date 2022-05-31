use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::{
    env::Env,
    parser::ast::{
        Decl, Expr as AstExpr, FuncDecl, LValue, Operator, RecordField, Type as AstType, VarDecl,
    },
    position::Positions,
    symbol::Symbol,
    translate::Expr as TransExpr,
    types::{CompleteType, IncompleteTypeError, Type, Unique},
};
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error, PartialEq, Eq)]
struct Error {
    pos: Positions,
    kind: ErrorKind,
}

impl Error {
    fn new(pos: Positions, kind: ErrorKind) -> Self {
        Self { pos, kind }
    }

    fn new_unexpected_type(
        pos: Positions,
        expected: Vec<CompleteType>,
        actual: CompleteType,
    ) -> Self {
        Self {
            pos,
            kind: ErrorKind::UnExpectedType(expected, actual),
        }
    }

    fn new_incomplete_type(pos: Positions, e: IncompleteTypeError) -> Self {
        Self {
            pos,
            kind: e.into(),
        }
    }

    fn new_unexpected_func(pos: Positions, sym: Symbol) -> Self {
        Self {
            pos,
            kind: ErrorKind::UnexpectedFunc(sym),
        }
    }

    fn new_unexpected_variable(pos: Positions, sym: Symbol) -> Self {
        Self {
            pos,
            kind: ErrorKind::UnexpectedVar(sym),
        }
    }

    fn new_undefined_item(pos: Positions, item: Item, sym: Symbol) -> Self {
        Self {
            pos,
            kind: ErrorKind::UndefinedItem(item, sym),
        }
    }

    fn new_missing_field(pos: Positions, ty: CompleteType, field: Symbol) -> Self {
        Self {
            pos,
            kind: ErrorKind::MissingFieldName(ty, field),
        }
    }

    fn new_missing_field_on_record_creation(
        pos: Positions,
        ty: CompleteType,
        field: Symbol,
    ) -> Self {
        Self {
            pos,
            kind: ErrorKind::MissingFieldOnRecordCreation(ty, field),
        }
    }

    fn new_mismatch_arg_count(pos: Positions, expected: u8, actual: u8, sym: Symbol) -> Self {
        Self {
            pos,
            kind: ErrorKind::MismatchArgCount(expected, actual, sym),
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
enum ErrorKind {
    #[error("unexpected type: expected {0:?}, found: {1:?}")]
    UnExpectedType(Vec<CompleteType>, CompleteType),
    #[error("incomplete type: {}", (.0).0.name())]
    InCompleteType(#[from] IncompleteTypeError),
    #[error("unexpected func item: {}, expected var item", .0.name())]
    UnexpectedFunc(Symbol),
    #[error("unexpected var item: {}, expected func item", .0.name())]
    UnexpectedVar(Symbol),
    #[error("undefined {0}: {}",.1.name())]
    UndefinedItem(Item, Symbol),
    #[error("no field `{:?}` on type `{}`",.0, .1.name())]
    MissingFieldName(CompleteType, Symbol),
    #[error("field `{}` on `{:?}` is not initialized",.1.name(),.0)]
    MissingFieldOnRecordCreation(CompleteType, Symbol),
    #[error("function {}: expected {0} argument(s), found {1} argument(s)", .2.name())]
    MismatchArgCount(u8, u8, Symbol),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.pos)?;
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Item {
    Var,
    Func,
    Type,
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Item::Var => "variable",
            Item::Func => "function",
            Item::Type => "type",
        };
        write!(f, "{}", name)
    }
}

pub struct Semant {
    var_env: Env<EnvEntry>, // var and func env
    type_env: Env<Type>,    // type env
}

impl Semant {
    pub fn new(var_base: Env<EnvEntry>, type_base: Env<Type>) -> Self {
        Self {
            var_env: var_base,
            type_env: type_base,
        }
    }

    pub fn trans_prog(&mut self, expr: AstExpr) {
        match self.trans_expr(expr) {
            Ok(t) => println!("success!, {:?}", t),
            Err(e) => println!("{}", e),
        }
    }

    fn check_type<T>(&mut self, expr: AstExpr, ty: T, pos: Positions) -> Result<()>
    where
        T: TupleMatch<Item = CompleteType>,
    {
        let ExprType { ty: got, .. } = self.trans_expr(expr)?;

        if ty.matches(&got) {
            Ok(())
        } else {
            Err(Error {
                pos,
                kind: ErrorKind::UnExpectedType(ty.to_vec(), got),
            })
        }
    }

    fn new_scope<F>(&mut self, f: F) -> Result<ExprType>
    where
        F: FnOnce(&mut Self) -> Result<ExprType>,
    {
        self.type_env.begin_scope();
        self.var_env.begin_scope();
        let result = f(self);
        self.type_env.end_scope();
        self.var_env.end_scope();
        result
    }

    fn trans_decl(&mut self, decl: Decl) -> Result<()> {
        match decl {
            Decl::Type(type_decls) => {
                for decl in type_decls.into_iter() {
                    let id = decl.id;
                    let sym = Symbol::from(id);
                    let _type = self.trans_type(decl.ty)?;
                    self.type_env.enter(sym, _type);
                }
                Ok(())
            }
            Decl::Var(VarDecl(id, ty, expr, pos)) => {
                let sym = Symbol::from(id);
                let ExprType { ty: expr_ty, .. } = self.trans_expr(expr)?;

                if let Some(type_id) = ty {
                    let ty_symbol = Symbol::from(type_id);
                    match self.type_env.look(ty_symbol) {
                        Some(expected_ty) => {
                            // todo recursion
                            let expected_ty = actual_ty(expected_ty, pos)?.clone();
                            if expected_ty != expr_ty {
                                return Err(Error::new_unexpected_type(
                                    pos,
                                    vec![expected_ty],
                                    expr_ty,
                                ));
                            }
                        }
                        None => return Err(Error::new_undefined_item(pos, Item::Type, ty_symbol)),
                    }
                }

                self.var_env.enter(
                    sym,
                    EnvEntry::Var {
                        ty: Type::Complete(expr_ty),
                    },
                );

                Ok(())
            }
            Decl::Func(fn_decls) => self.trans_fn_decls(fn_decls),
        }
    }

    fn trans_fn_decls(&mut self, func_decls: Vec<FuncDecl>) -> Result<()> {
        for func_decl in func_decls {
            // enter function defs
            let func_name = Symbol::from(func_decl.name);
            let formals = func_decl
                .params
                .iter()
                .map(|f| {
                    let ty_sym = Symbol::from(&f.type_id);
                    match self.type_env.look(ty_sym) {
                        Some(ty) => Ok(ty.clone()),
                        None => Err(Error::new_undefined_item(f.pos, Item::Type, ty_sym)),
                    }
                })
                .collect::<Result<Vec<_>>>()?;

            let ret_type = match func_decl.ret_type {
                Some(type_id) => {
                    let ty_sym = Symbol::from(type_id);
                    match self.type_env.look(ty_sym) {
                        Some(t) => t.clone(),
                        None => {
                            return Err(Error::new_undefined_item(
                                func_decl.pos,
                                Item::Type,
                                ty_sym,
                            ));
                        }
                    }
                }
                None => Type::Complete(CompleteType::Unit),
            };

            self.var_env.enter(
                func_name,
                EnvEntry::Func {
                    formals: formals.clone(),
                    result: ret_type.clone(),
                },
            );

            // function body
            let ret_type = into_actual_ty(ret_type, func_decl.pos)?;
            self.new_scope(|_self| {
                for (ty, id) in formals
                    .into_iter()
                    .zip(func_decl.params.into_iter().map(|f| f.id))
                {
                    let sym = Symbol::from(id);
                    _self.var_env.enter(sym, EnvEntry::Var { ty })
                }
                let ty = _self.trans_expr(func_decl.body)?;
                if ty.ty != ret_type {
                    Err(Error::new_unexpected_type(
                        func_decl.pos,
                        vec![ret_type],
                        ty.ty,
                    ))
                } else {
                    Ok(ty)
                }
            })?;
        }

        Ok(())
    }

    fn trans_expr(&mut self, expr: AstExpr) -> Result<ExprType> {
        match expr {
            AstExpr::LValue(lvalue, _) => self.trans_lvalue(lvalue),

            AstExpr::Nil(pos) => Ok(ExprType {
                expr: TransExpr::new(),
                ty: CompleteType::Nil,
            }),

            AstExpr::Sequence(exprs, _) => {
                // TODO: refactor
                let mut transed = exprs
                    .into_iter()
                    .map(|e| self.trans_expr(e))
                    .collect::<Result<Vec<_>>>()?;

                let ty = match transed.pop() {
                    Some(t) => t.ty,
                    None => CompleteType::Unit,
                };

                Ok(ExprType {
                    expr: TransExpr::new(),
                    ty,
                })
            }

            AstExpr::Int(_, _) => Ok(ExprType {
                expr: TransExpr::new(),
                ty: CompleteType::Int,
            }),

            AstExpr::Str(_, _) => Ok(ExprType {
                expr: TransExpr::new(),
                ty: CompleteType::String,
            }),

            AstExpr::FuncCall(ident, args, pos) => {
                let sym = Symbol::from(ident);
                let args_len = args.len();

                // proceed trans_expr first for ownership reason.
                let arg_iter = args
                    .into_iter()
                    .map(|e| self.trans_expr(e))
                    .collect::<Result<Vec<_>>>()?;

                match self.var_env.look(sym) {
                    Some(EnvEntry::Func { formals, result }) => {
                        if args_len != formals.len() {
                            return Err(Error::new_mismatch_arg_count(
                                pos,
                                formals.len() as u8,
                                args_len as u8,
                                sym,
                            ));
                        }

                        let formal_iter = formals
                            .iter()
                            .map(|v| actual_ty(v, pos))
                            .collect::<Result<Vec<_>>>()?;

                        let unmatched = arg_iter
                            .into_iter()
                            .zip(formal_iter.iter())
                            .enumerate()
                            .find(|(_, (ExprType { ty: lhs, .. }, rhs))| lhs != **rhs);

                        match unmatched {
                            Some((_, (ExprType { ty: actual, .. }, &expected))) => Err(
                                Error::new_unexpected_type(pos, vec![expected.clone()], actual),
                            ),
                            None => {
                                let result = actual_ty(result, pos)?;
                                Ok(ExprType {
                                    expr: TransExpr::new(),
                                    ty: result.clone(),
                                })
                            }
                        }
                    }

                    Some(_) => Err(Error::new_unexpected_variable(pos, sym)),
                    None => Err(Error::new_undefined_item(pos, Item::Func, sym)),
                }
            }

            // int only
            AstExpr::Op(
                Operator::Plus
                | Operator::Minus
                | Operator::Mul
                | Operator::Div
                | Operator::And
                | Operator::Or,
                lhs,
                rhs,
                pos,
            ) => {
                self.check_type(*lhs, (CompleteType::Int,), pos)?;
                self.check_type(*rhs, (CompleteType::Int,), pos)?;
                Ok(ExprType {
                    expr: TransExpr::new(),
                    ty: CompleteType::Int,
                })
            }

            // int and string
            AstExpr::Op(
                Operator::Le | Operator::Lt | Operator::Ge | Operator::Gt,
                lhs,
                rhs,
                pos,
            ) => {
                let ExprType { ty: lhs_ty, .. } = self.trans_expr(*lhs)?;
                let ExprType { ty: rhs_ty, .. } = self.trans_expr(*rhs)?;
                match (lhs_ty, rhs_ty) {
                    (CompleteType::Int, CompleteType::Int)
                    | (CompleteType::String, CompleteType::String) => Ok(ExprType {
                        expr: TransExpr::new(),
                        ty: CompleteType::Int,
                    }),
                    other => Err(Error::new_unexpected_type(pos, vec![other.0], other.1)),
                }
            }

            // compare two type
            AstExpr::Op(Operator::Eq | Operator::Neq, lhs, rhs, pos) => {
                let ExprType { ty: lhs_ty, .. } = self.trans_expr(*lhs)?;
                let ExprType { ty: rhs_ty, .. } = self.trans_expr(*rhs)?;
                if lhs_ty == rhs_ty {
                    Ok(ExprType {
                        expr: TransExpr::new(),
                        ty: CompleteType::Int,
                    })
                } else {
                    Err(Error::new_unexpected_type(pos, vec![lhs_ty], rhs_ty))
                }
            }

            AstExpr::Neg(e, _) => {
                let ExprType { ty, .. } = self.trans_expr(*e)?;
                Ok(ExprType {
                    expr: TransExpr::new(),
                    ty,
                })
            }

            AstExpr::RecordCreation(type_id, actual_fields, pos) => {
                let sym = Symbol::from(type_id);
                match self.type_env.look(sym) {
                    Some(ty) => {
                        let ty = actual_ty(ty, pos)?;

                        match ty {
                            CompleteType::Record {
                                fields: expected_fields,
                                ..
                            } => {
                                let mut expected_map = expected_fields
                                    .iter()
                                    .map(|f| {
                                        let ty = actual_ty(&f.1, pos)?;
                                        Ok((f.0, ty.clone()))
                                    })
                                    .collect::<Result<HashMap<_, _>>>()?;

                                let ty = ty.clone();
                                for RecordField { id, expr, pos } in actual_fields {
                                    let actual_sym = Symbol::from(id);
                                    match expected_map.remove(&actual_sym) {
                                        None => {
                                            return Err(Error::new_missing_field(
                                                pos, ty, actual_sym,
                                            ))
                                        }

                                        Some(expected_field_type) => {
                                            let ExprType {
                                                ty: actual_field_type,
                                                ..
                                            } = self.trans_expr(expr)?;

                                            if expected_field_type != actual_field_type {
                                                return Err(Error::new_unexpected_type(
                                                    pos,
                                                    vec![expected_field_type],
                                                    actual_field_type,
                                                ));
                                            }
                                        }
                                    }
                                }

                                if expected_map.is_empty() {
                                    Ok(ExprType {
                                        expr: TransExpr::new(),
                                        ty,
                                    })
                                } else {
                                    let sym = expected_map.iter().next().unwrap();
                                    Err(Error::new_missing_field_on_record_creation(
                                        pos, ty, *sym.0,
                                    ))
                                }
                            }
                            other => Err(Error::new_unexpected_type(
                                pos,
                                vec![CompleteType::dummy_record()],
                                other.clone(),
                            )),
                        }
                    }
                    None => Err(Error::new_undefined_item(pos, Item::Type, sym)),
                }
            }

            AstExpr::ArrayCreation {
                type_id,
                size,
                init,
                pos,
            } => {
                let sym = Symbol::from(type_id);
                match self.type_env.look(sym) {
                    Some(ty) => {
                        let ty = actual_ty(ty, pos)?;

                        match ty {
                            CompleteType::Array { ty, unique } => {
                                let unique = *unique;
                                let ty = ty.clone();

                                let arr_ty = actual_ty(&ty, pos)?;

                                let size_pos = size.pos();
                                self.check_type(*size, (CompleteType::Int,), size_pos)?;

                                let init_pos = init.pos();
                                self.check_type(*init, (arr_ty.clone(),), init_pos)?;

                                Ok(ExprType {
                                    expr: TransExpr::new(),
                                    ty: CompleteType::Array { ty, unique },
                                })
                            }
                            other => Err(Error::new_unexpected_type(
                                pos,
                                vec![CompleteType::dummy_record()],
                                other.clone(),
                            )),
                        }
                    }
                    None => Err(Error::new_undefined_item(pos, Item::Type, sym)),
                }
            }

            AstExpr::Assign(lvalue, expr, pos) => {
                let ExprType { ty: lhs_ty, .. } = self.trans_lvalue(lvalue)?;
                let ExprType { ty: rhs_ty, .. } = self.trans_expr(*expr)?;
                if lhs_ty == rhs_ty {
                    Ok(ExprType {
                        expr: TransExpr::new(),
                        ty: CompleteType::Unit,
                    })
                } else {
                    Err(Error::new_unexpected_type(pos, vec![lhs_ty], rhs_ty))
                }
            }

            AstExpr::If(cond, then, els, pos) => {
                let cond_pos = cond.pos();
                self.check_type(*cond, (CompleteType::Int,), cond_pos)?;

                let then_pos = then.pos();
                let ExprType { ty: then_ty, .. } = self.trans_expr(*then)?;
                match els {
                    Some(els) => {
                        let els_pos = els.pos();
                        self.check_type(*els, (then_ty.clone(),), els_pos)?;

                        Ok(ExprType {
                            expr: TransExpr::new(),
                            ty: then_ty,
                        })
                    }
                    None => {
                        if then_ty == CompleteType::Unit {
                            Ok(ExprType {
                                expr: TransExpr::new(),
                                ty: CompleteType::Unit,
                            })
                        } else {
                            Err(Error::new_unexpected_type(
                                then_pos,
                                vec![then_ty],
                                CompleteType::Unit,
                            ))
                        }
                    }
                }
            }

            AstExpr::While(cond, then, _) => {
                let cond_pos = cond.pos();
                self.check_type(*cond, (CompleteType::Int,), cond_pos)?;
                let then_pos = then.pos();
                self.check_type(*then, (CompleteType::Unit,), then_pos)?;

                Ok(ExprType {
                    expr: TransExpr::new(),
                    ty: CompleteType::Unit,
                })
            }

            AstExpr::For(id, expr1, expr2, then, _) => {
                let expr1_pos = expr1.pos();
                self.check_type(*expr1, (CompleteType::Int,), expr1_pos)?;

                let expr2_pos = expr2.pos();
                self.check_type(*expr2, (CompleteType::Int,), expr2_pos)?;

                let then_pos = then.pos();
                self.check_type(*then, (CompleteType::Unit,), then_pos)?;

                Ok(ExprType {
                    expr: TransExpr::new(),
                    ty: CompleteType::Unit,
                })
            }

            AstExpr::Break(pos) => Ok(ExprType {
                expr: TransExpr::new(),
                ty: CompleteType::Unit,
            }),

            AstExpr::Let(decls, exprs, pos) => self.new_scope(|_self| {
                for decl in decls {
                    _self.trans_decl(decl)?;
                }
                let ExprType { ty, .. } = _self.trans_expr(AstExpr::Sequence(exprs, pos))?;
                Ok(ExprType {
                    expr: TransExpr::new(),
                    ty,
                })
            }),
        }
    }

    fn trans_lvalue(&mut self, var: LValue) -> Result<ExprType> {
        match var {
            LValue::Var(id, pos) => {
                let sym = Symbol::from(id);
                match self.var_env.look(sym) {
                    Some(EnvEntry::Var { ty }) => {
                        let ty = actual_ty(ty, pos)?;
                        Ok(ExprType {
                            expr: TransExpr::new(),
                            ty: ty.clone(),
                        })
                    }
                    Some(_) => Err(Error::new_unexpected_func(pos, sym)),
                    None => Err(Error::new_undefined_item(pos, Item::Var, sym)),
                }
            }
            LValue::RecordField(lvar, id, pos) => {
                let ExprType { ty, .. } = self.trans_lvalue(*lvar)?;
                let sym = Symbol::from(id);
                match ty {
                    CompleteType::Record { fields, unique } => {
                        let field = fields.iter().find(|v| v.0 == sym);
                        match field {
                            None => Err(Error::new_missing_field(
                                pos,
                                CompleteType::Record { fields, unique },
                                sym,
                            )),
                            Some((_, ty)) => {
                                let ty = actual_ty(ty, pos)?;
                                Ok(ExprType {
                                    expr: TransExpr::new(),
                                    ty: ty.clone(),
                                })
                            }
                        }
                    }
                    other => Err(Error::new_unexpected_type(
                        pos,
                        vec![CompleteType::dummy_record()],
                        other,
                    )),
                }
            }
            LValue::Array(lvar, expr, pos) => {
                let ExprType { ty, .. } = self.trans_expr(*expr)?;
                if ty != CompleteType::Int {
                    return Err(Error::new_unexpected_type(pos, vec![CompleteType::Int], ty));
                }

                let ExprType { ty, .. } = self.trans_lvalue(*lvar)?;
                match ty {
                    CompleteType::Array { ty, unique: _ } => {
                        let ty = into_actual_ty(*ty, pos)?;
                        Ok(ExprType {
                            expr: TransExpr::new(),
                            ty,
                        })
                    }
                    other => Err(Error::new_unexpected_type(
                        pos,
                        vec![CompleteType::dummy_array()],
                        other,
                    )),
                }
            }
        }
    }

    fn trans_type(&mut self, ast_type: AstType) -> Result<Type> {
        match ast_type {
            AstType::Id(type_id, pos) => {
                let sym = Symbol::from(type_id);
                self.type_env
                    .look(sym)
                    .cloned()
                    .ok_or_else(|| Error::new_undefined_item(pos, Item::Type, sym))
            }

            AstType::Fields(fields, _) => {
                let fields = fields
                    .into_iter()
                    .map(|f| {
                        let name = Symbol::from(f.id);
                        let _type = Symbol::from(f.type_id);
                        let _type = self
                            .type_env
                            .look(_type)
                            .cloned()
                            .ok_or_else(|| Error::new_undefined_item(f.pos, Item::Type, _type));
                        _type.map(|v| (name, v))
                    })
                    .collect::<Result<Vec<_>>>()?;

                let ty = CompleteType::Record {
                    fields,
                    unique: Unique::new(),
                };

                Ok(Type::Complete(ty))
            }

            AstType::Array(type_id, pos) => {
                let sym = Symbol::from(type_id);
                let elem_ty = self
                    .type_env
                    .look(sym)
                    .cloned()
                    .ok_or_else(|| Error::new_undefined_item(pos, Item::Type, sym))?;

                Ok(Type::Complete(CompleteType::Array {
                    ty: Box::new(elem_ty),
                    unique: Unique::new(),
                }))
            }
        }
    }
}

fn actual_ty(ty: &Type, pos: Positions) -> Result<&CompleteType> {
    ty.actual().map_err(|e| Error::new_incomplete_type(pos, e))
}

fn into_actual_ty(ty: Type, pos: Positions) -> Result<CompleteType> {
    ty.into_actual()
        .map_err(|e| Error::new_incomplete_type(pos, e))
}

/// Variable and function entry
pub enum EnvEntry {
    Var { ty: Type },
    Func { formals: Vec<Type>, result: Type },
}

impl Env<Type> {
    pub fn with_base_type(mut self) -> Self {
        let base_types = vec![("int", CompleteType::Int), ("string", CompleteType::String)];

        for (sym, ty) in base_types {
            let sym = Symbol::new(sym);
            let ty = Type::Complete(ty);
            self.enter(sym, ty);
        }
        self.begin_scope();
        self
    }
}

#[derive(Debug)]
struct ExprType {
    expr: TransExpr,
    ty: CompleteType,
}

trait TupleMatch: Debug {
    type Item;

    fn matches(&self, item: &Self::Item) -> bool;
    fn to_vec(self) -> Vec<Self::Item>;
}

macro_rules! impl_tuple {
    ($t:ty, $(($n:tt, $t2:ty)),*) => {
        impl TupleMatch for ($($t2,)*) {
            type Item = $t;

            fn matches(&self, item: &Self::Item) -> bool {
                $(item == &self.$n || )* false
            }

            fn to_vec(self)-> Vec<Self::Item>{
                vec![
                    $(self.$n,)*
                ]
            }
        }
    }
}

impl_tuple!(CompleteType, (0, CompleteType));
impl_tuple!(CompleteType, (0, CompleteType), (1, CompleteType));
impl_tuple!(
    CompleteType,
    (0, CompleteType),
    (1, CompleteType),
    (2, CompleteType)
);
impl_tuple!(
    CompleteType,
    (0, CompleteType),
    (1, CompleteType),
    (2, CompleteType),
    (3, CompleteType)
);
impl_tuple!(
    CompleteType,
    (0, CompleteType),
    (1, CompleteType),
    (2, CompleteType),
    (3, CompleteType),
    (4, CompleteType)
);

#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn test_tuple_match() {
        let case: Vec<(&dyn TupleMatch<Item = CompleteType>, CompleteType, bool)> = vec![
            (
                &(CompleteType::Int, CompleteType::String) as &dyn TupleMatch<Item = CompleteType>,
                CompleteType::Int,
                true,
            ),
            (
                &(CompleteType::Nil, CompleteType::Unit, CompleteType::String)
                    as &dyn TupleMatch<Item = CompleteType>,
                CompleteType::Int,
                false,
            ),
            (
                &(CompleteType::Nil,) as &dyn TupleMatch<Item = CompleteType>,
                CompleteType::Int,
                false,
            ),
        ];

        for (tr, ref cond, expected) in case {
            assert_eq!(expected, tr.matches(cond));
        }
    }
}
