use std::fmt::{Debug, Display};

use crate::{
    env::Env,
    parser::ast::{Decl, Expr as AstExpr, LValue, Operator, Type as AstType},
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
    #[error("function {}: expected {0} argument(s), found {1} argument(s)", .2.name())]
    MismatchArgCount(u8, u8, Symbol),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Item {
    Var,
    Func,
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Item::Var => "variable",
            Item::Func => "function",
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

    fn trans_expr(&mut self, expr: AstExpr) -> Result<ExprType> {
        match expr {
            AstExpr::LValue(lvalue) => self.trans_lvalue(lvalue),

            AstExpr::Nil(pos) => Ok(ExprType {
                expr: TransExpr::new(),
                ty: CompleteType::Nil,
            }),

            AstExpr::Sequence(exprs) => {
                // TODO: refactor
                let mut transed = exprs
                    .into_iter()
                    .map(|e| self.trans_expr(e))
                    .collect::<Result<Vec<_>>>()?;
                let ty = transed.pop().expect("zero expr sequence").ty;
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
                            .map(|v| v.actual().map_err(|e| Error::new_incomplete_type(pos, e)))
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
                                let result = result
                                    .actual()
                                    .map_err(|e| Error::new_incomplete_type(pos, e))?;
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
                        let ty = ty
                            .actual()
                            .map_err(|e| Error::new_incomplete_type(pos, e))?;
                        match ty {
                            CompleteType::Record {
                                fields: expected_fields,
                                unique,
                            } => {
                                
                            }
                            other => {}
                        }
                    }
                    None => todo!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn trans_lvalue(&mut self, var: LValue) -> Result<ExprType> {
        match var {
            LValue::Var(id, pos) => {
                let sym = Symbol::from(id);
                match self.var_env.look(sym) {
                    Some(EnvEntry::Var { ty }) => {
                        let ty = ty
                            .actual()
                            .map_err(|e| Error::new_incomplete_type(pos, e))?;
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
                                let ty = ty
                                    .actual()
                                    .map_err(|e| Error::new_incomplete_type(pos, e))?;
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
                        let ty = ty
                            .into_actual()
                            .map_err(|e| Error::new_incomplete_type(pos, e))?;
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

    fn trans_decl(&mut self, dec: Decl) {}

    // fn trans_type(&mut self, ty: AstType) -> Type {}
}

/// Variable and function entry
pub enum EnvEntry {
    Var { ty: Type },
    Func { formals: Vec<Type>, result: Type },
}

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
