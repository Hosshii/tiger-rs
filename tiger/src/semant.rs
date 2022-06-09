use std::{
    collections::{HashMap, HashSet},
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
    types::{CompleteType, IncompleteType, IncompleteTypeError, Type, Unique},
};
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error, PartialEq, Eq)]
pub struct Error {
    pos: Positions,
    kind: ErrorKind,
}

impl Error {
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

    fn new_unconstrained_nil_initialize(pos: Positions) -> Self {
        Self {
            pos,
            kind: ErrorKind::UnconstrainedNilInitialize,
        }
    }

    fn new_unit_initialize(pos: Positions) -> Self {
        Self {
            pos,
            kind: ErrorKind::UnitInitialize,
        }
    }

    fn new_same_name(item: Item, sym: Symbol, pos: Positions) -> Self {
        Self {
            pos,
            kind: ErrorKind::SameNameInAdjacentDecl(item, sym),
        }
    }

    fn new_invalid_recursion(syms: Vec<Symbol>, pos: Positions) -> Self {
        Self {
            pos,
            kind: ErrorKind::InvalidRecursion(syms),
        }
    }

    fn new_invalid_break(pos: Positions) -> Self {
        Self {
            pos,
            kind: ErrorKind::InvalidBreak,
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
    #[error("initializing nil expressions must be constrained by record type")]
    UnconstrainedNilInitialize,
    #[error("initializing with unit is invalid")]
    UnitInitialize,
    #[error("duplicate name `{}`: same name in the same batch of mutually recursive {0} is invalid", .1.name())]
    SameNameInAdjacentDecl(Item, Symbol),
    #[error("{:?} are invalid recursion",(.0).iter().map(|s|s.name()).collect::<Vec<_>>())]
    InvalidRecursion(Vec<Symbol>),
    #[error("break statement must be inside `for` or `while` statement")]
    InvalidBreak,
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
    break_count: u32, // 0 means not inside break or while. greater than 1 means inside break or while
}

impl Semant {
    pub fn new(var_base: Env<EnvEntry>, type_base: Env<Type>) -> Self {
        Self {
            var_env: var_base,
            type_env: type_base,
            break_count: 0,
        }
    }

    pub fn new_with_base() -> Self {
        let var_env = Env::new().with_base_var();
        let type_env = Env::new().with_base_type();

        Self {
            var_env,
            type_env,
            break_count: 0,
        }
    }

    fn actual_ty<'a>(&'a self, ty: &'a Type, pos: Positions) -> Result<&'a CompleteType> {
        ty.actual(&self.type_env)
            .map_err(|e| Error::new_incomplete_type(pos, e))
    }

    pub fn trans_prog(&mut self, expr: AstExpr) -> Result<ExprType> {
        self.trans_expr(expr)
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

    fn detect_invalid_recursion(&mut self, iter: &[(Symbol, Positions)]) -> Result<()> {
        for &(sym, pos) in iter {
            let mut seen = HashSet::new();
            seen.insert(sym);
            match self.type_env.look(sym) {
                Some(ty) => match self.detect_invalid_recursion_inner(ty, pos, &mut seen) {
                    Ok(_) => continue,
                    e => return e.map(|_| ()),
                },
                None => return Err(Error::new_undefined_item(pos, Item::Type, sym)),
            }
        }
        Ok(())
    }

    // caller must insert `ty` 's symbol into seen.
    fn detect_invalid_recursion_inner(
        &self,
        ty: &Type,
        pos: Positions,
        seen: &mut HashSet<Symbol>,
    ) -> Result<()> {
        match ty {
            Type::Complete(_) => Ok(()),
            Type::InComplete(i) => {
                if seen.contains(&i.sym) {
                    return Err(Error::new_invalid_recursion(
                        seen.iter().cloned().collect(),
                        pos,
                    ));
                } else {
                    seen.insert(i.sym);
                    match &i.ty {
                        Some(ty) => self.detect_invalid_recursion_inner(ty, pos, seen),
                        None => match self.type_env.look(i.sym) {
                            Some(ty) => self.detect_invalid_recursion_inner(ty, pos, seen),
                            None => Err(Error::new_undefined_item(pos, Item::Type, i.sym)),
                        },
                    }
                }
            }
        }
        // Ok(())
    }

    fn is_brekable(&self) -> bool {
        self.break_count > 0
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

    fn new_break_scope<F>(&mut self, f: F) -> Result<ExprType>
    where
        F: FnOnce(&mut Self) -> Result<ExprType>,
    {
        self.break_count += 1;
        let result = f(self);
        self.break_count -= 1;
        result
    }

    fn trans_decl(&mut self, decl: Decl) -> Result<()> {
        match decl {
            Decl::Type(type_decls) => {
                let mut seen = HashSet::new();
                for decl in type_decls.iter() {
                    let sym = Symbol::from(&decl.id);
                    let ty = Type::InComplete(IncompleteType { sym, ty: None });
                    self.type_env.enter(sym, ty);
                    if !seen.insert(sym) {
                        return Err(Error::new_same_name(Item::Type, sym, decl.pos));
                    }
                }

                let iter: Vec<_> = type_decls
                    .iter()
                    .map(|v| (Symbol::from(&v.id), v.pos))
                    .collect();

                for decl in type_decls.into_iter() {
                    let id = decl.id;
                    let sym = Symbol::from(id);
                    let _type = self.trans_type(decl.ty)?;
                    self.type_env
                        .replace(sym, _type)
                        .expect("binding not found");
                }

                self.detect_invalid_recursion(&iter)?;

                Ok(())
            }
            Decl::Var(VarDecl(id, _, ty, expr, pos)) => {
                let sym = Symbol::from(id);
                let ExprType { ty: expr_ty, .. } = self.trans_expr(expr)?;

                if let Some(type_id) = ty {
                    let ty_symbol = Symbol::from(type_id);
                    match self.type_env.look(ty_symbol) {
                        Some(expected_ty) => {
                            let expected_ty = self.actual_ty(expected_ty, pos)?.clone();

                            if !expected_ty.assignable(&expr_ty) {
                                return Err(Error::new_unexpected_type(
                                    pos,
                                    vec![expected_ty],
                                    expr_ty,
                                ));
                            }
                        }
                        None => return Err(Error::new_undefined_item(pos, Item::Type, ty_symbol)),
                    }
                } else {
                    // initializing nil expressions not constrained by record type is invalid
                    // see test45.tig
                    if expr_ty == CompleteType::Nil {
                        return Err(Error::new_unconstrained_nil_initialize(pos));
                    }

                    // initializing with unit is invalid
                    // see test43.tig
                    if expr_ty == CompleteType::Unit {
                        return Err(Error::new_unit_initialize(pos));
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
        let mut header = vec![];
        let mut seen = HashSet::new();
        for func_decl in func_decls.iter() {
            // enter function defs
            let func_name = Symbol::from(&func_decl.name);
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

            let ret_type = match &func_decl.ret_type {
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
            header.push((formals, ret_type));
            if !seen.insert(func_name) {
                return Err(Error::new_same_name(Item::Func, func_name, func_decl.pos));
            }
        }

        assert_eq!(header.len(), func_decls.len());

        for (func_decl, (formals, ret_type)) in func_decls.into_iter().zip(header.into_iter()) {
            // function body
            let ret_type = self.actual_ty(&ret_type, func_decl.pos)?.clone();
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

            AstExpr::Nil(_) => Ok(ExprType {
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
                            .map(|v| self.actual_ty(v, pos))
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
                                let result = self.actual_ty(result, pos)?;
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
                if lhs_ty.assignable(&rhs_ty) {
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
                        let ty = self.actual_ty(ty, pos)?;

                        match ty {
                            CompleteType::Record {
                                fields: expected_fields,
                                ..
                            } => {
                                let mut expected_map = expected_fields
                                    .iter()
                                    .map(|f| {
                                        let ty = self.actual_ty(&f.1, pos)?;
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

                                            if !expected_field_type.assignable(&actual_field_type) {
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
                        let ty = self.actual_ty(ty, pos)?;

                        match ty {
                            CompleteType::Array { ty, unique } => {
                                let unique = *unique;
                                let ty = ty.clone();

                                let arr_ty = self.actual_ty(&ty, pos)?.clone();

                                let size_pos = size.pos();
                                self.check_type(*size, (CompleteType::Int,), size_pos)?;

                                let init_pos = init.pos();
                                self.check_type(*init, (arr_ty,), init_pos)?;

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

            AstExpr::If {
                cond,
                then,
                els,
                pos: _,
            } => {
                let cond_pos = cond.pos();
                self.check_type(*cond, (CompleteType::Int,), cond_pos)?;

                let then_pos = then.pos();
                let ExprType { ty: then_ty, .. } = self.trans_expr(*then)?;
                match els {
                    Some(els) => {
                        let els_pos = els.pos();
                        let ExprType { ty: else_ty, .. } = self.trans_expr(*els)?;

                        if !then_ty.assignable(&else_ty) {
                            Err(Error::new_unexpected_type(els_pos, vec![then_ty], else_ty))
                        } else {
                            Ok(ExprType {
                                expr: TransExpr::new(),
                                ty: then_ty,
                            })
                        }
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
                                vec![CompleteType::Unit],
                                then_ty,
                            ))
                        }
                    }
                }
            }

            AstExpr::While(cond, then, _) => self.new_break_scope(|_self| {
                let cond_pos = cond.pos();
                _self.check_type(*cond, (CompleteType::Int,), cond_pos)?;
                let then_pos = then.pos();
                _self.check_type(*then, (CompleteType::Unit,), then_pos)?;

                Ok(ExprType {
                    expr: TransExpr::new(),
                    ty: CompleteType::Unit,
                })
            }),

            AstExpr::For {
                id,
                is_escape: _,
                from: expr1,
                to: expr2,
                then,
                pos: _,
            } => self.new_scope(|_self| {
                _self.new_break_scope(|_self| {
                    let sym = Symbol::from(id);
                    _self.var_env.enter(
                        sym,
                        EnvEntry::Var {
                            ty: Type::Complete(CompleteType::Int),
                        },
                    );

                    let expr1_pos = expr1.pos();
                    _self.check_type(*expr1, (CompleteType::Int,), expr1_pos)?;

                    let expr2_pos = expr2.pos();
                    _self.check_type(*expr2, (CompleteType::Int,), expr2_pos)?;

                    let then_pos = then.pos();
                    _self.check_type(*then, (CompleteType::Unit,), then_pos)?;

                    Ok(ExprType {
                        expr: TransExpr::new(),
                        ty: CompleteType::Unit,
                    })
                })
            }),

            AstExpr::Break(pos) => {
                if self.is_brekable() {
                    Ok(ExprType {
                        expr: TransExpr::new(),
                        ty: CompleteType::Unit,
                    })
                } else {
                    Err(Error::new_invalid_break(pos))
                }
            }

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
                        let ty = self.actual_ty(ty, pos)?;
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
                                let ty = self.actual_ty(ty, pos)?;
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
                        let ty = self.actual_ty(&ty, pos)?;
                        Ok(ExprType {
                            expr: TransExpr::new(),
                            ty: ty.clone(),
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

impl Env<EnvEntry> {
    pub fn with_base_var(mut self) -> Self {
        use self::CompleteType::*;
        let base_func = vec![
            ("print", vec![String], Unit),
            ("flush", vec![], Unit),
            ("getchar", vec![], String),
            ("ord", vec![String], Int),
            ("chr", vec![Int], String),
            ("size", vec![String], Int),
            ("substring", vec![String, Int, Int], String),
            ("concat", vec![String, String], String),
            ("not", vec![Int], Int),
            ("exit", vec![Int], Unit),
        ];

        for (sym, formals, ret_type) in base_func {
            let sym = Symbol::from(sym);
            let formals = formals.into_iter().map(Type::Complete).collect();
            let result = Type::Complete(ret_type);
            self.enter(sym, EnvEntry::Func { formals, result });
        }

        self.begin_scope();
        self
    }
}

#[derive(Debug)]
pub struct ExprType {
    #[allow(dead_code)]
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

    macro_rules! test_file_inner {
        ($name:ident, $path:expr) => {
            use crate::parser::{self, ast::Program};
            use std::fs::File;
            let file = File::open($path).unwrap();
            let ast = parser::parse($path, file).unwrap();

            let mut semantic_analyzer = Semant::new_with_base();

            match ast {
                Program::Expr(e) => match semantic_analyzer.trans_prog(e) {
                    Ok(t) => println!("success! {:?}", t),
                    Err(e) => panic!("fail! {}", e),
                },
                Program::Decls(_) => panic!(),
            }
        };
    }

    macro_rules! test_file {
        ($name:ident, $path:expr, success) => {
            #[test]
            fn $name() {
                test_file_inner!($name, $path);
            }
        };
        // invalid syntax
        ($name:ident, $path:expr, invalid) => {
            #[test]
            #[should_panic]
            fn $name() {
                test_file_inner!($name, $path);
            }
        };
        // valid but currently cannot compile
        ($name:ident, $path:expr, fail) => {
            #[test]
            #[should_panic]
            fn $name() {
                test_file_inner!($name, $path);
            }
        };
    }

    test_file!(test_merge, "./testcases/merge.tig", success);
    test_file!(test_queens, "./testcases/queens.tig", success);
    test_file!(test_test1, "./testcases/test1.tig", success);
    test_file!(test_test2, "./testcases/test2.tig", success);
    test_file!(test_test3, "./testcases/test3.tig", success);
    test_file!(test_test4, "./testcases/test4.tig", success);
    test_file!(test_test5, "./testcases/test5.tig", success);
    test_file!(test_test6, "./testcases/test6.tig", success);
    test_file!(test_test7, "./testcases/test7.tig", success);
    test_file!(test_test8, "./testcases/test8.tig", success);
    test_file!(test_test9, "./testcases/test9.tig", invalid);
    test_file!(test_test10, "./testcases/test10.tig", invalid);
    test_file!(test_test11, "./testcases/test11.tig", invalid);
    test_file!(test_test12, "./testcases/test12.tig", success);
    test_file!(test_test13, "./testcases/test13.tig", invalid);
    test_file!(test_test14, "./testcases/test14.tig", invalid);
    test_file!(test_test15, "./testcases/test15.tig", invalid);
    test_file!(test_test16, "./testcases/test16.tig", invalid);
    test_file!(test_test17, "./testcases/test17.tig", invalid);
    test_file!(test_test18, "./testcases/test18.tig", invalid);
    test_file!(test_test19, "./testcases/test19.tig", invalid);
    test_file!(test_test20, "./testcases/test20.tig", invalid);
    test_file!(test_test21, "./testcases/test21.tig", invalid);
    test_file!(test_test22, "./testcases/test22.tig", invalid);
    test_file!(test_test23, "./testcases/test23.tig", invalid);
    test_file!(test_test24, "./testcases/test24.tig", invalid);
    test_file!(test_test25, "./testcases/test25.tig", invalid);
    test_file!(test_test26, "./testcases/test26.tig", invalid);
    test_file!(test_test27, "./testcases/test27.tig", success);
    test_file!(test_test28, "./testcases/test28.tig", invalid);
    test_file!(test_test29, "./testcases/test29.tig", invalid);
    test_file!(test_test30, "./testcases/test30.tig", success);
    test_file!(test_test31, "./testcases/test31.tig", invalid);
    test_file!(test_test32, "./testcases/test32.tig", invalid);
    test_file!(test_test33, "./testcases/test33.tig", invalid);
    test_file!(test_test34, "./testcases/test34.tig", invalid);
    test_file!(test_test35, "./testcases/test35.tig", invalid);
    test_file!(test_test36, "./testcases/test36.tig", invalid);
    test_file!(test_test37, "./testcases/test37.tig", success);
    test_file!(test_test38, "./testcases/test38.tig", invalid);
    test_file!(test_test39, "./testcases/test39.tig", invalid);
    test_file!(test_test40, "./testcases/test40.tig", invalid);
    test_file!(test_test41, "./testcases/test41.tig", success);
    test_file!(test_test42, "./testcases/test42.tig", success);
    test_file!(test_test43, "./testcases/test43.tig", invalid);
    test_file!(test_test44, "./testcases/test44.tig", success);
    test_file!(test_test45, "./testcases/test45.tig", invalid);
    test_file!(test_test46, "./testcases/test46.tig", success);
    test_file!(test_test47, "./testcases/test47.tig", success);
    test_file!(test_test48, "./testcases/test48.tig", success);
    test_file!(test_test49, "./testcases/test49.tig", invalid);
    test_file!(test_test60, "./testcases/test60.tig", invalid);
    test_file!(test_test61, "./testcases/test61.tig", success);
    test_file!(test_test62, "./testcases/test62.tig", success);
    test_file!(test_test63, "./testcases/test63.tig", success);
    test_file!(test_test64, "./testcases/test64.tig", success);
    test_file!(test_test65, "./testcases/test65.tig", invalid);
}
