mod builtin;
mod ctx;
mod env;
mod escape;
pub(crate) mod hir;
pub mod translate;
mod types;

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

use crate::{
    common::{Label, Positions, Symbol},
    parser::ast::{
        Decl as AstDecl, Expr as AstExpr, Ident, LValue as AstLValue, Operator, Program,
        RecordField, Type as AstType, TypeField as AstTypeField, VarDecl as AstVarDecl,
    },
    semant::{ctx::FnEntry, hir::FuncDecl},
};
use {
    env::Env,
    escape::EscapeFinder,
    hir::{Expr as HirExpr, ExprKind, Program as HirProgram},
    types::{IncompleteTypeError, Type, Unique},
};

use thiserror::Error;

use self::{
    builtin::Builtin,
    ctx::{FnId, TyCtx, VarEntry, VarId},
    hir::{Decl, LValue, Param, TypeDecl, VarDecl},
    types::TypeId,
};

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error, PartialEq, Eq)]
pub struct Error {
    pos: Positions,
    kind: ErrorKind,
}

impl Error {
    fn new_unexpected_type(pos: Positions, expected: Vec<Type>, actual: Type) -> Self {
        Self {
            pos,
            kind: ErrorKind::UnExpectedType(expected, actual),
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

    fn new_missing_field(pos: Positions, ty: Type, field: Symbol) -> Self {
        Self {
            pos,
            kind: ErrorKind::MissingFieldName(ty, field),
        }
    }

    fn new_missing_field_on_record_creation(pos: Positions, ty: Type, field: Symbol) -> Self {
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
    UnExpectedType(Vec<Type>, Type),
    #[error("incomplete type: {}", (.0).0.name())]
    InCompleteType(#[from] IncompleteTypeError),
    #[error("unexpected func item: {}, expected var item", .0.name())]
    UnexpectedFunc(Symbol),
    #[error("unexpected var item: {}, expected func item", .0.name())]
    UnexpectedVar(Symbol),
    #[error("undefined {0}: {}",.1.name())]
    UndefinedItem(Item, Symbol),
    #[error("no field `{:?}` on type `{}`",.0, .1.name())]
    MissingFieldName(Type, Symbol),
    #[error("field `{}` on `{:?}` is not initialized",.1.name(),.0)]
    MissingFieldOnRecordCreation(Type, Symbol),
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
    type_env: Env<TypeId>,  // type env
    ty_ctx: TyCtx,
    break_count: u32, // 0 means not inside break or while. greater than 1 means inside break or while
}

impl Semant {
    pub fn new(var_base: Env<EnvEntry>, type_base: Env<TypeId>, ty_ctx: TyCtx) -> Self {
        Self {
            var_env: var_base,
            type_env: type_base,
            ty_ctx,
            break_count: 0,
        }
    }

    pub fn new_with_base() -> Self {
        let ty_ctx = TyCtx::new().with_builtin();
        let type_env = Env::new().with_builtin();
        let var_env = Env::new().with_builtin();

        Self::new(var_env, type_env, ty_ctx)
    }

    /// whether rhs is assignable to lhs or not.
    fn assignable(&self, lhs: TypeId, rhs: TypeId) -> bool {
        let lhs = self.type_(lhs);
        let rhs = self.type_(rhs);
        lhs.assignable(rhs)
    }

    fn type_(&self, type_id: TypeId) -> &Type {
        self.ty_ctx.type_(type_id)
    }

    fn type_id(&self, sym: Symbol, pos: Positions) -> Result<&TypeId> {
        self.type_env
            .look(sym)
            .ok_or_else(|| Error::new_undefined_item(pos, Item::Type, sym))
    }

    pub fn trans_prog(mut self, mut prog: Program) -> Result<(HirProgram, TyCtx)> {
        EscapeFinder::find_escape(&mut prog);

        // if body returns int, then use that value as exit code.
        // otherwise use 0 as exit code.
        let prog = self.trans_prog_(prog)?;
        Ok((prog, self.ty_ctx))
    }

    fn check_type(&self, actual: TypeId, expected: &[TypeId], pos: Positions) -> Result<()> {
        if expected.iter().any(|e| e == &actual) {
            Ok(())
        } else {
            Err(Error::new_unexpected_type(
                pos,
                expected.iter().map(|v| self.type_(*v)).cloned().collect(),
                self.type_(actual).clone(),
            ))
        }
    }

    fn is_brekable(&self) -> bool {
        self.break_count > 0
    }

    fn new_scope<G, R>(&mut self, g: G) -> Result<R>
    where
        G: FnOnce(&mut Self) -> Result<R>,
    {
        self.type_env.begin_scope();
        self.var_env.begin_scope();
        let result = g(self);
        self.type_env.end_scope();
        self.var_env.end_scope();
        result
    }

    fn new_break_scope<G>(&mut self, f: G) -> Result<HirExpr>
    where
        G: FnOnce(&mut Self) -> Result<HirExpr>,
    {
        self.break_count += 1;
        let result = f(self);
        self.break_count -= 1;
        result
    }

    fn new_var(&mut self, e: VarEntry) -> VarId {
        let sym = e.sym();
        let id = self.ty_ctx.new_var(e);
        self.var_env.enter(sym, EnvEntry::new_var(id));
        id
    }

    fn new_fn(&mut self, e: FnEntry) -> FnId {
        let sym = e.sym();
        let id = self.ty_ctx.new_fn(e);
        self.var_env.enter(sym, EnvEntry::new_func(id));
        id
    }

    fn trans_decl(&mut self, decl: AstDecl) -> Result<Decl> {
        match decl {
            AstDecl::Type(type_decls) => {
                let mut seen = HashSet::new();

                for decl in type_decls.iter() {
                    let sym = Symbol::from(&decl.id);
                    if !seen.insert(sym) {
                        return Err(Error::new_same_name(Item::Type, sym, decl.pos));
                    }

                    // Reserve new type id.
                    // Type id is only reserved if type is array or record.
                    // Otherwise it means type alias, and not create new type.
                    if matches!(decl.ty, AstType::Array(_, _) | AstType::Fields(_, _)) {
                        let type_id = self.ty_ctx.reserve_type();
                        self.type_env.enter(sym, type_id);
                    }
                }

                // Resolve type alias,
                let mut aliases = type_decls
                    .iter()
                    .filter(|v| matches!(v.ty, AstType::Id(_, _)))
                    .map(|v| {
                        let AstType::Id(ref type_ident,_)= v.ty else{unreachable!()};
                        (&v.id, type_ident, v.pos)
                    })
                    .collect::<Vec<_>>();

                loop {
                    let leaf = aliases.iter().position(|v| {
                        let sym = Symbol::from(v.1);
                        self.type_env.look(sym).is_some()
                    });

                    if let Some(idx) = leaf {
                        let (lhs, rhs, pos) = aliases.swap_remove(idx);
                        let lhs = Symbol::from(lhs);
                        let rhs = Symbol::from(rhs);
                        let type_id = self.type_id(rhs, pos)?;
                        self.type_env.enter(lhs, *type_id);
                    } else {
                        break;
                    }
                }

                if !aliases.is_empty() {
                    let syms = aliases.iter().map(|v| Symbol::from(v.0)).collect();
                    return Err(Error::new_invalid_recursion(syms, aliases[0].2));
                }

                // Resolve record and array.
                for decl in type_decls.iter() {
                    let id = &decl.id;
                    let sym = Symbol::from(id);
                    let &type_id = self.type_id(sym, decl.pos)?;

                    match &decl.ty {
                        AstType::Fields(fields, _) => {
                            let fields = fields
                                .iter()
                                .map(|f| {
                                    let name = Symbol::from(&f.id);
                                    let _type = Symbol::from(&f.type_id);
                                    let _type = self.type_id(_type, f.pos).copied();
                                    _type.map(|v| (name, v))
                                })
                                .collect::<Result<Vec<_>>>()?;

                            let ty = Type::Record {
                                fields,
                                unique: Unique::new(),
                            };

                            self.ty_ctx.set_reserved_type(type_id, ty);
                        }

                        AstType::Array(type_ident, pos) => {
                            let sym = Symbol::from(type_ident);
                            let &elem_ty = self.type_id(sym, *pos)?;

                            let ty = Type::Array {
                                ty: elem_ty,
                                unique: Unique::new(),
                            };

                            self.ty_ctx.set_reserved_type(type_id, ty);
                        }
                        AstType::Id(_, _) => (),
                    }
                }

                let mut converted = Vec::new();
                for decl in type_decls {
                    let sym = Symbol::from(&decl.id);
                    let &type_id = self.type_id(sym, decl.pos)?;
                    converted.push(TypeDecl {
                        id: decl.id.into(),
                        type_id,
                        pos: decl.pos,
                    })
                }

                Ok(Decl::Type(converted))
            }
            AstDecl::Var(AstVarDecl(id, is_escape, ty, expr, pos)) => {
                let sym = Symbol::from(id.clone());
                let expr = self.trans_expr(expr)?;

                if let Some(ref type_id) = ty {
                    let ty_symbol = Symbol::from(type_id);
                    match self.type_env.look(ty_symbol) {
                        Some(expected_ty) => {
                            let expected_ty = self.type_(*expected_ty);
                            let actual_ty = self.type_(expr.ty);

                            if !expected_ty.assignable(actual_ty) {
                                return Err(Error::new_unexpected_type(
                                    pos,
                                    vec![expected_ty.clone()],
                                    actual_ty.clone(),
                                ));
                            }
                        }
                        None => return Err(Error::new_undefined_item(pos, Item::Type, ty_symbol)),
                    }
                } else {
                    // initializing nil expressions not constrained by record type is invalid
                    // see test45.tig
                    if expr.ty == TypeId::nil() {
                        return Err(Error::new_unconstrained_nil_initialize(pos));
                    }

                    // initializing with unit is invalid
                    // see test43.tig
                    if expr.ty == TypeId::unit() {
                        return Err(Error::new_unit_initialize(pos));
                    }
                }

                let e = VarEntry::new(expr.ty, sym);
                let var_id = self.new_var(e);

                Ok(Decl::Var(VarDecl(
                    id,
                    var_id,
                    is_escape,
                    ty.map(Into::into),
                    expr.ty,
                    expr,
                    pos,
                )))
            }
            AstDecl::Func(fn_decls) => {
                let mut header = vec![];
                let mut seen = HashSet::new();
                for func_decl in fn_decls.iter() {
                    // enter function defs
                    let func_name = Symbol::from(&func_decl.name);
                    let formals = func_decl
                        .params
                        .iter()
                        .map(|f| {
                            let ty_sym = Symbol::from(&f.type_id);
                            self.type_id(ty_sym, f.pos).copied()
                        })
                        .collect::<Result<Vec<_>>>()?;

                    let ret_type = match &func_decl.ret_type {
                        Some(type_id) => {
                            let ty_sym = Symbol::from(type_id);
                            self.type_id(ty_sym, func_decl.pos).copied()?
                        }
                        None => TypeId::unit(),
                    };

                    let label = Label::new_fn(func_name.name().as_ref());
                    let e = FnEntry::new(func_name, label, formals.clone(), ret_type);
                    let fn_id = self.new_fn(e);
                    header.push((formals, ret_type, fn_id));
                    if !seen.insert(func_name) {
                        return Err(Error::new_same_name(Item::Func, func_name, func_decl.pos));
                    }
                }

                assert_eq!(header.len(), fn_decls.len());

                let mut converted = Vec::new();
                for (func_decl, (formals, ret_type, fn_id)) in
                    fn_decls.into_iter().zip(header.into_iter())
                {
                    // function body

                    let (body, params) = self.new_scope(|_self| {
                        let mut params = Vec::new();
                        for (&ty, f) in formals.iter().zip(func_decl.params.iter()) {
                            let e = VarEntry::new(ty, Symbol::from(&f.id));
                            let id = _self.new_var(e);
                            params.push(id);
                        }

                        let ty = _self.trans_expr(func_decl.body)?;

                        _self.check_type(ty.ty, &[ret_type], func_decl.pos)?;
                        Ok((ty, params))
                    })?;

                    let mut formals = Vec::new();
                    for (param, var_id) in func_decl.params.into_iter().zip(params.iter()) {
                        formals.push(self.trans_param(param, *var_id)?);
                    }
                    converted.push(FuncDecl {
                        name: func_decl.name,
                        fn_id,
                        params: formals,
                        ret_type: func_decl.ret_type.map(Into::into),
                        re_type_id: ret_type,
                        body,
                        pos: func_decl.pos,
                    })
                }

                Ok(Decl::Func(converted))
            }
        }
    }

    fn trans_prog_(&mut self, prog: Program) -> Result<HirProgram> {
        self.trans_expr(prog)
    }

    fn trans_expr(&mut self, expr: AstExpr) -> Result<HirExpr> {
        match expr {
            AstExpr::LValue(lvalue, pos) => {
                let (lvalue, ty) = self.trans_lvalue(lvalue)?;
                Ok(HirExpr {
                    kind: ExprKind::LValue(lvalue, pos),
                    ty,
                })
            }

            AstExpr::Nil(pos) => Ok(HirExpr {
                kind: ExprKind::Nil(pos),
                ty: TypeId::nil(),
            }),

            // exprs.len() may be 0.
            AstExpr::Sequence(exprs, pos) => {
                // TODO: refactor
                let mut transed = exprs
                    .into_iter()
                    .map(|e| self.trans_expr(e))
                    .collect::<Result<Vec<_>>>()?;

                let expr_type = match transed.len() {
                    0 => HirExpr {
                        kind: ExprKind::Int(0, pos),
                        ty: TypeId::unit(),
                    },
                    1 => transed.pop().unwrap(),
                    _ => {
                        let ty = transed.last().unwrap().ty;
                        let pos = transed.last().unwrap().pos();
                        HirExpr {
                            kind: ExprKind::Sequence(transed, pos),
                            ty,
                        }
                    }
                };

                Ok(expr_type)
            }

            AstExpr::Int(num, pos) => Ok(HirExpr {
                kind: ExprKind::Int(num, pos),
                ty: TypeId::int(),
            }),

            AstExpr::Str(lit, pos) => Ok(HirExpr {
                kind: ExprKind::Str(lit, pos),
                ty: TypeId::string(),
            }),

            AstExpr::FuncCall(ident, args, pos) => {
                let sym = Symbol::from(ident.clone());
                let args_len = args.len();

                // proceed trans_expr first for ownership reason.
                let arg_iter = args
                    .into_iter()
                    .map(|e| self.trans_expr(e))
                    .collect::<Result<Vec<_>>>()?;

                match self.var_env.look(sym) {
                    Some(EnvEntry::Func { id }) => {
                        let e = self.ty_ctx.fn_(*id);
                        if args_len != e.formals().len() {
                            return Err(Error::new_mismatch_arg_count(
                                pos,
                                e.formals().len() as u8,
                                args_len as u8,
                                sym,
                            ));
                        };

                        let unmatched = arg_iter
                            .iter()
                            .zip(e.formals().iter())
                            .enumerate()
                            .find(|(_, (HirExpr { ty: lhs, .. }, rhs))| lhs != *rhs);

                        match unmatched {
                            Some((_, (HirExpr { ty: actual, .. }, &expected))) => {
                                Err(Error::new_unexpected_type(
                                    pos,
                                    vec![self.type_(expected).clone()],
                                    self.type_(*actual).clone(),
                                ))
                            }
                            None => Ok(HirExpr {
                                kind: ExprKind::FuncCall(ident, *id, arg_iter, pos),
                                ty: e.result(),
                            }),
                        }
                    }

                    Some(_) => Err(Error::new_unexpected_variable(pos, sym)),
                    None => Err(Error::new_undefined_item(pos, Item::Func, sym)),
                }
            }

            // int only
            AstExpr::Op(
                op @ (Operator::Plus
                | Operator::Minus
                | Operator::Mul
                | Operator::Div
                | Operator::And
                | Operator::Or),
                lhs,
                rhs,
                pos,
            ) => {
                let lhs = self.trans_expr(*lhs)?;
                let rhs = self.trans_expr(*rhs)?;
                self.check_type(lhs.ty, &[TypeId::int()], pos)?;
                self.check_type(rhs.ty, &[TypeId::int()], pos)?;
                Ok(HirExpr {
                    kind: ExprKind::Op(op.into(), Box::new(lhs), Box::new(rhs), pos),
                    ty: TypeId::int(),
                })
            }

            // int and string
            AstExpr::Op(
                op @ (Operator::Le | Operator::Lt | Operator::Ge | Operator::Gt),
                lhs,
                rhs,
                pos,
            ) => {
                let lhs = self.trans_expr(*lhs)?;
                let rhs = self.trans_expr(*rhs)?;

                match (self.type_(lhs.ty), self.type_(rhs.ty)) {
                    (Type::Int, Type::Int) | (Type::String, Type::String) => Ok(HirExpr {
                        kind: ExprKind::Op(op.into(), Box::new(lhs), Box::new(rhs), pos),
                        ty: TypeId::int(),
                    }),
                    other => Err(Error::new_unexpected_type(
                        pos,
                        vec![other.0.clone()],
                        other.1.clone(),
                    )),
                }
            }

            // compare two type
            AstExpr::Op(op @ (Operator::Eq | Operator::Neq), lhs, rhs, pos) => {
                let lhs = self.trans_expr(*lhs)?;
                let rhs = self.trans_expr(*rhs)?;
                if self.assignable(lhs.ty, rhs.ty) {
                    Ok(HirExpr {
                        kind: ExprKind::Op(op.into(), Box::new(lhs), Box::new(rhs), pos),
                        ty: TypeId::int(),
                    })
                } else {
                    Err(Error::new_unexpected_type(
                        pos,
                        vec![self.type_(lhs.ty).clone()],
                        self.type_(rhs.ty).clone(),
                    ))
                }
            }

            AstExpr::Neg(e, pos) => {
                let expr = self.trans_expr(*e)?;
                self.check_type(expr.ty, &[TypeId::int()], pos)?;

                let ty = expr.ty;
                Ok(HirExpr {
                    kind: ExprKind::Neg(Box::new(expr), pos),
                    ty,
                })
            }

            AstExpr::RecordCreation(type_ident, actual_fields, pos) => {
                let sym = Symbol::from(&type_ident);
                match self.type_env.look(sym) {
                    Some(&type_id) => {
                        let ty = self.type_(type_id);

                        match ty {
                            Type::Record {
                                fields: expected_fields,
                                ..
                            } => {
                                let mut expected_map =
                                    expected_fields.iter().copied().collect::<HashMap<_, _>>();

                                let ty = ty.clone();
                                let mut exprs = Vec::new();
                                for RecordField { id, expr, pos } in actual_fields {
                                    let actual_sym = Symbol::from(id.clone());
                                    match expected_map.remove(&actual_sym) {
                                        None => {
                                            return Err(Error::new_missing_field(
                                                pos, ty, actual_sym,
                                            ))
                                        }

                                        Some(expected_field_type) => {
                                            let expr = self.trans_expr(expr)?;

                                            if !self.assignable(expected_field_type, expr.ty) {
                                                return Err(Error::new_unexpected_type(
                                                    pos,
                                                    vec![self.type_(expected_field_type).clone()],
                                                    self.type_(expr.ty).clone(),
                                                ));
                                            }
                                            exprs.push(hir::RecordField {
                                                ident: id,
                                                expr,
                                                field_type: expected_field_type,
                                                pos,
                                            });
                                        }
                                    }
                                }

                                if expected_map.is_empty() {
                                    Ok(HirExpr {
                                        kind: ExprKind::RecordCreation(
                                            type_ident.into(),
                                            exprs,
                                            pos,
                                        ),
                                        ty: type_id,
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
                                vec![Type::dummy_record()],
                                other.clone(),
                            )),
                        }
                    }
                    None => Err(Error::new_undefined_item(pos, Item::Type, sym)),
                }
            }

            AstExpr::ArrayCreation {
                type_id: type_ident,
                size,
                init,
                pos,
            } => {
                let sym = Symbol::from(&type_ident);
                match self.type_env.look(sym) {
                    Some(&type_id) => {
                        let ty = self.type_(type_id).clone();

                        match ty {
                            Type::Array { ty: elem_ty, .. } => {
                                let size_pos = size.pos();
                                let size = self.trans_expr(*size)?;
                                self.check_type(size.ty, &[TypeId::int()], size_pos)?;

                                let init_pos = init.pos();
                                let init = self.trans_expr(*init)?;
                                self.check_type(init.ty, &[elem_ty], init_pos)?;

                                Ok(HirExpr {
                                    kind: ExprKind::ArrayCreation {
                                        type_ident: type_ident.into(),
                                        size: Box::new(size),
                                        init: Box::new(init),
                                        pos,
                                    },
                                    ty: type_id,
                                })
                            }
                            other => Err(Error::new_unexpected_type(
                                pos,
                                vec![Type::dummy_record()],
                                other,
                            )),
                        }
                    }
                    None => Err(Error::new_undefined_item(pos, Item::Type, sym)),
                }
            }

            AstExpr::Assign(lvalue, expr, pos) => {
                let (lvalue, type_id) = self.trans_lvalue(lvalue)?;

                let rhs = self.trans_expr(*expr)?;
                self.check_type(rhs.ty, &[type_id], pos)?;
                Ok(HirExpr {
                    kind: ExprKind::Assign(lvalue, Box::new(rhs), pos),
                    ty: TypeId::unit(),
                })
            }

            AstExpr::If {
                cond,
                then,
                els,
                pos,
            } => {
                let cond_pos = cond.pos();
                let cond = self.trans_expr(*cond)?;
                self.check_type(cond.ty, &[TypeId::int()], cond_pos)?;

                let then = self.trans_expr(*then)?;
                match els {
                    Some(els) => {
                        let els_pos = els.pos();
                        let els = self.trans_expr(*els)?;

                        if !self.assignable(then.ty, els.ty) {
                            Err(Error::new_unexpected_type(
                                els_pos,
                                vec![self.type_(then.ty).clone()],
                                self.type_(els.ty).clone(),
                            ))
                        } else {
                            Ok(HirExpr {
                                ty: then.ty,
                                kind: ExprKind::If {
                                    cond: Box::new(cond),
                                    then: Box::new(then),
                                    els: Some(Box::new(els)),
                                    pos,
                                },
                            })
                        }
                    }
                    None => {
                        self.check_type(then.ty, &[TypeId::unit()], pos)?;
                        Ok(HirExpr {
                            kind: ExprKind::If {
                                cond: Box::new(cond),
                                then: Box::new(then),
                                els: None,
                                pos,
                            },
                            ty: TypeId::unit(),
                        })
                    }
                }
            }

            AstExpr::While(cond, then, pos) => self.new_break_scope(|_self| {
                let cond_pos = cond.pos();
                let cond = _self.trans_expr(*cond)?;
                _self.check_type(cond.ty, &[TypeId::int()], cond_pos)?;

                let then_pos = then.pos();
                let then = _self.trans_expr(*then)?;
                _self.check_type(then.ty, &[TypeId::unit()], then_pos)?;

                Ok(HirExpr {
                    kind: ExprKind::While(Box::new(cond), Box::new(then), pos),
                    ty: TypeId::unit(),
                })
            }),

            AstExpr::For {
                id,
                is_escape,
                from,
                to,
                then,
                pos,
            } => {
                // convert
                //
                //   for id := `from` to `to` do then
                //
                // into
                //
                //   let
                //     id := from
                //     __id__limit := to
                //   in
                //     while id < __id__limit do (
                //       then;
                //       id := id + 1;
                //     )
                //   end
                let limit_id = Ident::new(format!("__{}__limit", id));
                let new_ast = AstExpr::Let(
                    vec![
                        AstDecl::Var(AstVarDecl(id.clone(), is_escape, None, *from, pos)),
                        AstDecl::Var(AstVarDecl(limit_id.clone(), false, None, *to, pos)),
                    ],
                    vec![AstExpr::While(
                        Box::new(AstExpr::Op(
                            Operator::Le,
                            Box::new(AstExpr::LValue(AstLValue::Var(id.clone(), pos), pos)),
                            Box::new(AstExpr::LValue(AstLValue::Var(limit_id, pos), pos)),
                            pos,
                        )),
                        Box::new(AstExpr::Sequence(
                            vec![
                                *then,
                                AstExpr::Assign(
                                    AstLValue::Var(id.clone(), pos),
                                    Box::new(AstExpr::Op(
                                        Operator::Plus,
                                        Box::new(AstExpr::LValue(AstLValue::Var(id, pos), pos)),
                                        Box::new(AstExpr::Int(1, pos)),
                                        pos,
                                    )),
                                    pos,
                                ),
                            ],
                            pos,
                        )),
                        pos,
                    )],
                    pos,
                );

                self.trans_expr(new_ast)
            }

            AstExpr::Break(pos) => {
                if self.is_brekable() {
                    Ok(HirExpr {
                        kind: ExprKind::Break(pos),
                        ty: TypeId::unit(),
                    })
                } else {
                    Err(Error::new_invalid_break(pos))
                }
            }

            AstExpr::Let(decls, exprs, pos) => self.new_scope(|_self| {
                let mut converted_decls = Vec::new();
                for decl in decls {
                    let decl = _self.trans_decl(decl)?;
                    converted_decls.push(decl);
                }

                let mut converted_exprs = Vec::new();
                for expr in exprs {
                    converted_exprs.push(_self.trans_expr(expr)?);
                }
                let ty = if let Some(e) = converted_exprs.last() {
                    e.ty
                } else {
                    TypeId::unit()
                };
                Ok(HirExpr {
                    kind: ExprKind::Let(converted_decls, converted_exprs, pos),
                    ty,
                })
            }),
        }
    }

    fn trans_lvalue(&mut self, var: AstLValue) -> Result<(LValue, TypeId)> {
        match var {
            AstLValue::Var(ident, pos) => {
                let sym = Symbol::from(ident.clone());
                match self.var_env.look(sym) {
                    Some(EnvEntry::Var { id }) => {
                        let e = self.ty_ctx.var(*id);
                        let ty = e.type_id();
                        Ok((LValue::Var(ident, *id, ty, pos), ty))
                    }
                    Some(_) => Err(Error::new_unexpected_func(pos, sym)),
                    None => Err(Error::new_undefined_item(pos, Item::Var, sym)),
                }
            }
            AstLValue::RecordField(lvar, id, pos) => {
                let (lvar, ty) = self.trans_lvalue(*lvar)?;
                let sym = Symbol::from(&id);
                match self.type_(ty) {
                    Type::Record { fields, unique } => {
                        let field = fields
                            .iter()
                            .enumerate()
                            .find(|(_, (_sym, _))| _sym == &sym);
                        match field {
                            None => Err(Error::new_missing_field(
                                pos,
                                Type::Record {
                                    fields: fields.clone(),
                                    unique: *unique,
                                },
                                sym,
                            )),
                            Some((field_index, (_, field_type))) => Ok((
                                LValue::RecordField {
                                    record: Box::new(lvar),
                                    record_type: ty,
                                    field_ident: id,
                                    field_index,
                                    field_type: *field_type,
                                    pos,
                                },
                                *field_type,
                            )),
                        }
                    }
                    other => Err(Error::new_unexpected_type(
                        pos,
                        vec![Type::dummy_record()],
                        other.clone(),
                    )),
                }
            }
            AstLValue::Array(lvar, expr, pos) => {
                let index_expr = self.trans_expr(*expr)?;
                if index_expr.ty != TypeId::int() {
                    return Err(Error::new_unexpected_type(
                        pos,
                        vec![Type::Int],
                        self.type_(index_expr.ty).clone(),
                    ));
                }

                let (lvar, arr_ty) = self.trans_lvalue(*lvar)?;
                match self.type_(arr_ty) {
                    Type::Array { ty, unique: _ } => Ok((
                        LValue::Array {
                            array: Box::new(lvar),
                            array_type: arr_ty,
                            index_type: index_expr.ty,
                            index: Box::new(index_expr),
                            pos,
                        },
                        *ty,
                    )),
                    other => Err(Error::new_unexpected_type(
                        pos,
                        vec![Type::dummy_array()],
                        other.clone(),
                    )),
                }
            }
        }
    }

    fn trans_param(&self, param: AstTypeField, var_id: VarId) -> Result<Param> {
        let ty_sym = Symbol::from(&param.type_id);
        let type_id = self.type_id(ty_sym, param.pos)?;

        Ok(Param {
            ident: param.id,
            var_id,
            is_escape: param.is_escape,
            type_ident: param.type_id.into(),
            type_id: *type_id,
            pos: param.pos,
        })
    }
}

/// Variable and function entry
pub enum EnvEntry {
    Var { id: VarId },
    Func { id: FnId },
}

impl EnvEntry {
    pub fn new_var(id: VarId) -> Self {
        Self::Var { id }
    }

    pub fn new_func(id: FnId) -> Self {
        Self::Func { id }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_file_inner {
        ($name:ident, $path:expr) => {
            use crate::parser;
            use std::fs::File;
            let file = File::open($path).unwrap();
            let e = parser::parse($path, file).unwrap();

            let semantic_analyzer = Semant::new_with_base();

            match semantic_analyzer.trans_prog(e) {
                Ok(_) => println!("success!"),
                Err(e) => panic!("fail! {}", e),
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
    // test_file!(test_test60, "./testcases/test60.tig", invalid);
    // test_file!(test_test61, "./testcases/test61.tig", success);
    // test_file!(test_test62, "./testcases/test62.tig", success);
    // test_file!(test_test63, "./testcases/test63.tig", success);
    // test_file!(test_test64, "./testcases/test64.tig", success);
    // test_file!(test_test65, "./testcases/test65.tig", invalid);
    // test_file!(test_test66, "./testcases/test66.tig", success);
}
