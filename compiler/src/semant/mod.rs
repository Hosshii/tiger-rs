mod ctx;
mod env;
mod escape;
mod translate;
mod types;

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

use crate::{
    codegen::aarch64_apple_darwin::frame::ARM64,
    common::{Label, Positions, Symbol},
    frame::{Fragment, Frame},
    parser::ast::{
        Decl, Expr as AstExpr, Ident, LValue, Operator, RecordField, Type as AstType, VarDecl,
    },
};
use {
    env::Env,
    escape::EscapeFinder,
    translate::{Access, Expr as TransExpr, Level, Translator},
    types::{IncompleteTypeError, Type, Unique},
};

use thiserror::Error;

use self::{ctx::TyCtx, types::TypeId};

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

pub struct Semant<F: Frame> {
    var_env: Env<EnvEntry<F>>, // var and func env
    type_env: Env<TypeId>,     // type env
    ty_ctx: TyCtx,
    break_count: u32, // 0 means not inside break or while. greater than 1 means inside break or while
    translator: Translator<F>,
}

impl Semant<ARM64> {}

impl<F: Frame> Semant<F> {
    pub fn new(var_base: Env<EnvEntry<F>>, type_base: Env<TypeId>, ty_ctx: TyCtx) -> Self {
        Self {
            var_env: var_base,
            type_env: type_base,
            ty_ctx,
            break_count: 0,
            translator: Translator::default(),
        }
    }

    pub fn new_with_base() -> Self {
        let mut ty_ctx = TyCtx::new();
        let type_env = ty_ctx.new_ty_env();
        let var_env = Env::new().with_base_var();

        Self::new(var_env, type_env, ty_ctx)
    }

    /// whether rhs is assignable to lhs or not.
    fn assignable(&self, lhs: TypeId, rhs: TypeId) -> bool {
        let lhs = self.type_(lhs);
        let rhs = self.type_(rhs);
        lhs.assignable(rhs)
    }

    fn type_id(&self, ty: &Type) -> Option<TypeId> {
        self.ty_ctx.type_id(ty)
    }

    fn type_(&self, type_id: TypeId) -> &Type {
        self.ty_ctx.type_(type_id)
    }

    pub fn trans_prog(mut self, mut expr: AstExpr, main_name: &str) -> Result<Vec<Fragment<F>>> {
        EscapeFinder::find_escape(&mut expr);

        // if body returns int, then use that value as exit code.
        // otherwise use 0 as exit code.
        let mut main_level =
            Level::outermost_with_name(Label::with_named_fn(main_name.to_string()));
        let body = self.trans_expr(expr, &mut main_level, None)?;
        let body = if body.ty == TypeId::int() {
            body
        } else {
            let seq = vec![body.expr, translate::num(0)];
            let expr = translate::sequence(seq);
            ExprType {
                expr,
                ty: TypeId::int(),
            }
        };

        self.translator.proc_entry_exit(main_level, body.expr);

        Ok(self.translator.get_result())
    }

    fn check_type(&self, actual: TypeId, expected: &[TypeId], pos: Positions) -> Result<()> {
        if expected.iter().any(|e| e == &actual) {
            Ok(())
        } else {
            Err(Error {
                pos,
                kind: ErrorKind::UnExpectedType(
                    expected.iter().map(|v| self.type_(*v)).cloned().collect(),
                    self.type_(actual).clone(),
                ),
            })
        }
    }

    fn unify_type_id(&mut self, unify: &mut HashMap<TypeId, TypeId>) -> bool {
        let start = unify
            .keys()
            .find(|v| unify.values().all(|w| w != *v))
            .copied();
        if let Some(start) = start {
            let to = unify.remove(&start);
            true
        } else {
            false
        }
    }

    fn detect_invalid_recursion(&self, iter: &[(Symbol, Positions)]) -> Result<()> {
        if iter.is_empty() {
            return Ok(());
        }

        if self.ty_ctx.has_invalid_recursion() {
            let pos = iter[0].1;
            let syms = iter.iter().map(|(s, _)| *s).collect();
            Err(Error::new_invalid_recursion(syms, pos))
        } else {
            Ok(())
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

    fn new_break_scope<G>(&mut self, f: G) -> Result<ExprType>
    where
        G: FnOnce(&mut Self) -> Result<ExprType>,
    {
        self.break_count += 1;
        let result = f(self);
        self.break_count -= 1;
        result
    }

    fn trans_decl(
        &mut self,
        decl: Decl,
        parent_level: &mut Level<F>,
        break_label: Option<Label>,
    ) -> Result<Option<TransExpr>> {
        match decl {
            Decl::Type(mut type_decls) => {
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
                        let type_id = self.ty_ctx.reserve();
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
                        let (lhs, rhs, _) = aliases.swap_remove(idx);
                        let lhs = Symbol::from(lhs);
                        let rhs = Symbol::from(rhs);
                        let type_id = self.type_env.look(rhs).unwrap();
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
                for decl in type_decls.into_iter() {
                    let id = decl.id;
                    let sym = Symbol::from(id);
                    let type_id = self
                        .type_env
                        .look(sym)
                        .expect(format!("type not found {}", sym).as_str());

                    self.trans_type(*type_id, decl.ty)?;
                }

                Ok(None)
            }
            Decl::Var(VarDecl(id, is_escape, ty, expr, pos)) => {
                let sym = Symbol::from(id);
                let expr = self.trans_expr(expr, parent_level, break_label)?;

                if let Some(type_id) = ty {
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

                let access = translate::alloc_local(parent_level.clone(), is_escape);
                self.var_env
                    .enter(sym, EnvEntry::new_var(access.clone(), expr.ty));
                let lhs = translate::var_decl(access, expr.expr);

                Ok(Some(lhs))
            }
            Decl::Func(fn_decls) => {
                let mut header = vec![];
                let mut seen = HashSet::new();
                let mut levels = vec![];
                for func_decl in fn_decls.iter() {
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

                    let formals_is_escape: Vec<_> =
                        func_decl.params.iter().map(|p| p.is_escape).collect();

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
                        None => TypeId::unit(),
                    };

                    let label = Label::new_fn(func_name.name().as_ref());
                    let level = translate::new_level(
                        parent_level.clone(),
                        label.clone(),
                        formals_is_escape,
                    );
                    levels.push(level.clone());
                    self.var_env.enter(
                        func_name,
                        EnvEntry::new_func(level, label, formals.clone(), ret_type.clone()),
                    );
                    header.push((formals, ret_type));
                    if !seen.insert(func_name) {
                        return Err(Error::new_same_name(Item::Func, func_name, func_decl.pos));
                    }
                }

                assert_eq!(header.len(), fn_decls.len());

                for ((func_decl, (formals, ret_type)), mut level) in fn_decls
                    .into_iter()
                    .zip(header.into_iter())
                    .zip(levels.into_iter())
                {
                    // function body

                    self.new_scope(|_self| {
                        for ((ty, f), access) in formals
                            .into_iter()
                            .zip(func_decl.params.into_iter())
                            .zip(level.formals())
                        {
                            let sym = Symbol::from(f.id);
                            _self.var_env.enter(sym, EnvEntry::new_var(access, ty))
                        }

                        let ty =
                            _self.trans_expr(func_decl.body, &mut level, break_label.clone())?;

                        if ty.ty != ret_type {
                            Err(Error::new_unexpected_type(
                                func_decl.pos,
                                vec![_self.type_(ret_type).clone()],
                                _self.type_(ty.ty).clone(),
                            ))
                        } else {
                            _self.translator.proc_entry_exit(level, ty.expr);
                            Ok(())
                        }
                    })?;
                }

                Ok(None)
            }
        }
    }

    fn trans_expr(
        &mut self,
        expr: AstExpr,
        level: &mut Level<F>,
        break_label: Option<Label>,
    ) -> Result<ExprType> {
        match expr {
            AstExpr::LValue(lvalue, _) => self.trans_lvalue(lvalue, level, break_label),

            AstExpr::Nil(_) => Ok(ExprType {
                expr: translate::num(0),
                ty: TypeId::nil(),
            }),

            // exprs.len() may be 0.
            AstExpr::Sequence(exprs, _) => {
                // TODO: refactor
                let mut transed = exprs
                    .into_iter()
                    .map(|e| self.trans_expr(e, level, break_label.clone()))
                    .collect::<Result<Vec<_>>>()?;

                let expr_type = match transed.len() {
                    0 => ExprType {
                        expr: translate::num(0),
                        ty: TypeId::unit(),
                    },
                    1 => transed.pop().unwrap(),
                    _ => {
                        let ty = transed.last().unwrap().ty.clone();
                        let exprs = transed.into_iter().map(|e| e.expr).collect();
                        let expr = translate::sequence(exprs);
                        ExprType { expr, ty }
                    }
                };

                Ok(expr_type)
            }

            AstExpr::Int(num, _) => Ok(ExprType {
                expr: translate::num(num as i64),
                ty: TypeId::int(),
            }),

            AstExpr::Str(lit, _) => Ok(ExprType {
                expr: self.translator.string_literal(lit.to_string()),
                ty: TypeId::string(),
            }),

            AstExpr::FuncCall(ident, args, pos) => {
                let sym = Symbol::from(ident);
                let args_len = args.len();

                // proceed trans_expr first for ownership reason.
                let arg_iter = args
                    .into_iter()
                    .map(|e| self.trans_expr(e, level, break_label.clone()))
                    .collect::<Result<Vec<_>>>()?;

                match self.var_env.look(sym) {
                    Some(EnvEntry::Func {
                        level: fn_level,
                        label,
                        formals,
                        result,
                    }) => {
                        if args_len != formals.len() {
                            return Err(Error::new_mismatch_arg_count(
                                pos,
                                formals.len() as u8,
                                args_len as u8,
                                sym,
                            ));
                        };

                        let unmatched = arg_iter
                            .iter()
                            .zip(formals.iter())
                            .enumerate()
                            .find(|(_, (ExprType { ty: lhs, .. }, rhs))| lhs != *rhs);

                        match unmatched {
                            Some((_, (ExprType { ty: actual, .. }, &expected))) => {
                                Err(Error::new_unexpected_type(
                                    pos,
                                    vec![self.type_(expected).clone()],
                                    self.type_(*actual).clone(),
                                ))
                            }
                            None => {
                                let args = arg_iter.into_iter().map(|v| v.expr).collect();
                                let fn_exp =
                                    translate::fn_call(label.clone(), fn_level, level, args);
                                Ok(ExprType {
                                    expr: fn_exp,
                                    ty: *result,
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
                let lhs = self.trans_expr(*lhs, level, break_label.clone())?;
                let rhs = self.trans_expr(*rhs, level, break_label)?;
                self.check_type(lhs.ty, &[TypeId::int()], pos)?;
                self.check_type(rhs.ty, &[TypeId::int()], pos)?;
                Ok(ExprType {
                    expr: translate::bin_op(op.try_into().expect("convert op"), lhs.expr, rhs.expr),
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
                let lhs = self.trans_expr(*lhs, level, break_label.clone())?;
                let rhs = self.trans_expr(*rhs, level, break_label)?;

                match (self.type_(lhs.ty), self.type_(rhs.ty)) {
                    (Type::Int, Type::Int) => Ok(ExprType {
                        expr: translate::rel_op(
                            op.try_into().expect("convert op"),
                            lhs.expr,
                            rhs.expr,
                        ),
                        ty: TypeId::int(),
                    }),
                    (Type::String, Type::String) => Ok(ExprType {
                        expr: translate::string_ord::<F>(
                            op.try_into().expect("convert op"),
                            lhs.expr,
                            rhs.expr,
                        ),
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
                let lhs = self.trans_expr(*lhs, level, break_label.clone())?;
                let rhs = self.trans_expr(*rhs, level, break_label)?;
                if self.assignable(lhs.ty, rhs.ty) {
                    let expr = if lhs.ty == TypeId::string() {
                        translate::string_eq::<F>(
                            op.try_into().expect("convert op"),
                            lhs.expr,
                            rhs.expr,
                        )
                    } else {
                        translate::rel_op(op.try_into().expect("convert op"), lhs.expr, rhs.expr)
                    };
                    Ok(ExprType {
                        expr,
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

            AstExpr::Neg(e, _) => {
                let pos = e.pos();
                let expr_ty = self.trans_expr(*e, level, break_label)?;
                self.check_type(expr_ty.ty, &[TypeId::int()], pos)?;

                Ok(ExprType {
                    expr: translate::neg(expr_ty.expr),
                    ty: expr_ty.ty,
                })
            }

            AstExpr::RecordCreation(type_id, actual_fields, pos) => {
                let sym = Symbol::from(type_id);
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
                                                expr,
                                            } =
                                                self.trans_expr(expr, level, break_label.clone())?;

                                            if !self
                                                .assignable(expected_field_type, actual_field_type)
                                            {
                                                return Err(Error::new_unexpected_type(
                                                    pos,
                                                    vec![self.type_(expected_field_type).clone()],
                                                    self.type_(actual_field_type).clone(),
                                                ));
                                            }
                                            exprs.push(expr);
                                        }
                                    }
                                }

                                if expected_map.is_empty() {
                                    Ok(ExprType {
                                        expr: translate::record_creation::<F>(exprs),
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
                type_id,
                size,
                init,
                pos,
            } => {
                let sym = Symbol::from(type_id);
                match self.type_env.look(sym) {
                    Some(&type_id) => {
                        let ty = self.type_(type_id).clone();

                        match ty {
                            Type::Array { ty: elem_ty, .. } => {
                                let size_pos = size.pos();
                                let size = self.trans_expr(*size, level, break_label.clone())?;
                                self.check_type(size.ty, &[TypeId::int()], size_pos)?;

                                let init_pos = init.pos();
                                let init = self.trans_expr(*init, level, break_label)?;
                                self.check_type(init.ty, &[elem_ty], init_pos)?;

                                Ok(ExprType {
                                    expr: translate::array_creation::<F>(size.expr, init.expr),
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
                let lhs = self.trans_lvalue(lvalue, level, break_label.clone())?;
                let rhs = self.trans_expr(*expr, level, break_label)?;
                if lhs.ty == rhs.ty {
                    Ok(ExprType {
                        expr: translate::assign(lhs.expr, rhs.expr),
                        ty: TypeId::unit(),
                    })
                } else {
                    Err(Error::new_unexpected_type(
                        pos,
                        vec![self.type_(lhs.ty).clone()],
                        self.type_(rhs.ty).clone(),
                    ))
                }
            }

            AstExpr::If {
                cond,
                then,
                els,
                pos: _,
            } => {
                let cond_pos = cond.pos();
                let cond = self.trans_expr(*cond, level, break_label.clone())?;
                self.check_type(cond.ty, &[TypeId::int()], cond_pos)?;

                let then_pos = then.pos();
                let then = self.trans_expr(*then, level, break_label.clone())?;
                match els {
                    Some(els) => {
                        let els_pos = els.pos();
                        let els = self.trans_expr(*els, level, break_label)?;

                        if !self.assignable(then.ty, els.ty) {
                            Err(Error::new_unexpected_type(
                                els_pos,
                                vec![self.type_(then.ty).clone()],
                                self.type_(els.ty).clone(),
                            ))
                        } else {
                            Ok(ExprType {
                                expr: translate::if_expr(
                                    level,
                                    cond.expr,
                                    then.expr,
                                    Some(els.expr),
                                ),
                                ty: then.ty,
                            })
                        }
                    }
                    None => {
                        if then.ty == TypeId::unit() {
                            Ok(ExprType {
                                expr: translate::if_expr(level, cond.expr, then.expr, None),
                                ty: TypeId::unit(),
                            })
                        } else {
                            Err(Error::new_unexpected_type(
                                then_pos,
                                vec![Type::Unit],
                                self.type_(then.ty).clone(),
                            ))
                        }
                    }
                }
            }

            AstExpr::While(cond, then, _) => self.new_break_scope(|_self| {
                let cond_pos = cond.pos();
                let cond = _self.trans_expr(*cond, level, break_label)?;
                _self.check_type(cond.ty, &[TypeId::int()], cond_pos)?;

                let inner_break_label = Label::new();
                let then_pos = then.pos();
                let then = _self.trans_expr(*then, level, Some(inner_break_label.clone()))?;
                _self.check_type(then.ty, &[TypeId::unit()], then_pos)?;

                Ok(ExprType {
                    expr: translate::while_expr(cond.expr, then.expr, inner_break_label),
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
                        Decl::Var(VarDecl(id.clone(), is_escape, None, *from, pos)),
                        Decl::Var(VarDecl(limit_id.clone(), false, None, *to, pos)),
                    ],
                    vec![AstExpr::While(
                        Box::new(AstExpr::Op(
                            Operator::Le,
                            Box::new(AstExpr::LValue(LValue::Var(id.clone(), pos), pos)),
                            Box::new(AstExpr::LValue(LValue::Var(limit_id, pos), pos)),
                            pos,
                        )),
                        Box::new(AstExpr::Sequence(
                            vec![
                                *then,
                                AstExpr::Assign(
                                    LValue::Var(id.clone(), pos),
                                    Box::new(AstExpr::Op(
                                        Operator::Plus,
                                        Box::new(AstExpr::LValue(LValue::Var(id, pos), pos)),
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

                self.trans_expr(new_ast, level, break_label)
            }

            AstExpr::Break(pos) => {
                if self.is_brekable() {
                    Ok(ExprType {
                        expr: translate::break_expr(break_label.expect("break label")),
                        ty: TypeId::unit(),
                    })
                } else {
                    Err(Error::new_invalid_break(pos))
                }
            }

            AstExpr::Let(decls, exprs, pos) => self.new_scope(|_self| {
                let mut decls_expr = Vec::new();
                for decl in decls {
                    let decl = _self.trans_decl(decl, level, break_label.clone())?;
                    if let Some(stmt) = decl {
                        decls_expr.push(stmt);
                    }
                }
                let body = _self.trans_expr(AstExpr::Sequence(exprs, pos), level, break_label)?;
                Ok(ExprType {
                    expr: translate::let_expr(decls_expr, body.expr),
                    ty: body.ty,
                })
            }),
        }
    }

    fn trans_lvalue(
        &mut self,
        var: LValue,
        level: &mut Level<F>,
        break_label: Option<Label>,
    ) -> Result<ExprType> {
        match var {
            LValue::Var(id, pos) => {
                let sym = Symbol::from(id);
                match self.var_env.look(sym) {
                    Some(EnvEntry::Var { access, ty }) => Ok(ExprType {
                        expr: translate::simple_var(access.clone(), level),
                        ty: *ty,
                    }),
                    Some(_) => Err(Error::new_unexpected_func(pos, sym)),
                    None => Err(Error::new_undefined_item(pos, Item::Var, sym)),
                }
            }
            LValue::RecordField(lvar, id, pos) => {
                let ExprType { ty, expr } = self.trans_lvalue(*lvar, level, break_label)?;
                let sym = Symbol::from(id);
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
                                    unique: unique.clone(),
                                },
                                sym,
                            )),
                            Some((field_index, (_, ty))) => Ok(ExprType {
                                expr: translate::record_field::<F>(expr, field_index),
                                ty: *ty,
                            }),
                        }
                    }
                    other => Err(Error::new_unexpected_type(
                        pos,
                        vec![Type::dummy_record()],
                        other.clone(),
                    )),
                }
            }
            LValue::Array(lvar, expr, pos) => {
                let ExprType {
                    ty,
                    expr: subscript,
                } = self.trans_expr(*expr, level, break_label.clone())?;
                if ty != TypeId::int() {
                    return Err(Error::new_unexpected_type(
                        pos,
                        vec![Type::Int],
                        self.type_(ty).clone(),
                    ));
                }

                let ExprType { ty, expr: var } = self.trans_lvalue(*lvar, level, break_label)?;
                match self.type_(ty) {
                    Type::Array { ty, unique: _ } => Ok(ExprType {
                        expr: translate::array_subscript::<F>(var, subscript),
                        ty: *ty,
                    }),
                    other => Err(Error::new_unexpected_type(
                        pos,
                        vec![Type::dummy_array()],
                        other.clone(),
                    )),
                }
            }
        }
    }

    fn trans_type(&mut self, type_id: TypeId, ast_type: AstType) -> Result<()> {
        match ast_type {
            AstType::Id(type_ident, pos) => {
                let sym = Symbol::from(type_ident);
                if self.type_env.look(sym).is_some() {
                    Ok(())
                } else {
                    Err(Error::new_undefined_item(pos, Item::Type, sym))
                }
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

                let ty = Type::Record {
                    fields,
                    unique: Unique::new(),
                };

                self.ty_ctx.set_reserved(type_id, ty);

                Ok(())
            }

            AstType::Array(type_ident, pos) => {
                let sym = Symbol::from(type_ident);
                let elem_ty = self
                    .type_env
                    .look(sym)
                    .cloned()
                    .ok_or_else(|| Error::new_undefined_item(pos, Item::Type, sym))?;

                let ty = Type::Array {
                    ty: elem_ty,
                    unique: Unique::new(),
                };

                self.ty_ctx.set_reserved(type_id, ty);
                Ok(())
            }
        }
    }
}

/// Variable and function entry
pub enum EnvEntry<F: Frame> {
    Var {
        access: Access<F>,
        ty: TypeId,
    },
    Func {
        level: Level<F>,
        label: Label,
        formals: Vec<TypeId>,
        result: TypeId,
    },
}

impl<F: Frame> EnvEntry<F> {
    pub fn new_var(access: Access<F>, ty: TypeId) -> Self {
        Self::Var { access, ty }
    }

    pub fn new_func(level: Level<F>, label: Label, formals: Vec<TypeId>, result: TypeId) -> Self {
        Self::Func {
            level,
            label,
            formals,
            result,
        }
    }
}

impl<F: Frame> Env<EnvEntry<F>> {
    pub fn with_base_var(mut self) -> Self {
        // TODO: use external call for these functions.
        let base_func = vec![
            ("print", vec![TypeId::string()], TypeId::unit()),
            ("flush", vec![], TypeId::unit()),
            ("getchar", vec![], TypeId::string()),
            ("ord", vec![TypeId::string()], TypeId::int()),
            ("chr", vec![TypeId::int()], TypeId::string()),
            ("size", vec![TypeId::string()], TypeId::int()),
            (
                "substring",
                vec![TypeId::string(), TypeId::int(), TypeId::int()],
                TypeId::string(),
            ),
            (
                "concat",
                vec![TypeId::string(), TypeId::string()],
                TypeId::string(),
            ),
            ("not", vec![TypeId::int()], TypeId::int()),
            ("exit", vec![TypeId::int()], TypeId::unit()),
        ];

        for (sym, formals, ret_type) in base_func {
            let sym = Symbol::from(sym);
            let label = Label::with_named_fn(sym.to_string());
            self.enter(
                sym,
                EnvEntry::new_func(Level::outermost(), label, formals, ret_type),
            );
        }

        self.begin_scope();
        self
    }
}

#[derive(Debug)]
pub struct ExprType {
    #[allow(dead_code)]
    expr: TransExpr,
    ty: TypeId,
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_file_inner {
        ($name:ident, $path:expr) => {
            use crate::{
                codegen::aarch64_apple_darwin::frame::ARM64,
                parser::{self},
            };
            use std::fs::File;
            let file = File::open($path).unwrap();
            let e = parser::parse($path, file).unwrap();

            let semantic_analyzer = Semant::<ARM64>::new_with_base();

            match semantic_analyzer.trans_prog(e, "main") {
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
