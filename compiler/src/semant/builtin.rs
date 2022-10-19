use crate::common::{Label, Symbol};

use super::{
    ctx::{FnEntry, FnId, TyCtx},
    env::Env,
    types::{Type, TypeId},
    EnvEntry,
};

pub const BUILTIN_TYPES: &[(&str, TypeId, Type)] = &[
    ("unit", TypeId::unit(), Type::Unit),
    ("nil", TypeId::nil(), Type::Nil),
    ("int", TypeId::int(), Type::Int),
    ("string", TypeId::string(), Type::String),
];

pub const BUILTIN_FUNCS: &[(&str, FnId, &[TypeId], TypeId)] = &[
    ("print", FnId::print(), &[TypeId::string()], TypeId::unit()),
    ("flush", FnId::flush(), &[], TypeId::unit()),
    ("getchar", FnId::getchar(), &[], TypeId::string()),
    ("ord", FnId::ord(), &[TypeId::string()], TypeId::int()),
    ("chr", FnId::chr(), &[TypeId::int()], TypeId::string()),
    ("size", FnId::size(), &[TypeId::string()], TypeId::int()),
    (
        "substring",
        FnId::substring(),
        &[TypeId::string(), TypeId::int(), TypeId::int()],
        TypeId::string(),
    ),
    (
        "concat",
        FnId::concat(),
        &[TypeId::string(), TypeId::string()],
        TypeId::string(),
    ),
    ("not", FnId::not(), &[TypeId::int()], TypeId::int()),
    ("exit", FnId::exit(), &[TypeId::int()], TypeId::unit()),
];

impl TypeId {
    pub fn dummy() -> Self {
        Self::new(0)
    }

    pub(super) const fn unit() -> Self {
        Self::new(1)
    }

    pub(super) const fn nil() -> Self {
        Self::new(2)
    }

    pub(super) const fn int() -> Self {
        Self::new(3)
    }

    pub(super) const fn string() -> Self {
        Self::new(4)
    }
}

impl FnId {
    pub(super) const fn dummy() -> Self {
        Self::new(0)
    }

    pub(super) const fn print() -> Self {
        Self::new(1)
    }

    pub(super) const fn flush() -> Self {
        Self::new(2)
    }

    pub(super) const fn getchar() -> Self {
        Self::new(3)
    }

    pub(super) const fn ord() -> Self {
        Self::new(4)
    }

    pub(super) const fn chr() -> Self {
        Self::new(5)
    }

    pub(super) const fn size() -> Self {
        Self::new(6)
    }

    pub(super) const fn substring() -> Self {
        Self::new(7)
    }

    pub(super) const fn concat() -> Self {
        Self::new(8)
    }

    pub(super) const fn not() -> Self {
        Self::new(9)
    }

    pub(super) const fn exit() -> Self {
        Self::new(10)
    }
}

impl Env<TypeId> {
    pub fn with_builtin_type(mut self) -> Self {
        let base_types = vec![("int", TypeId::int()), ("string", TypeId::string())];

        for (sym, type_id) in base_types {
            let sym = Symbol::new(sym);
            self.enter(sym, type_id);
        }
        self.begin_scope();
        self
    }
}

impl Env<EnvEntry> {
    pub fn with_builtin_fn(mut self) -> Self {
        for (name, id, _, _) in BUILTIN_FUNCS {
            let sym = Symbol::from(*name);
            self.enter(sym, EnvEntry::new_func(*id));
        }

        self.begin_scope();
        self
    }
}

impl TyCtx {
    pub fn with_builtin(mut self) -> Self {
        for (_, type_id, type_) in BUILTIN_TYPES {
            self.insert_ty_inner(*type_id, type_.clone())
        }

        for (name, id, args, result) in BUILTIN_FUNCS {
            let entry = FnEntry::new(
                Symbol::from(*name),
                Label::with_named_fn(name.to_string()),
                args.to_vec(),
                *result,
            );
            self.insert_fn_inner(*id, entry);
        }

        self
    }
}
