use std::collections::HashMap;

use once_cell::sync::Lazy;

use crate::{
    common::{Label, Symbol},
    frame::Frame,
};

use super::{
    ctx::{FnEntry, FnId, TyCtx},
    env::Env,
    translate::{Level, Translator},
    types::{Type, TypeId},
    EnvEntry,
};

pub(super) trait Builtin: Sized {
    fn with_builtin(self) -> Self;
}

pub const UNIT: &str = "unit";
pub const NIL: &str = "nil";
pub const INT: &str = "int";
pub const STRING: &str = "string";

pub const BUILTIN_TYPES: &[(&str, TypeId, Type)] = &[
    (UNIT, TypeId::UNIT, Type::Unit),
    (NIL, TypeId::NIL, Type::Nil),
    (INT, TypeId::INT, Type::Int),
    (STRING, TypeId::STRING, Type::String),
];

pub const PRINT: &str = "print";
pub const FLUSH: &str = "flush";
pub const GETCHAR: &str = "getchar";
pub const ORD: &str = "ord";
pub const CHR: &str = "chr";
pub const SIZE: &str = "size";
pub const SUBSTRING: &str = "substring";
pub const CONCAT: &str = "concat";
pub const NOT: &str = "not";
pub const EXIT: &str = "exit";

pub const BUILTIN_FUNCS: &[(&str, FnId, &[TypeId], TypeId)] = &[
    (PRINT, FnId::PRINT, &[TypeId::STRING], TypeId::UNIT),
    (FLUSH, FnId::FLUSH, &[], TypeId::UNIT),
    (GETCHAR, FnId::GETCHAR, &[], TypeId::STRING),
    (ORD, FnId::ORD, &[TypeId::STRING], TypeId::INT),
    (CHR, FnId::CHR, &[TypeId::INT], TypeId::STRING),
    (SIZE, FnId::SIZE, &[TypeId::STRING], TypeId::INT),
    (
        SUBSTRING,
        FnId::SUBSTRING,
        &[TypeId::STRING, TypeId::INT, TypeId::INT],
        TypeId::STRING,
    ),
    (
        CONCAT,
        FnId::CONCAT,
        &[TypeId::STRING, TypeId::STRING],
        TypeId::STRING,
    ),
    (NOT, FnId::NOT, &[TypeId::INT], TypeId::INT),
    (EXIT, FnId::EXIT, &[TypeId::INT], TypeId::UNIT),
];

pub static BUILTIN_FUNC_NAME_MAP: Lazy<HashMap<&str, &str>> = Lazy::new(|| {
    let mut map = HashMap::new();
    for (name, _, _, _) in BUILTIN_FUNCS {
        if *name == EXIT {
            map.insert(EXIT, "tiger_exit");
            continue;
        }
        map.insert(*name, *name);
    }
    map
});

impl TypeId {
    pub const DUMMY: Self = Self::new(0);
    pub const UNIT: Self = Self::new(1);
    pub const NIL: Self = Self::new(2);
    pub const INT: Self = Self::new(3);
    pub const STRING: Self = Self::new(4);
}

impl FnId {
    pub const DUMMY: Self = Self::new(0);
    pub const PRINT: Self = Self::new(1);
    pub const FLUSH: Self = Self::new(2);
    pub const GETCHAR: Self = Self::new(3);
    pub const ORD: Self = Self::new(4);
    pub const CHR: Self = Self::new(5);
    pub const SIZE: Self = Self::new(6);
    pub const SUBSTRING: Self = Self::new(7);
    pub const CONCAT: Self = Self::new(8);
    pub const NOT: Self = Self::new(9);
    pub const EXIT: Self = Self::new(10);
}

impl Builtin for Env<TypeId> {
    fn with_builtin(mut self) -> Self {
        let base_types = vec![("int", TypeId::INT), ("string", TypeId::STRING)];

        for (sym, type_id) in base_types {
            let sym = Symbol::new(sym);
            self.enter(sym, type_id);
        }
        self.begin_scope();
        self
    }
}

impl Builtin for Env<EnvEntry> {
    fn with_builtin(mut self) -> Self {
        for (name, id, _, _) in BUILTIN_FUNCS {
            let sym = Symbol::from(*name);
            self.enter(sym, EnvEntry::new_func(*id));
        }

        self.begin_scope();
        self
    }
}

impl Builtin for TyCtx {
    fn with_builtin(mut self) -> Self {
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

impl<'tcx, F: Frame> Builtin for Translator<'tcx, F> {
    fn with_builtin(mut self) -> Self {
        for (_, id, _, _) in BUILTIN_FUNCS {
            self.fn_env.insert(*id, Level::outermost());
        }

        self
    }
}

pub trait IsBuiltin {
    fn is_builtin(&self) -> bool;
}

impl IsBuiltin for Label {
    fn is_builtin(&self) -> bool {
        match self {
            Label::NamedFn(name) => BUILTIN_FUNCS.iter().any(|(n, _, _, _)| n == name),
            _ => false,
        }
    }
}

impl<T: IsBuiltin> IsBuiltin for &T {
    fn is_builtin(&self) -> bool {
        (*self).is_builtin()
    }
}
