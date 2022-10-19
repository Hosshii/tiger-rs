use std::{fmt::Display, sync::atomic::AtomicU32};

use crate::common::Symbol;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

impl TypeId {
    pub const fn new(num: u32) -> Self {
        Self(num)
    }
}

/// `Type` represents tiger language's type.
/// `Complete` can determine the type just by looking at it and it never changes.
/// `InComplete` may be incomplete due to mutual recursion etc. And may be changed later.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Int,
    String,
    Record {
        fields: Vec<(Symbol, TypeId)>,
        unique: Unique,
    },
    Array {
        ty: TypeId,
        unique: Unique,
    },
    Nil,
    Unit,
}

impl Type {
    pub fn dummy_record() -> Self {
        Self::Record {
            fields: Vec::new(),
            unique: Unique::dummy(),
        }
    }

    pub fn dummy_array() -> Self {
        Self::Array {
            ty: TypeId::dummy(),
            unique: Unique::dummy(),
        }
    }

    pub fn assignable(&self, other: &Self) -> bool {
        use self::Type::*;
        match (self, other) {
            (Nil, Record { .. }) | (Record { .. }, Nil) => true,
            _ => self == other,
        }
    }
}

#[derive(Debug, Clone, Copy, Error, PartialEq, Eq)]
pub struct IncompleteTypeError(pub Symbol);

impl Display for IncompleteTypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "incomplete type: {}", self.0.name())
    }
}

// 0 is dummy
static UNIQUE_INDEX: AtomicU32 = AtomicU32::new(1);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Unique(u32);

impl Unique {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Unique(UNIQUE_INDEX.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }

    pub fn dummy() -> Self {
        Self(0)
    }
}
