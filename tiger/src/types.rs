use std::{fmt::Display, sync::atomic::AtomicU32};

use crate::{env::Env, symbol::Symbol};
use thiserror::Error;

/// `Type` represents tiger language's type.
/// `Complete` can determine the type just by looking at it and it never changes.
/// `InComplete` may be incomplete due to mutual recursion etc. And may be changed later.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Complete(CompleteType),
    InComplete(IncompleteType),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompleteType {
    Int,
    String,
    Record {
        fields: Vec<(Symbol, Type)>,
        unique: Unique,
    },
    Array {
        ty: Box<Type>,
        unique: Unique,
    },
    Nil,
    Unit,
}

impl CompleteType {
    pub fn dummy_record() -> Self {
        Self::Record {
            fields: Vec::new(),
            unique: Unique::dummy(),
        }
    }

    pub fn dummy_array() -> Self {
        Self::Array {
            ty: Box::new(Type::InComplete(IncompleteType {
                sym: Symbol::dummy(),
                ty: None,
            })),
            unique: Unique::dummy(),
        }
    }

    pub fn assignable(&self, other: &Self) -> bool {
        use self::CompleteType::*;
        match (self, other) {
            (Nil, Record { .. }) | (Record { .. }, Nil) => true,
            _ => self == other,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IncompleteType {
    pub sym: Symbol,
    pub ty: Option<Box<Type>>,
}

impl Type {
    // TODO: should use `Either` instead of result
    pub fn actual<'a>(
        &'a self,
        env: &'a Env<Type>,
    ) -> Result<&'a CompleteType, IncompleteTypeError> {
        match self {
            Type::Complete(c) => Ok(c),
            Type::InComplete(i) => match &i.ty {
                Some(ty) => ty.actual(env),
                None => match env.look(i.sym) {
                    None => Err(IncompleteTypeError(i.sym)),
                    Some(ty) => ty.actual(env),
                },
            },
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
