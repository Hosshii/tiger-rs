use std::sync::atomic::AtomicU32;

use crate::symbol::Symbol;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
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
    Name {
        sym: Symbol,
        ty: Option<Box<Type>>,
    },
}

static UNIQUE_INDEX: AtomicU32 = AtomicU32::new(0);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Unique(u32);

impl Unique {
    pub fn new() -> Self {
        Unique(UNIQUE_INDEX.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}
