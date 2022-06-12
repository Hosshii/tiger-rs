use std::sync::atomic::AtomicU32;

use crate::{
    frame::{Frame, X86 as X86Frame},
    temp::Label,
};

pub trait Translate {
    type Level: Clone;
    type Access: Clone;

    fn outermost() -> Self::Level;
    fn new_level(parent: Self::Level, name: Label, formals: Vec<bool>) -> Self::Level;

    fn formals(level: Self::Level) -> Vec<Self::Access>;
    fn alloc_local(level: &Self::Level, is_escape: bool) -> Self::Access;
}

#[derive(Debug)]
pub struct Expr {}

impl Expr {
    pub fn new() -> Self {
        Self {}
    }
}

pub struct X86(Translator<X86Frame>);

pub struct Translator<F: Frame> {
    frame: F,
}

impl<F: Frame> Translate for Translator<F> {
    type Level = Level;
    type Access = (Self::Level, F::Access);

    fn outermost() -> Self::Level {
        Level::outermost()
    }

    fn new_level(parent: Self::Level, name: Label, formals: Vec<bool>) -> Self::Level {
        let frame = F::new(name, formals);
        let level = Level::new(parent);
        level
    }

    fn formals(level: Self::Level) -> Vec<Self::Access> {
        todo!()
    }

    fn alloc_local(level: &Self::Level, is_escape: bool) -> Self::Access {
        todo!()
    }
}

static LEVEL_GLOBAL: AtomicU32 = AtomicU32::new(1);

#[derive(Debug, Clone)]
pub struct Level {
    current: u32, // unique value
    parent: Option<Box<Level>>,
}

impl Level {
    fn new(parent: Level) -> Self {
        let current = LEVEL_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let parent = Some(Box::new(parent));
        Self { current, parent }
    }

    pub fn outermost() -> Self {
        Self {
            current: 0,
            parent: None,
        }
    }
}
