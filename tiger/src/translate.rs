use std::{marker::PhantomData, sync::atomic::AtomicU32};

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
    fn alloc_local(level: Self::Level, is_escape: bool) -> Self::Access;
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
    _phantomdata: PhantomData<fn() -> F>,
}

impl<F: Frame> Translate for Translator<F> {
    type Level = Level<F>;
    type Access = (Self::Level, F::Access);

    fn outermost() -> Self::Level {
        Level::outermost()
    }

    fn new_level(parent: Self::Level, name: Label, formals: Vec<bool>) -> Self::Level {
        let frame = F::new(name, formals);
        Level::new(parent, frame)
    }

    fn formals(level: Self::Level) -> Vec<Self::Access> {
        level
            .frame
            .formals()
            .iter()
            .map(|access| (level.clone(), access.clone()))
            .collect()
    }

    fn alloc_local(level: Self::Level, is_escape: bool) -> Self::Access {
        let frame = level.frame.alloc_local(is_escape);
        (level, frame)
    }
}

static LEVEL_GLOBAL: AtomicU32 = AtomicU32::new(1);

#[derive(Debug, Clone)]
pub struct Level<F: Frame> {
    current: u32, // unique value
    frame: F,
    parent: Option<Box<Level<F>>>,
}

impl<F: Frame> Level<F> {
    fn new(parent: Level<F>, frame: F) -> Self {
        let current = LEVEL_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let parent = Some(Box::new(parent));
        Self {
            current,
            frame,
            parent,
        }
    }

    pub fn outermost() -> Self {
        Self {
            current: 0,
            frame: F::new(Label::new(), vec![]),
            parent: None,
        }
    }
}
