use std::{fmt::Display, sync::atomic::AtomicU32};

static TEMP_GLOBAL: AtomicU32 = AtomicU32::new(0);

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Temp {
    num: u32,
}

impl Temp {
    pub fn new() -> Self {
        Temp {
            num: TEMP_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        }
    }

    pub fn new_with(num: u32) -> Self {
        Temp { num }
    }
}

impl Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.num)
    }
}

impl Default for Temp {
    fn default() -> Self {
        Self::new()
    }
}

static LABEL_GLOBAL: AtomicU32 = AtomicU32::new(0);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Label {
    Num(u32),
    Named(String),
}

impl Label {
    pub fn new() -> Self {
        Self::Num(LABEL_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }

    pub fn with_num(num: u32) -> Self {
        Self::Num(num)
    }

    pub fn with_name(name: String) -> Self {
        Self::Named(name)
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Num(n) => write!(f, "{}", n),
            Label::Named(s) => write!(f, "{}", s),
        }
    }
}

impl Default for Label {
    fn default() -> Self {
        Self::new()
    }
}
