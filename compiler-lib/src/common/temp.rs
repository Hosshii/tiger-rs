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

    #[cfg(test)]
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
    // unique identifier.
    Num(u32),
    // function that has unique identifier.
    Fn(u32, String),
    // use name directory.
    NamedFn(String),
}

impl Label {
    pub fn new() -> Self {
        Self::Num(LABEL_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }

    pub fn new_fn(name: impl Into<String>) -> Self {
        Self::Fn(
            LABEL_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            name.into(),
        )
    }

    pub fn with_num(num: u32) -> Self {
        Self::Num(num)
    }

    pub fn with_named_fn(name: String) -> Self {
        Self::NamedFn(name)
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Num(n) => {
                write!(f, "{}", n)
            }
            Label::Fn(n, s) => {
                write!(f, "{}_{}", n, s)
            }
            Label::NamedFn(s) => write!(f, "{}", s),
        }
    }
}

impl Default for Label {
    fn default() -> Self {
        Self::new()
    }
}
