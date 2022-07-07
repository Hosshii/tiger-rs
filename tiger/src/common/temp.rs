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
pub struct Label {
    num: u32,
}

impl Label {
    pub fn new() -> Self {
        Self {
            num: LABEL_GLOBAL.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.num)
    }
}

impl Default for Label {
    fn default() -> Self {
        Self::new()
    }
}
