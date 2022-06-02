use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::{Arc, Mutex},
};

use once_cell::sync::Lazy;

use crate::parser::ast::{Ident, TypeIdent};

static SYMBOL_GLOBAL: Lazy<Mutex<SymbolGlobal>> = Lazy::new(|| Mutex::new(SymbolGlobal::default()));

#[derive(Debug, Default)]
struct SymbolGlobal {
    count: u32,
    table: HashMap<Arc<String>, SymbolIndex>,
    name: Vec<Arc<String>>,
}

impl SymbolGlobal {
    fn new_symbol(&mut self, s: impl Into<String>) -> Symbol {
        let s = Arc::new(s.into());
        let idx = self.table.entry(s.clone()).or_insert_with(|| {
            let sym = SymbolIndex {
                private: self.count,
            };
            self.count += 1;
            self.name.push(s);
            sym
        });

        assert!((idx.private as usize) < self.name.len());

        Symbol(*idx)
    }

    fn name(&self, s: &Symbol) -> Arc<String> {
        self.name[s.0.private as usize].clone()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(SymbolIndex);

impl Symbol {
    pub fn new(s: impl Into<String>) -> Self {
        SYMBOL_GLOBAL.lock().unwrap().new_symbol(s)
    }

    pub fn name(&self) -> Arc<String> {
        SYMBOL_GLOBAL.lock().unwrap().name(self)
    }

    #[allow(dead_code)]
    fn as_u32(&self) -> u32 {
        self.0.private
    }

    pub fn dummy() -> Self {
        Self(SymbolIndex { private: u32::MAX })
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(index: {}, name: {})", self.0.private, self.name())
    }
}

impl<T> From<T> for Symbol
where
    T: Into<String>,
{
    fn from(s: T) -> Self {
        Self::new(s)
    }
}

impl From<Ident> for Symbol {
    fn from(id: Ident) -> Self {
        Self::new(id.0)
    }
}

impl From<&Ident> for Symbol {
    fn from(id: &Ident) -> Self {
        Self::new(&id.0)
    }
}

impl From<TypeIdent> for Symbol {
    fn from(id: TypeIdent) -> Self {
        id.0.into()
    }
}

impl From<&TypeIdent> for Symbol {
    fn from(id: &TypeIdent) -> Self {
        (&id.0).into()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct SymbolIndex {
    private: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol() {
        let cases = vec![
            ("aa", 0),
            ("bb", 1),
            ("aa", 0),
            ("heohwearf", 2),
            ("hello world", 3),
        ];

        let mut symbol_global = SymbolGlobal::default();
        for (sym_str, num) in cases {
            let sym = symbol_global.new_symbol(sym_str);
            assert_eq!(num, sym.as_u32());
            assert_eq!(sym_str, symbol_global.name(&sym).as_str());
        }
    }
}
