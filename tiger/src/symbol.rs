use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use once_cell::sync::Lazy;

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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Symbol(SymbolIndex);

impl Symbol {
    pub fn new(s: impl Into<String>) -> Self {
        SYMBOL_GLOBAL.lock().unwrap().new_symbol(s)
    }

    pub fn name(&self) -> Arc<String> {
        SYMBOL_GLOBAL.lock().unwrap().name(self)
    }

    fn as_u32(&self) -> u32 {
        self.0.private
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

        for (sym_str, num) in cases {
            let sym = Symbol::new(sym_str);
            assert_eq!(num, sym.as_u32());
            assert_eq!(sym_str, sym.name().as_str());
        }
    }
}