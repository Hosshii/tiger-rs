use std::collections::HashMap;

use crate::symbol::Symbol;

type Bucket<T> = Vec<T>;

struct Stack<T> {
    // None indicates separator
    inner: Vec<Option<T>>,
}

impl<T> Stack<T> {
    fn new() -> Self {
        Self { inner: Vec::new() }
    }

    fn push(&mut self, t: T) {
        self.inner.push(Some(t))
    }

    fn begin_scope(&mut self) {
        self.inner.push(None)
    }

    fn end_scope<F>(&mut self, mut f: F)
    where
        F: FnMut(T),
    {
        while let Some(Some(t)) = self.inner.pop() {
            f(t);
        }
    }
}

struct EnvTable<T> {
    inner: HashMap<Symbol, Bucket<T>>,
}

impl<T> EnvTable<T> {
    fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    fn enter(&mut self, sym: Symbol, binding: T) {
        let bucket = self.inner.entry(sym).or_default();
        bucket.push(binding);
    }

    fn look(&self, sym: Symbol) -> Option<&T> {
        self.inner.get(&sym).and_then(|v| v.last())
    }

    fn remove(&mut self, sym: Symbol) {
        if let Some(bucket) = self.inner.get_mut(&sym) {
            bucket.pop();
            if bucket.is_empty() {
                self.inner.remove(&sym);
            }
        }
    }
}

/// Correspond `Symbol` to `T`
pub struct Env<T> {
    inner: EnvTable<T>,
    scope_stack: Stack<Symbol>,
}

impl<T> Env<T> {
    pub fn new() -> Self {
        Self {
            inner: EnvTable::new(),
            scope_stack: Stack::new(),
        }
    }

    /// bind the symbol `sym` to the `binding`
    /// ex. env.enter(Symbol1, Type::Int) adds bindings of `Symbol1 -> Type::Int` .
    pub fn enter(&mut self, sym: Symbol, binding: T) {
        self.inner.enter(sym, binding);
        self.scope_stack.push(sym);
    }

    /// Find the binding correspond to the given symbol.
    /// If there are more than two bindings, the latest (last of the bindings that have not yet been deleted) added binding is returned.
    pub fn look(&mut self, sym: Symbol) -> Option<&T> {
        self.inner.look(sym)
    }

    /// Create new scope.
    /// After this operation, the bindings with the same name are shadowed.
    pub fn begin_scope(&mut self) {
        self.scope_stack.begin_scope();
    }

    /// Delete current scope. Bindings added in the current scope are also deleted.
    pub fn end_scope(&mut self) {
        self.scope_stack.end_scope(|sym| self.inner.remove(sym));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_env() {
        use Operation::*;
        enum Operation<T> {
            Enter {
                sym: &'static str,
                binding: T,
            },
            Look {
                sym: &'static str,
                expected: Option<T>,
            },
            BeginScope,
            EndScope,
        }
        let mut env = Env::new();

        let cases = vec![
            Enter {
                sym: "a",
                binding: 1,
            },
            Enter {
                sym: "b",
                binding: 2,
            },
            Look {
                sym: "a",
                expected: Some(1),
            },
            Look {
                sym: "b",
                expected: Some(2),
            },
            Look {
                sym: "c",
                expected: None,
            },
            Enter {
                sym: "a",
                binding: 3,
            },
            Look {
                sym: "a",
                expected: Some(3),
            },
            BeginScope,
            Enter {
                sym: "a",
                binding: 10,
            },
            Enter {
                sym: "c",
                binding: 11,
            },
            Look {
                sym: "a",
                expected: Some(10),
            },
            Look {
                sym: "b",
                expected: Some(2),
            },
            Look {
                sym: "c",
                expected: Some(11),
            },
            Look {
                sym: "d",
                expected: None,
            },
            BeginScope,
            Enter {
                sym: "a",
                binding: 100,
            },
            Enter {
                sym: "b",
                binding: 111,
            },
            Look {
                sym: "a",
                expected: Some(100),
            },
            Look {
                sym: "b",
                expected: Some(111),
            },
            Look {
                sym: "c",
                expected: Some(11),
            },
            Look {
                sym: "d",
                expected: None,
            },
            EndScope,
            Look {
                sym: "a",
                expected: Some(10),
            },
            Look {
                sym: "b",
                expected: Some(2),
            },
            Look {
                sym: "c",
                expected: Some(11),
            },
            Look {
                sym: "d",
                expected: None,
            },
            EndScope,
            Look {
                sym: "a",
                expected: Some(3),
            },
            Look {
                sym: "b",
                expected: Some(2),
            },
            Look {
                sym: "c",
                expected: None,
            },
        ];

        for op in cases {
            match op {
                Enter { sym, binding } => {
                    let sym = Symbol::new(sym);
                    env.enter(sym, binding);
                }
                Look { sym, expected } => {
                    let sym = Symbol::new(sym);
                    let got = env.look(sym);
                    assert_eq!(expected.as_ref(), got);
                }
                BeginScope => env.begin_scope(),
                EndScope => env.end_scope(),
            }
        }
    }
}
