use super::ast::{Func, FuncTypeDef, Index, Module, TypeUse};

pub(super) struct Rewriter {
    fn_type_idx: u32,
    types: Vec<FuncTypeDef>,
}

impl Rewriter {
    pub fn new() -> Self {
        Self {
            fn_type_idx: 0, // initialize in rewrite.
            types: Vec::new(),
        }
    }

    pub fn rewrite(&mut self, module: &mut Module) {
        self.fn_type_idx = module.types.len() as u32;

        for func in module.func.iter_mut() {
            self.rewrite_func(func);
        }

        module.types.append(&mut self.types);
    }

    fn rewrite_func(&mut self, func: &mut Func) {
        match func.ty {
            TypeUse::Index(_) => (),
            TypeUse::Inline(ref ty) => {
                let idx = self.fn_type_idx;
                self.fn_type_idx += 1;
                self.types.push(FuncTypeDef {
                    name: None,
                    params: ty.params.clone(),
                    result: ty.result.clone(),
                });
                func.ty = TypeUse::Index(Index::Index(idx));
            }
        }
    }
}
