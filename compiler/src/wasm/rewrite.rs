use super::ast::{Export, ExportKind, Func, FuncTypeDef, Index, Module, TypeUse};

pub(super) struct Rewriter {
    fn_type_idx: u32,
    types: Vec<FuncTypeDef>,
    export: Vec<Export>,
}

impl Rewriter {
    pub fn new() -> Self {
        Self {
            fn_type_idx: 0, // initialize in rewrite.
            types: Vec::new(),
            export: Vec::new(),
        }
    }

    pub fn rewrite(&mut self, module: &mut Module) {
        self.fn_type_idx = module.types.len() as u32;

        for (idx, func) in module.func.iter_mut().enumerate() {
            self.rewrite_func(func, idx);
        }

        module.types.append(&mut self.types);
        module.export.append(&mut self.export);
    }

    fn rewrite_func(&mut self, func: &mut Func, idx: usize) {
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

        if let Some(ref export) = func.export {
            self.export.push(Export {
                name: export.name.clone(),
                kind: ExportKind::Func(Index::Index(idx as u32)),
            });
        }
    }
}
