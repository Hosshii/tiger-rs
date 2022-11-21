use super::ast::{
    BlockType, Export, ExportKind, Expr, Func, FuncTypeDef, Index, Instruction, Module, TypeUse,
};

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

        for ele in func.instr.iter_mut() {
            self.rewrite_instr(ele);
        }
    }

    fn rewrite_block_type(&mut self, ty: &mut BlockType) {
        if let TypeUse::Inline(ref func_ty) = ty.0 {
            if func_ty.params.is_empty() && func_ty.result.len() <= 1 {
                return;
            }

            let idx = self.fn_type_idx;
            self.fn_type_idx += 1;
            self.types.push(FuncTypeDef {
                name: None,
                params: func_ty.params.clone(),
                result: func_ty.result.clone(),
            });
            ty.0 = TypeUse::Index(Index::Index(idx));
        }
    }

    fn rewrite_instr(&mut self, instr: &mut Instruction) {
        match instr {
            Instruction::Expr(e) => self.rewrite_expr(e),
            Instruction::Op(_) => (),
        }
    }

    fn rewrite_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Op(_) => (),
            Expr::OpExpr(_, e) => {
                for ele in e {
                    self.rewrite_expr(ele);
                }
            }
            Expr::Block(_, ty, instr) => {
                self.rewrite_block_type(ty);
                for ele in instr {
                    self.rewrite_instr(ele);
                }
            }
        }
    }
}
