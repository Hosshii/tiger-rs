use std::collections::HashMap;

use super::ast::{
    BlockType, Export, ExportKind, Expr, Func, FuncTypeDef, Index, Instruction, Module, Name,
    Operator, TypeUse,
};

pub(super) struct Rewriter {
    types: Vec<FuncTypeDef>,
    export: Vec<Export>,
    // local_name_map: HashMap<Name, Index>,
    global_name_map: HashMap<Name, Index>,
}

impl Rewriter {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            export: Vec::new(),
            // local_name_map: HashMap::new(),
            global_name_map: HashMap::new(),
        }
    }

    fn fn_type_idx(&self) -> u32 {
        self.types.len() as u32
    }

    pub fn rewrite(&mut self, module: &mut Module) {
        // firstly, decide global index
        for (idx, global) in module.globals.iter().enumerate() {
            if let Some(ref name) = global.name {
                self.global_name_map
                    .insert(name.clone(), Index::Index(idx as u32));
            }
        }

        std::mem::swap(&mut self.types, &mut module.types);
        std::mem::swap(&mut self.export, &mut module.exports);

        for (idx, func) in module.funcs.iter_mut().enumerate() {
            self.rewrite_func(func, idx);
        }

        module.types.append(&mut self.types);
        module.exports.append(&mut self.export);
    }

    fn rewrite_func(&mut self, func: &mut Func, fn_idx: usize) {
        match func.ty {
            TypeUse::Index(_) => (),
            TypeUse::Inline(ref ty) => {
                let idx = self.fn_type_idx();
                self.types.push(FuncTypeDef {
                    name: None,
                    ty: ty.clone(),
                });
                func.ty = TypeUse::Index(Index::Index(idx));
            }
        }

        if let Some(ref export) = func.export {
            self.export.push(Export {
                name: export.name.clone(),
                kind: ExportKind::Func(Index::Index(fn_idx as u32)),
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

            let idx = self.fn_type_idx();
            self.types.push(FuncTypeDef {
                name: None,
                ty: func_ty.clone(),
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
            Expr::Op(Operator::GlobalGet(Index::Name(name))) => {
                if let Some(idx) = self.global_name_map.get(name) {
                    *expr = Expr::Op(Operator::GlobalGet(idx.clone()));
                }
            }
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
