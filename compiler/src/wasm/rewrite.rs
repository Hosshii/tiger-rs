use std::collections::HashMap;

use super::ast::{
    BlockType, Export, ExportKind, Expr, Func, FuncTypeDef, ImportKind, Index, Instruction, Module,
    Name, Operator, TypeUse,
};

pub(super) struct Rewriter {
    types: Vec<FuncTypeDef>,
    exports: Vec<Export>,
    // local_name_map: HashMap<Name, Index>,
    global_name_map: HashMap<Name, Index>,
    func_name_map: HashMap<Name, Index>,
}

impl Rewriter {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            exports: Vec::new(),
            // local_name_map: HashMap::new(),
            global_name_map: HashMap::new(),
            func_name_map: HashMap::new(),
        }
    }

    fn fn_type_idx(&self) -> u32 {
        self.types.len() as u32
    }

    fn insert_global_name(&mut self, name: Name) -> Index {
        let idx = Index::Index(self.global_name_map.len() as u32);
        let global = self.global_name_map.insert(name, idx.clone());
        assert!(global.is_none());
        idx
    }

    fn insert_func_name(&mut self, name: Name) -> Index {
        let idx = Index::Index(self.func_name_map.len() as u32);
        let func = self.func_name_map.insert(name, idx.clone());
        assert!(func.is_none());
        idx
    }

    pub fn rewrite(&mut self, module: &mut Module) {
        self.types.append(&mut module.types);
        self.exports.append(&mut module.exports);

        // firstly, decide global index
        for global in module.globals.iter() {
            if let Some(ref name) = global.name {
                self.insert_global_name(name.clone());
            }
        }

        // secondly, decide import func index
        for func in module.imports.iter_mut() {
            if let ImportKind::Func(Some(Index::Name(name)), ty_use) = &func.kind {
                let idx = self.insert_func_name(name.clone());

                func.kind = ImportKind::Func(Some(idx), ty_use.clone());
            }

            if let ImportKind::Func(ref name, TypeUse::Inline(ref t)) = func.kind {
                let idx = self.fn_type_idx();
                self.types.push(FuncTypeDef {
                    name: None,
                    ty: t.clone(),
                });

                func.kind = ImportKind::Func(name.clone(), TypeUse::Index(Index::Index(idx)));
            }
        }

        // thirdly, decide func index
        for func in module.funcs.iter() {
            if let Some(ref name) = func.name {
                self.insert_func_name(name.clone());
            }
        }

        for func in module.funcs.iter_mut() {
            self.rewrite_func(func);
        }

        module.types.append(&mut self.types);
        module.exports.append(&mut self.exports);
    }

    fn rewrite_func(&mut self, func: &mut Func) {
        let func_ty_indx = match func.ty {
            TypeUse::Index(ref n) => n.clone(),
            TypeUse::Inline(ref ty) => {
                let idx = self.fn_type_idx();
                self.types.push(FuncTypeDef {
                    name: None,
                    ty: ty.clone(),
                });
                func.ty = TypeUse::Index(Index::Index(idx));
                Index::Index(idx)
            }
        };

        if let Some(ref export) = func.export {
            self.exports.push(Export {
                name: export.name.clone(),
                kind: ExportKind::Func(func_ty_indx),
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
            Expr::Comment(_, e) => self.rewrite_expr(e),
            Expr::Op(op) => self.rewrite_operator(op),
            Expr::OpExpr(op, e) => {
                self.rewrite_operator(op);
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
            Expr::If(_, block_ty, cond, then, els) => {
                self.rewrite_block_type(block_ty);
                for ele in cond {
                    self.rewrite_expr(ele);
                }
                for ele in then {
                    self.rewrite_instr(ele);
                }
                if let Some(els) = els {
                    for ele in els {
                        self.rewrite_instr(ele);
                    }
                }
            }
            Expr::Loop(_, block_ty, instr) => {
                self.rewrite_block_type(block_ty);
                for ele in instr {
                    self.rewrite_instr(ele);
                }
            }
        }
    }

    fn rewrite_operator(&self, op: &mut Operator) {
        match op {
            Operator::GlobalGet(index) | Operator::GlobalSet(index) => {
                if let Index::Name(name) = index {
                    if let Some(idx) = self.global_name_map.get(name) {
                        *index = idx.clone();
                    }
                }
            }
            Operator::Call(Index::Name(name)) => {
                if let Some(idx) = self.func_name_map.get(name) {
                    *op = Operator::Call(idx.clone());
                }
            }
            _ => (),
        }
    }
}
