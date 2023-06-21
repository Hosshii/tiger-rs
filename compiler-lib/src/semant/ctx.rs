use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::common::{Label, Symbol};

use super::types::{Type, TypeId};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct VarId(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FnId(u32);

impl FnId {
    pub const fn new(num: u32) -> Self {
        Self(num)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct VarEntry {
    id: TypeId,
    sym: Symbol,
}

impl VarEntry {
    pub fn new(id: TypeId, sym: Symbol) -> Self {
        Self { id, sym }
    }

    pub fn type_id(&self) -> TypeId {
        self.id
    }

    pub fn sym(&self) -> Symbol {
        self.sym
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FnEntry {
    name: Symbol,
    label: Label,
    formals: Vec<TypeId>,
    result: TypeId,
}

impl FnEntry {
    pub fn new(name: Symbol, label: Label, formals: Vec<TypeId>, result: TypeId) -> Self {
        Self {
            name,
            label,
            formals,
            result,
        }
    }

    pub fn sym(&self) -> Symbol {
        self.name
    }

    pub fn label(&self) -> &Label {
        &self.label
    }

    pub fn formals(&self) -> &[TypeId] {
        self.formals.as_ref()
    }

    pub fn result(&self) -> TypeId {
        self.result
    }
}

pub struct TyCtx {
    ty_count: u32, // 0 ~ 9 : for builtin.
    ty: HashMap<TypeId, Rc<Type>>,
    ty_rev: HashMap<Rc<Type>, TypeId>,
    reserved: HashSet<TypeId>,

    /// common to var and func.
    /// 0 ~ 9 : reserved.
    var_count: u32,
    var: HashMap<VarId, VarEntry>,
    func: HashMap<FnId, FnEntry>,
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            ty_count: 10,
            ty: HashMap::new(),
            ty_rev: HashMap::new(),
            reserved: HashSet::new(),

            var_count: 10,
            var: HashMap::new(),
            func: HashMap::new(),
        }
    }

    fn gen_type_id(&mut self) -> TypeId {
        let id = TypeId::new(self.ty_count);
        self.ty_count += 1;
        id
    }

    fn gen_var_id(&mut self) -> VarId {
        let id = VarId(self.var_count);
        self.var_count += 1;
        id
    }

    fn gen_fn_id(&mut self) -> FnId {
        let id = FnId(self.var_count);
        self.var_count += 1;
        id
    }

    pub(super) fn insert_ty_inner(&mut self, id: TypeId, ty: Type) {
        let ty = Rc::new(ty);
        self.ty.insert(id, ty.clone());
        self.ty_rev.insert(ty, id);
    }

    pub(super) fn insert_fn_inner(&mut self, id: FnId, entry: FnEntry) {
        self.func.insert(id, entry);
    }

    pub fn new_var(&mut self, e: VarEntry) -> VarId {
        let id = self.gen_var_id();
        self.var.insert(id, e);
        id
    }

    pub fn var(&self, id: VarId) -> &VarEntry {
        &self.var[&id]
    }

    pub fn new_fn(&mut self, e: FnEntry) -> FnId {
        let id = self.gen_fn_id();
        self.insert_fn_inner(id, e);
        id
    }

    pub fn fn_(&self, id: FnId) -> &FnEntry {
        &self.func[&id]
    }

    /// reserve a type id
    pub fn reserve_type(&mut self) -> TypeId {
        let id = self.gen_type_id();
        assert!(self.reserved.insert(id));
        id
    }

    pub fn set_reserved_type(&mut self, id: TypeId, ty: Type) {
        assert!(self.reserved.remove(&id));
        self.insert_ty_inner(id, ty);
    }

    pub fn type_(&self, id: TypeId) -> &Type {
        assert!(!self.reserved.contains(&id));

        // TODO
        self.ty.get(&id).expect("type not found")
    }
}

impl Default for TyCtx {
    fn default() -> Self {
        Self::new()
    }
}
