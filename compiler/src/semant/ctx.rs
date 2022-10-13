use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::common::Symbol;

use super::{
    env::Env,
    types::{Type, TypeId},
};

pub struct TyCtx {
    ty: HashMap<TypeId, Rc<Type>>,
    ty_rev: HashMap<Rc<Type>, TypeId>,
    reserved: HashSet<TypeId>,
}

impl TyCtx {
    pub fn new() -> Self {
        let mut s = Self {
            ty: HashMap::new(),
            ty_rev: HashMap::new(),
            reserved: HashSet::new(),
        };
        let builtin = [
            (TypeId::unit(), Type::Unit),
            (TypeId::int(), Type::Int),
            (TypeId::string(), Type::String),
            (TypeId::nil(), Type::Nil),
        ];

        for (type_id, type_) in builtin {
            s.insert_inner(type_id, type_)
        }
        s
    }

    pub fn new_ty_env(&mut self) -> Env<TypeId> {
        let mut env = Env::new();
        let base_types = vec![("int", TypeId::int()), ("string", TypeId::string())];

        for (sym, type_id) in base_types {
            let sym = Symbol::new(sym);
            env.enter(sym, type_id);
        }
        env.begin_scope();
        env
    }

    pub fn insert(&mut self, ty: Type) -> TypeId {
        let id = TypeId::new_uniq();
        self.insert_inner(id, ty);
        id
    }

    fn insert_inner(&mut self, id: TypeId, ty: Type) {
        let ty = Rc::new(ty);
        self.ty.insert(id, ty.clone());
        self.ty_rev.insert(ty, id);
    }

    /// reserve a type id
    pub fn reserve(&mut self) -> TypeId {
        let id = TypeId::new_uniq();
        assert!(self.reserved.insert(id));
        id
    }

    pub fn set_reserved(&mut self, id: TypeId, ty: Type) {
        assert!(self.reserved.remove(&id));
        self.insert_inner(id, ty);
    }

    pub fn has_invalid_recursion(&self) -> bool {
        !self.reserved.is_empty()
    }

    pub fn type_(&self, id: TypeId) -> &Type {
        assert!(!self.reserved.contains(&id));

        // TODO
        self.ty.get(&id).expect("type not found")
    }

    pub fn type_id(&self, ty: &Type) -> Option<TypeId> {
        // TODO
        self.ty_rev.get(ty).copied()
    }
}
