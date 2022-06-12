use super::Frame;

pub struct X86 {}

impl Frame for X86 {
    type Access = Access;

    fn new(name: crate::temp::Label, formals: Vec<bool>) -> Self {
        todo!()
    }

    fn name(&self) -> &crate::temp::Label {
        todo!()
    }

    fn formals(&self) -> &[Self::Access] {
        todo!()
    }

    fn alloc_local(&self, is_escape: bool) -> Self::Access {
        todo!()
    }
}

#[derive(Clone)]
pub struct Access {}
