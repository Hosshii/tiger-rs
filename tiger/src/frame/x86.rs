use crate::temp::{Label, Temp};

use super::Frame;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X86 {
    name: Label,
    formals: Vec<Access>,
    pointer: i64,
}

const PTR_SIZE: i64 = 8;

impl Frame for X86 {
    type Access = Access;

    fn new(name: Label, formals: Vec<bool>) -> Self {
        let mut frame = Self {
            name,
            formals: vec![],
            pointer: 0,
        };
        let formals = formals.into_iter().map(|v| frame.alloc_local(v)).collect();
        frame.formals = formals;
        frame
    }

    fn name(&self) -> &Label {
        &self.name
    }

    fn formals(&self) -> &[Self::Access] {
        &self.formals
    }

    fn alloc_local(&mut self, is_escape: bool) -> Self::Access {
        if is_escape {
            self.pointer += PTR_SIZE;
            Access::InFrame(self.pointer)
        } else {
            Access::InReg(Temp::new())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Access {
    InFrame(i64),
    InReg(Temp),
}
