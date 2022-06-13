mod x86;

use crate::temp::Label;
pub use x86::X86;

pub trait Frame: Clone {
    type Access: Clone;

    fn new(name: Label, formals: Vec<bool>) -> Self;
    fn name(&self) -> &Label;
    fn formals(&self) -> &[Self::Access];
    fn alloc_local(&self, is_escape: bool) -> Self::Access;
}
