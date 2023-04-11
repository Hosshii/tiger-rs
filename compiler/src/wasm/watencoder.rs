use std::fmt::Debug;

use super::{
    ast::{
        BinOp, BlockType, Export, ExportKind, Expr, Func, FuncType, FuncTypeDef, Global,
        GlobalType, Import, Index, InlineFuncExport, Instruction, Local, Module, Mut, Name,
        NumType, Operator, Param, TypeUse, ValType, WasmResult,
    },
    rewrite::Rewriter,
};

pub struct Encoder {
    pub(crate) bytes: String,
}

impl Encoder {
    pub fn new() -> Self {
        Self {
            bytes: String::new(),
        }
    }

    pub fn encode_module(mut self, module: &Module) -> String {
        module.encode(&mut self.bytes);
        self.bytes
    }

    /// Remove syntax sugar.
    pub fn rewrite_module(&mut self, module: &mut Module) {
        let mut rewriter = Rewriter::new();
        rewriter.rewrite(module);
    }
}

pub trait Encode {
    fn encode(&self, sink: &mut String);
}

impl<T: Encode + ?Sized> Encode for &'_ T {
    fn encode(&self, sink: &mut String) {
        (*self).encode(sink)
    }
}

/// https://webassembly.github.io/spec/core/binary/conventions.html#vectors
impl<T: Encode> Encode for [T] {
    fn encode(&self, sink: &mut String) {
        for item in self.iter() {
            // sink.push('(');
            item.encode(sink);
            // sink.push(')');
            sink.push(' ');
        }
    }
}

impl<T: Encode> Encode for Vec<T> {
    fn encode(&self, sink: &mut String) {
        self.as_slice().encode(sink)
    }
}

impl Encode for u8 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for usize {
    fn encode(&self, sink: &mut String) {
        assert!(*self <= u32::MAX as usize);
        (*self as u32).encode(sink)
    }
}

impl Encode for u32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for i32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for u64 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for i64 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for str {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self);
    }
}

impl<T, U> Encode for (T, U)
where
    T: Encode,
    U: Encode,
{
    fn encode(&self, sink: &mut String) {
        self.0.encode(sink);
        self.1.encode(sink);
    }
}

impl Encode for Name {
    fn encode(&self, sink: &mut String) {
        self.0.encode(sink);
    }
}

impl Encode for NumType {
    fn encode(&self, sink: &mut String) {
        match self {
            NumType::I32 => sink.push_str("i32"),
            NumType::I64 => sink.push_str("i64"),
            NumType::F32 => sink.push_str("f32"),
            NumType::F64 => sink.push_str("f64"),
        }
    }
}

impl Encode for ValType {
    fn encode(&self, sink: &mut String) {
        match self {
            ValType::Num(num_type) => num_type.encode(sink),
        }
    }
}

impl Encode for BlockType {
    fn encode(&self, sink: &mut String) {
        self.0.encode(sink);
    }
}

impl Encode for FuncType {
    fn encode(&self, sink: &mut String) {
        self.params.encode(sink);
        self.result.encode(sink);
    }
}

impl Encode for BinOp {
    fn encode(&self, sink: &mut String) {
        match self {
            BinOp::Add => sink.push_str("add"),
            BinOp::Sub => sink.push_str("sub"),
        }
    }
}

impl Encode for Expr {
    fn encode(&self, sink: &mut String) {
        match self {
            Expr::Op(op) => op.encode(sink),
            Expr::OpExpr(op, exprs) => {
                for expr in exprs {
                    expr.encode(sink);
                    sink.push('\n');
                }
                op.encode(sink);
            }
            Expr::Block(name, ty, instrs) => {
                sink.push_str("block ");
                if let Some(name) = name {
                    sink.push('$');
                    name.encode(sink);
                    sink.push(' ');
                }
                ty.encode(sink);
                sink.push('\n');
                for instr in instrs {
                    instr.encode(sink);
                    sink.push('\n');
                }
                sink.push_str("end");
            }
        }
    }
}

impl Encode for Instruction {
    fn encode(&self, sink: &mut String) {
        match self {
            Instruction::Expr(expr) => expr.encode(sink),
            Instruction::Op(op) => op.encode(sink),
        }
    }
}

impl Encode for Operator {
    fn encode(&self, sink: &mut String) {
        match self {
            Operator::GlobalGet(var) => {
                sink.push_str("global.get ");
                var.encode(sink);
            }
            Operator::LocalGet(var) => {
                sink.push_str("local.get ");
                var.encode(sink);
            }
            Operator::Store(num_type) => {
                num_type.encode(sink);
                sink.push_str(".store")
            }
            Operator::Load(num_type) => {
                num_type.encode(sink);
                sink.push_str(".load")
            }
            Operator::Bin(num_type, bin_op) => {
                num_type.encode(sink);
                sink.push('.');
                bin_op.encode(sink);
            }
            Operator::Const(num_type, value) => {
                num_type.encode(sink);
                sink.push_str(".const ");
                value.encode(sink);
            }
            Operator::Nop => {
                todo!()
            }
            Operator::Drop => {
                todo!()
            }
        }
    }
}

impl Encode for Index {
    fn encode(&self, sink: &mut String) {
        match self {
            Index::Index(index) => index.encode(sink),
            Index::Name(name) => panic!("unresolved name {}", name.0),
        }
    }
}

impl<T: Debug> Encode for TypeUse<T> {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(type ");
        match self {
            TypeUse::Index(index) => index.encode(sink),
            TypeUse::Inline(type_) => {
                panic!("unresolved type {:?}", type_)
            }
        }
        sink.push_str(" )");
    }
}

impl Encode for InlineFuncExport {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(export \"");
        self.name.encode(sink);
        sink.push_str("\" )");
    }
}

impl Encode for Func {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(func ");
        if let Some(ref export) = self.export {
            export.encode(sink);
        }
        sink.push(' ');
        self.ty.encode(sink);
        sink.push(' ');
        self.locals.encode(sink);
        sink.push(' ');
        for instr in self.instr.iter() {
            instr.encode(sink);
        }
        sink.push(')');
    }
}

impl Encode for Param {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(param ");
        if let Some(ref name) = self.name {
            sink.push('$');
            name.encode(sink);
        }
        self.type_.encode(sink);
        sink.push(')');
    }
}

impl Encode for WasmResult {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(result ");
        self.0.encode(sink);
        sink.push(')');
    }
}

impl Encode for Local {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(local ");
        if let Some(ref name) = self.name {
            sink.push('$');
            name.encode(sink);
        }
        self.type_.encode(sink);
        sink.push(')');
    }
}

impl Encode for FuncTypeDef {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(type ");
        if let Some(ref name) = self.name {
            sink.push('$');
            name.encode(sink);
        }
        self.ty.encode(sink);
        sink.push(')');
    }
}

impl Encode for Import {
    fn encode(&self, sink: &mut String) {
        todo!()
    }
}

impl Encode for Export {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(export \"");
        self.name.encode(sink);
        sink.push('"');
        self.kind.encode(sink);
        sink.push(')');
    }
}

impl Encode for ExportKind {
    fn encode(&self, sink: &mut String) {
        sink.push('(');
        match self {
            ExportKind::Func(index) => {
                sink.push_str("func ");
                index.encode(sink);
            }
        }
        sink.push(')');
    }
}

impl Encode for Global {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(global ");
        if let Some(name) = &self.name {
            sink.push('$');
            name.encode(sink);
        }
        sink.push(' ');
        self.ty.encode(sink);
        sink.push(' ');
        self.init.encode(sink);
        sink.push(')');
    }
}

impl Encode for GlobalType {
    fn encode(&self, sink: &mut String) {
        match self.m {
            Mut::Const => self.ty.encode(sink),
            Mut::Var => {
                sink.push_str("(mut ");
                self.ty.encode(sink);
                sink.push(')');
            }
        }
    }
}

// impl Encode for Mut {
//     fn encode(&self, sink: &mut String) {
//         match self {
//             Mut::Const => sink.push_str("const"),
//             Mut::Var => sink.push_str("var"),
//         }
//     }
// }

// impl Module {
//     fn section<T: Encode>(id: SectionId, cont: T, sink: &mut String) {
//         let mut tmp = Vec::new();
//         cont.encode(&mut tmp);

//         id.encode(sink);
//         tmp.len().encode(sink);
//         sink.extend_from_slice(&tmp);
//     }

//     fn section_list<T: Encode>(id: SectionId, cont: &[T], sink: &mut String) {
//         if cont.is_empty() {
//             return;
//         }

//         Self::section(id, cont, sink);
//     }
// }

struct Lines<'a, T>(&'a [T]);
impl<'a, T: Encode> Encode for Lines<'a, T> {
    fn encode(&self, sink: &mut String) {
        for line in self.0.iter() {
            line.encode(sink);
            sink.push('\n');
        }
    }
}

impl<'a, T> From<&'a [T]> for Lines<'a, T> {
    fn from(arr: &'a [T]) -> Self {
        Self(arr)
    }
}

impl Encode for Module {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(module\n");
        Lines::from(self.types.as_slice()).encode(sink);
        Lines::from(self.funcs.as_slice()).encode(sink);
        Lines::from(self.imports.as_slice()).encode(sink);
        Lines::from(self.exports.as_slice()).encode(sink);
        Lines::from(self.globals.as_slice()).encode(sink);
        sink.push(')');
    }
}
