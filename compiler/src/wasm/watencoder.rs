use std::fmt::Debug;

use super::{
    ast::{
        BinOp, BlockType, CvtOp, Export, ExportKind, Expr, Func, FuncType, FuncTypeDef, Global,
        GlobalType, Import, ImportKind, Index, InlineFuncExport, Instruction, Limits, Local,
        Memory, Module, Mut, Name, NumType, Operator, Param, TestOp, TypeUse, ValType, WasmResult,
    },
    rewrite::Rewriter,
};

pub struct Line {
    indent: usize,
    bytes: String,
    width: usize,
}

impl Line {
    pub fn new(width: usize) -> Self {
        Self {
            indent: 0,
            bytes: String::new(),
            width,
        }
    }

    pub fn push(&mut self, ch: char) {
        self.bytes.push(ch);
    }

    pub fn push_str(&mut self, s: &str) {
        self.bytes.push_str(s);
    }

    pub fn pop(&mut self) {
        self.bytes.pop();
    }

    pub fn newline(&mut self) {
        self.bytes.push('\n');
        self.bytes.push_str(" ".repeat(self.indent).as_str());
    }

    pub fn block<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.indent += self.width;
        self.newline();
        f(self);
        self.indent -= self.width;
    }
}

#[derive(Debug, Default)]
pub struct Encoder {}

impl Encoder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn encode_module(module: &Module) -> String {
        let mut line = Line::new(2);
        module.encode(&mut line);
        line.bytes
    }

    /// Remove syntax sugar.
    pub fn rewrite_module(&mut self, module: &mut Module) {
        let mut rewriter = Rewriter::new();
        rewriter.rewrite(module);
    }
}

struct MultiLine<'a, T>(&'a [T]);

impl<'a, T> MultiLine<'a, T> {
    pub fn new(item: &'a [T]) -> Self {
        Self(item)
    }
}

impl<'a, T> From<&'a [T]> for MultiLine<'a, T> {
    fn from(item: &'a [T]) -> Self {
        Self(item)
    }
}

impl<T: Encode> Encode for MultiLine<'_, T> {
    fn encode(&self, sink: &mut Line) {
        let len = self.0.len();
        for (idx, item) in self.0.iter().enumerate() {
            item.encode(sink);
            if idx < len - 1 {
                sink.newline();
            }
        }
    }
}

pub trait Encode {
    fn encode(&self, sink: &mut Line);
}

impl<T: Encode + ?Sized> Encode for &'_ T {
    fn encode(&self, sink: &mut Line) {
        (*self).encode(sink)
    }
}

/// https://webassembly.github.io/spec/core/binary/conventions.html#vectors
impl<T: Encode> Encode for [T] {
    fn encode(&self, sink: &mut Line) {
        for item in self.iter() {
            // sink.push('(');
            item.encode(sink);
            // sink.push(')');
            sink.push(' ');
        }
    }
}

impl<T: Encode> Encode for Vec<T> {
    fn encode(&self, sink: &mut Line) {
        self.as_slice().encode(sink)
    }
}

impl Encode for u8 {
    fn encode(&self, sink: &mut Line) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for usize {
    fn encode(&self, sink: &mut Line) {
        assert!(*self <= u32::MAX as usize);
        (*self as u32).encode(sink)
    }
}

impl Encode for u32 {
    fn encode(&self, sink: &mut Line) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for i32 {
    fn encode(&self, sink: &mut Line) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for u64 {
    fn encode(&self, sink: &mut Line) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for i64 {
    fn encode(&self, sink: &mut Line) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for str {
    fn encode(&self, sink: &mut Line) {
        sink.push_str(self);
    }
}

impl<T, U> Encode for (T, U)
where
    T: Encode,
    U: Encode,
{
    fn encode(&self, sink: &mut Line) {
        self.0.encode(sink);
        self.1.encode(sink);
    }
}

impl Encode for Name {
    fn encode(&self, sink: &mut Line) {
        self.0.encode(sink);
    }
}

impl Encode for NumType {
    fn encode(&self, sink: &mut Line) {
        match self {
            NumType::I32 => sink.push_str("i32"),
            NumType::I64 => sink.push_str("i64"),
            NumType::F32 => sink.push_str("f32"),
            NumType::F64 => sink.push_str("f64"),
        }
    }
}

impl Encode for ValType {
    fn encode(&self, sink: &mut Line) {
        match self {
            ValType::Num(num_type) => num_type.encode(sink),
        }
    }
}

impl Encode for BlockType {
    fn encode(&self, sink: &mut Line) {
        if let TypeUse::Index(Index::Index(_)) = self.0 {
            self.0.encode(sink);
            return;
        }

        let TypeUse::Inline(ref ty) = self.0 else {
            panic!("invalid block type");
        };

        if ty.params.is_empty() && ty.result.is_empty() {
            return;
        }

        if ty.params.is_empty() && ty.result.len() == 1 {
            ty.result[0].encode(sink);
            return;
        }

        panic!("multi-value block should have index");
    }
}

impl Encode for FuncType {
    fn encode(&self, sink: &mut Line) {
        self.params.encode(sink);
        self.result.encode(sink);
    }
}

impl Encode for BinOp {
    fn encode(&self, sink: &mut Line) {
        match self {
            BinOp::Add => sink.push_str("add"),
            BinOp::Sub => sink.push_str("sub"),
            BinOp::Mul => sink.push_str("mul"),
            BinOp::DivSigned => sink.push_str("div_s"),
            BinOp::Eq => sink.push_str("eq"),
            BinOp::Ne => sink.push_str("ne"),
            BinOp::LessThanSigned => sink.push_str("lt_s"),
            BinOp::LessOrEqualSigned => sink.push_str("le_s"),
            BinOp::GreaterThanSigned => sink.push_str("gt_s"),
            BinOp::GreaterOrEqualSigned => sink.push_str("ge_s"),
        }
    }
}

impl Encode for TestOp {
    fn encode(&self, sink: &mut Line) {
        match self {
            TestOp::Eqz => sink.push_str("eqz"),
        }
    }
}

impl Encode for CvtOp {
    fn encode(&self, sink: &mut Line) {
        match self {
            CvtOp::Wrap => sink.push_str("wrap"),
        }
    }
}

impl Encode for Expr {
    fn encode(&self, sink: &mut Line) {
        sink.push('(');
        match self {
            Expr::Comment(comment, expr) => {
                sink.pop();
                sink.newline();
                sink.push_str("(; ");
                comment.encode(sink);
                sink.push_str(";)");
                sink.newline();
                expr.encode(sink);

                // dont push last ')'
                return;
            }
            Expr::Op(op) => op.encode(sink),
            Expr::OpExpr(op, exprs) => {
                op.encode(sink);
                sink.push(' ');
                sink.block(|sink| MultiLine::new(exprs).encode(sink))
            }
            Expr::Block(name, ty, instrs) => {
                sink.push_str("block ");
                if let Some(name) = name {
                    sink.push('$');
                    name.encode(sink);
                    sink.push(' ');
                }
                ty.encode(sink);
                sink.block(|sink| MultiLine::new(instrs).encode(sink));
            }
            Expr::If(name, block_ty, cond, then, els) => {
                sink.push_str("if ");
                if let Some(name) = name {
                    sink.push('$');
                    name.encode(sink);
                    sink.push(' ');
                }
                block_ty.encode(sink);
                sink.block(|sink| cond.encode(sink));
                sink.block(|sink| then.encode(sink));
                if let Some(els) = els {
                    sink.block(|sink| els.encode(sink));
                }
            }
            Expr::Loop(name, block_ty, instr) => {
                sink.push_str("loop ");
                if let Some(name) = name {
                    sink.push('$');
                    name.encode(sink);
                    sink.push(' ');
                }
                block_ty.encode(sink);
                sink.block(|sink| MultiLine::new(instr).encode(sink));
            }
        }
        sink.push(')');
    }
}

impl Encode for Instruction {
    fn encode(&self, sink: &mut Line) {
        match self {
            Instruction::Expr(expr) => expr.encode(sink),
            Instruction::Op(op) => op.encode(sink),
        }
    }
}

impl Encode for Operator {
    fn encode(&self, sink: &mut Line) {
        match self {
            Operator::Br(index) => {
                sink.push_str("br ");
                index.encode(sink);
            }
            Operator::BrIf(index) => {
                sink.push_str("br_if ");
                index.encode(sink);
            }
            Operator::Call(index) => {
                sink.push_str("call ");
                index.encode(sink);
            }
            Operator::GlobalGet(var) => {
                sink.push_str("global.get ");
                var.encode(sink);
            }
            Operator::GlobalSet(var) => {
                sink.push_str("global.set ");
                var.encode(sink);
            }
            Operator::LocalGet(var) => {
                sink.push_str("local.get ");
                var.encode(sink);
            }
            Operator::LocalSet(var) => {
                sink.push_str("local.set ");
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
            Operator::Test(num_type, test_op) => {
                num_type.encode(sink);
                sink.push('.');
                test_op.encode(sink);
            }
            Operator::Const(num_type, value) => {
                num_type.encode(sink);
                sink.push_str(".const ");
                value.encode(sink);
            }
            Operator::Convert(t1, t2, op, sign) => {
                t1.encode(sink);
                sink.push('.');
                op.encode(sink);
                sink.push('_');
                t2.encode(sink);
                if sign.is_some() {
                    sink.push_str("_s");
                }
            }
            Operator::Nop => {
                todo!()
            }
            Operator::Drop => {
                sink.push_str("drop");
            }
        }
    }
}

impl Encode for Index {
    fn encode(&self, sink: &mut Line) {
        match self {
            Index::Index(index) => index.encode(sink),
            Index::Name(name) => {
                sink.push('$');
                name.encode(sink)
            }
        }
    }
}

impl<T: Debug + Encode> Encode for TypeUse<T> {
    fn encode(&self, sink: &mut Line) {
        match self {
            TypeUse::Index(index) => {
                sink.push_str("(type ");
                index.encode(sink);
                sink.push(')');
            }
            TypeUse::Inline(type_) => {
                type_.encode(sink);
            }
        }
    }
}

impl Encode for InlineFuncExport {
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(export \"");
        self.name.encode(sink);
        sink.push_str("\")");
    }
}

impl Encode for Func {
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(func ");
        if let Some(ref name) = self.name {
            sink.push('$');
            name.encode(sink);
            sink.push(' ');
        }

        if let Some(ref export) = self.export {
            export.encode(sink);
        }
        sink.push(' ');
        self.ty.encode(sink);
        sink.push(' ');
        self.locals.encode(sink);
        sink.push(' ');
        MultiLine::new(self.instr.as_slice()).encode(sink);
        sink.push(')');
    }
}

impl Encode for Param {
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(param ");
        if let Some(ref name) = self.name {
            sink.push('$');
            name.encode(sink);
            sink.push(' ');
        }
        self.type_.encode(sink);
        sink.push(')');
    }
}

impl Encode for WasmResult {
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(result ");
        self.0.encode(sink);
        sink.push(')');
    }
}

impl Encode for Local {
    fn encode(&self, sink: &mut Line) {
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
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(type ");
        if let Some(ref name) = self.name {
            sink.push('$');
            name.encode(sink);
        }
        sink.push_str("(func ");
        self.ty.encode(sink);
        sink.push_str("))");
    }
}

impl Encode for Import {
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(import \"");
        self.module.encode(sink);
        sink.push_str("\" \"");
        self.name.encode(sink);
        sink.push_str("\" ");
        self.kind.encode(sink);
        sink.push(')');
    }
}

impl Encode for ImportKind {
    fn encode(&self, sink: &mut Line) {
        match self {
            ImportKind::Func(_, index) => {
                sink.push_str("(func ");
                index.encode(sink);
                sink.push(')');
            }
        }
    }
}

impl Encode for Export {
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(export \"");
        self.name.encode(sink);
        sink.push_str("\" ");
        self.kind.encode(sink);
        sink.push(')');
    }
}

impl Encode for ExportKind {
    fn encode(&self, sink: &mut Line) {
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
    fn encode(&self, sink: &mut Line) {
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
    fn encode(&self, sink: &mut Line) {
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

impl Encode for Limits {
    fn encode(&self, sink: &mut Line) {
        match self.max {
            Some(max) => {
                sink.push('(');
                self.min.encode(sink);
                sink.push(' ');
                max.encode(sink);
                sink.push(')');
            }
            None => self.min.encode(sink),
        }
    }
}

impl Encode for Memory {
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(memory ");
        if let Some(name) = &self.name {
            sink.push('$');
            name.encode(sink);
        }
        sink.push(' ');
        self.ty.encode(sink);
        sink.push(')');
    }
}

impl Encode for Module {
    fn encode(&self, sink: &mut Line) {
        sink.push_str("(module");
        sink.block(|sink| {
            MultiLine::from(self.types.as_slice()).encode(sink);
            sink.newline();
            MultiLine::from(self.imports.as_slice()).encode(sink);
            sink.newline();
            MultiLine::from(self.funcs.as_slice()).encode(sink);
            sink.newline();
            MultiLine::from(self.exports.as_slice()).encode(sink);
            sink.newline();
            MultiLine::from(self.globals.as_slice()).encode(sink);
            sink.newline();
            MultiLine::from(self.memories.as_slice()).encode(sink);
        });
        sink.newline();
        sink.push(')');
    }
}
