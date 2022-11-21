use std::fmt::Debug;

use super::{
    ast::{
        BinOp, BlockType, Export, ExportKind, Expr, Func, FuncType, FuncTypeDef, Index,
        Instruction, Local, Module, Name, NumType, Operator, Param, TypeUse, ValType, WasmResult,
    },
    rewrite::Rewriter,
};

pub struct Encoder {
    pub(crate) bytes: Vec<u8>,
}

impl Encoder {
    pub fn new() -> Self {
        Self {
            bytes: vec![
                0x00, 0x61, 0x73, 0x6D, // Magic
                0x01, 0x00, 0x00, 0x00, // Version
            ],
        }
    }

    pub fn encode_module(mut self, module: &Module) -> Vec<u8> {
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
    fn encode(&self, sink: &mut Vec<u8>);
}

impl<T: Encode + ?Sized> Encode for &'_ T {
    fn encode(&self, sink: &mut Vec<u8>) {
        (*self).encode(sink)
    }
}

/// https://webassembly.github.io/spec/core/binary/conventions.html#vectors
impl<T: Encode> Encode for [T] {
    fn encode(&self, sink: &mut Vec<u8>) {
        let len = self.len();
        len.encode(sink);
        for item in self.iter() {
            item.encode(sink);
        }
    }
}

impl<T: Encode> Encode for Vec<T> {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.as_slice().encode(sink)
    }
}

impl Encode for u8 {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(*self);
    }
}

impl Encode for usize {
    fn encode(&self, sink: &mut Vec<u8>) {
        assert!(*self <= u32::MAX as usize);
        (*self as u32).encode(sink)
    }
}

impl Encode for u32 {
    fn encode(&self, sink: &mut Vec<u8>) {
        leb128::write::unsigned(sink, *self as u64).unwrap();
    }
}

impl Encode for i32 {
    fn encode(&self, sink: &mut Vec<u8>) {
        leb128::write::signed(sink, *self as i64).unwrap();
    }
}

impl Encode for u64 {
    fn encode(&self, sink: &mut Vec<u8>) {
        leb128::write::unsigned(sink, *self).unwrap();
    }
}

impl Encode for i64 {
    fn encode(&self, sink: &mut Vec<u8>) {
        leb128::write::signed(sink, *self).unwrap();
    }
}

impl Encode for str {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.as_bytes().encode(sink);
    }
}

impl<T, U> Encode for (T, U)
where
    T: Encode,
    U: Encode,
{
    fn encode(&self, sink: &mut Vec<u8>) {
        self.0.encode(sink);
        self.1.encode(sink);
    }
}

impl Encode for Name {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.0.encode(sink);
    }
}

impl Encode for ValType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ValType::Num(num_type) => match num_type {
                NumType::I32 => sink.push(0x7F),
                NumType::I64 => sink.push(0x7E),
                NumType::F32 => sink.push(0x7D),
                NumType::F64 => sink.push(0x7C),
            },
        }
    }
}

impl Encode for BlockType {
    fn encode(&self, sink: &mut Vec<u8>) {
        if let TypeUse::Index(Index::Index(x)) = self.0 {
            i64::from(x).encode(sink);
            return;
        }

        let TypeUse::Inline(ref ty) = self.0 else {
            panic!("invalid block type");
        };

        if ty.params.is_empty() && ty.result.is_empty() {
            sink.push(0x40);
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
    fn encode(&self, sink: &mut Vec<u8>) {
        self.params.encode(sink);
        self.result.encode(sink);
    }
}

impl Encode for Expr {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Expr::Op(op) => op.encode(sink),
            Expr::OpExpr(op, exprs) => {
                for expr in exprs {
                    expr.encode(sink);
                }
                op.encode(sink);
            }
            Expr::Block(_, ty, instr) => {
                sink.push(0x02);
                ty.encode(sink);
                for ele in instr {
                    ele.encode(sink);
                }
                sink.push(0x0B);
            }
        }
    }
}

impl Encode for Instruction {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Instruction::Expr(expr) => expr.encode(sink),
            Instruction::Op(op) => op.encode(sink),
        }
    }
}

impl Encode for Operator {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Operator::GlobalGet(var) => {
                sink.push(0x23);
                var.encode(sink);
            }
            Operator::LocalGet(var) => {
                sink.push(0x20);
                var.encode(sink);
            }
            Operator::Store(num_type) => {
                match num_type {
                    NumType::I32 => sink.push(0x36),
                    NumType::I64 => sink.push(0x37),
                    NumType::F32 => sink.push(0x38),
                    NumType::F64 => sink.push(0x39),
                }
                // TODO
                32u32.encode(sink);
                0u32.encode(sink);
            }
            Operator::Load(num_type) => {
                match num_type {
                    NumType::I32 => sink.push(0x28),
                    NumType::I64 => sink.push(0x29),
                    NumType::F32 => sink.push(0x2A),
                    NumType::F64 => sink.push(0x2B),
                }
                // TODO
                32u32.encode(sink);
                0u32.encode(sink);
            }
            Operator::Bin(num_type, bin_op) => match (num_type, bin_op) {
                (NumType::I32, BinOp::Add) => sink.push(0x6A),
                (NumType::I32, BinOp::Sub) => sink.push(0x6B),
                (NumType::I64, BinOp::Add) => sink.push(0x7C),
                (NumType::I64, BinOp::Sub) => sink.push(0x7D),
                (NumType::F32, BinOp::Add) => sink.push(0x92),
                (NumType::F32, BinOp::Sub) => sink.push(0x93),
                (NumType::F64, BinOp::Add) => sink.push(0xA0),
                (NumType::F64, BinOp::Sub) => sink.push(0xA1),
            },
            Operator::Const(num_type, value) => {
                match num_type {
                    NumType::I32 => sink.push(0x41),
                    NumType::I64 => sink.push(0x42),
                    NumType::F32 => sink.push(0x43),
                    NumType::F64 => sink.push(0x44),
                }
                value.encode(sink);
            }
            Operator::Nop => {
                sink.push(0x01);
            }
            Operator::Drop => {
                sink.push(0x1A);
            }
        }
    }
}

impl Encode for Index {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Index::Index(index) => index.encode(sink),
            Index::Name(name) => panic!("unresolved name {}", name),
        }
    }
}

impl<T: Debug> Encode for TypeUse<T> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            TypeUse::Index(index) => index.encode(sink),
            TypeUse::Inline(type_) => {
                panic!("unresolved type {:?}", type_)
            }
        }
    }
}

impl Encode for Func {
    fn encode(&self, sink: &mut Vec<u8>) {
        let mut tmp = Vec::new();
        self.locals.encode(&mut tmp);
        for instr in self.instr.iter() {
            instr.encode(&mut tmp);
        }
        tmp.push(0x0b);

        tmp.len().encode(sink);
        sink.extend_from_slice(&tmp);
    }
}

impl Encode for Param {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.type_.encode(sink);
    }
}

impl Encode for WasmResult {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.0.encode(sink);
    }
}

impl Encode for Vec<Local> {
    fn encode(&self, sink: &mut Vec<u8>) {
        let v = self
            .iter()
            .enumerate()
            .map(|(i, v)| (i as u32, &v.type_))
            .collect::<Vec<_>>();
        v.encode(sink);
    }
}

impl Encode for FuncTypeDef {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(0x60);
        self.params.encode(sink);
        self.result.encode(sink);
    }
}

impl Encode for Export {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.name.encode(sink);
        self.kind.encode(sink);
    }
}

impl Encode for ExportKind {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ExportKind::Func(index) => {
                sink.push(0x00);
                index.encode(sink);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum SectionId {
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
}

impl Encode for SectionId {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(*self as u8);
    }
}

impl Module {
    fn section<T: Encode>(id: SectionId, cont: T, sink: &mut Vec<u8>) {
        let mut tmp = Vec::new();
        cont.encode(&mut tmp);

        id.encode(sink);
        tmp.len().encode(sink);
        sink.extend_from_slice(&tmp);
    }

    fn section_list<T: Encode>(id: SectionId, cont: &[T], sink: &mut Vec<u8>) {
        if cont.is_empty() {
            return;
        }

        Self::section(id, cont, sink);
    }
}

impl Encode for Module {
    fn encode(&self, sink: &mut Vec<u8>) {
        Module::section_list(SectionId::Type, self.types.as_slice(), sink);

        let func_tys = self.func.iter().map(|f| &f.ty).collect::<Vec<_>>();
        Module::section_list(SectionId::Function, func_tys.as_slice(), sink);
        Module::section_list(SectionId::Export, self.export.as_slice(), sink);

        Module::section_list(SectionId::Code, self.func.as_slice(), sink);
    }
}

#[cfg(test)]
mod tests {
    use crate::wasm::{
        ast::{InlineFuncExport, ModuleBuilder},
        MAIN_SYMBOL,
    };

    use super::*;

    fn assert_module(mut module: Module, expected: &[u8]) {
        let mut encoder = Encoder::new();

        encoder.rewrite_module(&mut module);
        let encoded = encoder.encode_module(&module);

        assert_eq!(encoded, expected);
    }

    #[test]
    fn test_fn() {
        let func = Func::new(
            Some(Name::new(MAIN_SYMBOL.to_string())),
            TypeUse::Inline(FuncType::new(
                vec![],
                vec![WasmResult::new(ValType::Num(NumType::I64))],
            )),
            None,
            vec![],
            vec![Instruction::Op(Operator::Const(NumType::I64, 10))],
        );

        let builder = ModuleBuilder::new();

        let module = builder.add_func(func).build();

        // (module
        //     (func (result i64) i64.const 10))
        let expected = vec![
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01,
            0x7e, 0x03, 0x02, 0x01, 0x00, 0x0a, 0x06, 0x01, 0x04, 0x00, 0x42, 0x0a, 0x0b,
        ];

        assert_module(module, &expected);
    }

    #[test]
    fn test_fn_export() {
        let func = Func::new(
            Some(Name::new(MAIN_SYMBOL.to_string())),
            TypeUse::Inline(FuncType::new(
                vec![],
                vec![WasmResult::new(ValType::Num(NumType::I64))],
            )),
            Some(InlineFuncExport::new(Name::new(MAIN_SYMBOL.to_string()))),
            vec![],
            vec![Instruction::Op(Operator::Const(NumType::I64, 10))],
        );

        let builder = ModuleBuilder::new();

        let module = builder.add_func(func).build();

        // (module
        //     (func (export "_start")(result i64) i64.const 10))
        let expected = vec![
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01,
            0x7e, 0x03, 0x02, 0x01, 0x00, 0x07, 0x0a, 0x01, 0x06, 0x5f, 0x73, 0x74, 0x61, 0x72,
            0x74, 0x00, 0x00, 0x0a, 0x06, 0x01, 0x04, 0x00, 0x42, 0x0a, 0x0b,
        ];

        assert_module(module, &expected);
    }
}
