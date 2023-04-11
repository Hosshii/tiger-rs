mod codegen;
mod ir;
mod lexer;
mod parser;
mod semant;

mod asm;
mod common;
mod frame;
mod wasm;

use std::{
    io::{self, Read, Write},
    marker::PhantomData,
};

use semant::translate;
use thiserror::Error;
use wasm::{WasmEncoder, WatEncoder};

use crate::{
    codegen::{
        aarch64_apple_darwin::ARM64 as ARM64Codegen, reg_alloc,
        x86_64_apple_darwin::X86_64 as X86_64Codegen, Codegen,
    },
    frame::{Fragment, Frame as _},
    semant::Semant,
};

pub const AARCH64_APPLE_DARWIN: PhantomData<ARM64Codegen> = PhantomData;
pub const X86_64_APPLE_DARWIN: PhantomData<X86_64Codegen> = PhantomData;

pub fn compile<C, N, R, O>(
    filename: N,
    r: R,
    mut o: O,
    #[allow(unused)] arch: PhantomData<C>,
) -> Result<(), Error>
where
    N: Into<String>,
    R: Read,
    O: Write,
    C: Codegen,
{
    let ast = parser::parse(filename, r)?;

    let semantic_analyzer = Semant::new_with_base();

    let (hir, tcx) = semantic_analyzer.trans_prog(ast)?;
    let fragments = translate::translate::<C::Frame>(&tcx, &hir, C::MAIN_SYMBOL);

    writeln!(o, "{}", C::header())?;

    for fragment in fragments {
        match fragment {
            Fragment::Proc(body, frame) => {
                // dbg!(&body);
                let body = frame.borrow_mut().proc_entry_exit1(body);
                // dbg!(&body);

                let stmts = ir::linearize(body);
                // dbg!(&stmts);

                let (basic_blocks, done_label) = ir::basic_blocks(stmts);
                // dbg!(&basic_blocks);

                let stmts = ir::trace_schedule(basic_blocks, done_label);
                // dbg!(&stmts);

                let mut frame = frame.borrow().clone();
                let instructions = stmts
                    .into_iter()
                    .flat_map(|stmt| C::codegen(&frame, stmt))
                    .collect();

                let instructions = frame.proc_entry_exit2(instructions);
                let instructions = frame.proc_entry_exit3(instructions);

                let (instructions, allocation) = reg_alloc::alloc::<C>(instructions, &mut frame);
                for instruction in instructions {
                    let code = instruction.to_string::<C::Frame>(&allocation);
                    if !code.is_empty() {
                        writeln!(o, "{}", code)?;
                    }
                }
            }
            Fragment::String(label, s) => {
                let asm = C::string(&label, &s);
                writeln!(o, "{}", asm)?;
            }
        }
    }

    Ok(())
}

pub fn compile_wasm<N, R, O>(filename: N, r: R, mut o: O) -> Result<(), Error>
where
    N: Into<String>,
    R: Read,
    O: Write,
{
    let ast = parser::parse(filename, r)?;

    let semantic_analyzer = Semant::new_with_base();

    let (hir, tcx) = semantic_analyzer.trans_prog(ast)?;
    let mut module = wasm::translate(&tcx, &hir);

    let mut encoder = WasmEncoder::new();
    encoder.rewrite_module(&mut module);

    o.write_all(&encoder.encode_module(&module))?;

    Ok(())
}

pub fn compile_wat<N, R, O>(filename: N, r: R, mut o: O) -> Result<(), Error>
where
    N: Into<String>,
    R: Read,
    O: Write,
{
    let ast = parser::parse(filename, r)?;

    let semantic_analyzer = Semant::new_with_base();

    let (hir, tcx) = semantic_analyzer.trans_prog(ast)?;
    let mut module = wasm::translate(&tcx, &hir);

    let mut encoder = WatEncoder::new();
    encoder.rewrite_module(&mut module);

    o.write_all(encoder.encode_module(&module).as_bytes())?;

    Ok(())
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    SemantError(#[from] semant::Error),

    #[error("{0}")]
    ParseError(#[from] parser::Error),

    #[error("io error: {0}")]
    IoError(#[from] io::Error),
}
