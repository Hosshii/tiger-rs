mod codegen;
mod ir;
mod lexer;
mod parser;
mod semant;

mod asm;
mod common;
mod frame;

use std::{
    io::{self, Read, Write},
    marker::PhantomData,
};

use thiserror::Error;

use crate::{
    codegen::{arm64::ARM64 as ARM64Codegen, reg_alloc, Codegen},
    frame::{Fragment, Frame as _},
    semant::Semant,
};

pub const ARM64: PhantomData<ARM64Codegen> = PhantomData;

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

    let semantic_analyzer = Semant::<C::Frame>::new_with_base();

    let fragments = semantic_analyzer.trans_prog(ast, C::MAIN_SYMBOL)?;

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

                let frame = frame.borrow().clone();
                let instructions = stmts
                    .into_iter()
                    .flat_map(|stmt| C::codegen(&frame, stmt))
                    .collect();

                let instructions = frame.proc_entry_exit2(instructions);
                let instructions = frame.proc_entry_exit3(instructions);

                let (instructions, allocation) = reg_alloc::alloc(instructions, &frame);
                for instruction in instructions {
                    let code = instruction.to_string::<C::Frame>(&allocation);
                    if !code.is_empty() {
                        writeln!(o, "{}", code)?;
                    }
                }
            }
            Fragment::String(label, st) => {
                dbg!(label, st);
            }
        }
    }

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
