mod codegen;
mod ir;
mod lexer;
mod parser;
mod semant;

mod asm;
mod common;
mod frame;

use std::{io::Read, marker::PhantomData};

use parser::ast::Program;

use crate::{
    codegen::{arm64::ARM64 as ARM64Codegen, reg_alloc, Codegen},
    frame::{Fragment, Frame as _},
    semant::Semant,
};

pub const ARM64: PhantomData<ARM64Codegen> = PhantomData;

pub fn compile<C, N, R>(
    filename: N,
    r: R,
    #[allow(unused)] arch: PhantomData<C>,
) -> Result<(), Box<dyn std::error::Error>>
where
    C: Codegen,
    R: Read,
    N: Into<String>,
{
    let ast = parser::parse(filename, r).map_err(|e| anyhow::format_err!("{:?}", e))?;

    let semantic_analyzer = Semant::<C::Frame>::new_with_base();

    let fragments = semantic_analyzer
        .trans_prog(ast, C::MAIN_SYMBOL)
        .map_err(|e| anyhow::format_err!("{:?}", e))?;

    ARM64Codegen::debug();
    for fragment in fragments {
        match fragment {
            Fragment::Proc(body, frame) => {
                let body = frame.borrow_mut().proc_entry_exit1(body);

                let stmts = ir::linearize(body);
                let (basic_blocks, done_label) = ir::basic_blocks(stmts);
                let stmts = ir::trace_schedule(basic_blocks, done_label);

                let frame = frame.borrow().clone();
                for stmt in stmts {
                    let instructions = C::codegen(&frame, stmt);
                    let instructions = frame.proc_entry_exit2(instructions);
                    let instructions = frame.proc_entry_exit3(instructions);

                    let (instructions, allocation) = reg_alloc::alloc(instructions, &frame);
                    for instruction in instructions {
                        println!("{}", instruction.to_string::<C::Frame>(&allocation));
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

// fn add_runtime_main(ast: Program) -> Program {
//     // convert Expr into
//     // let
//     //   function __main() Expr
//     // in
//     //   __main()
//     // end
// }
