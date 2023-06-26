mod codegen;
mod ir;
mod lexer;
mod parser;
mod semant;

mod asm;
mod common;
mod frame;
pub mod wasm;

use std::{
    io::{self, Read, Write},
    marker::PhantomData,
};

use semant::translate;
use thiserror::Error;
use wasm::{WasmEncoder, WatEncoder};

use crate::{
    codegen::{
        aarch64_apple_darwin::ARM64 as ARM64AppleDarwinCodegen, reg_alloc,
        x86_64_apple_darwin::X86_64 as X86_64AppleDarwinCodegen,
        x86_64_linux_gnu::X86_64 as X86_64UnknownLinuxGnuCodegen, Codegen,
    },
    frame::{Fragment, Frame as _},
    semant::Semant,
};
use sealed::ArchInner;

const AARCH64_APPLE_DARWIN: PhantomData<ARM64AppleDarwinCodegen> = PhantomData;
const X86_64_APPLE_DARWIN: PhantomData<X86_64AppleDarwinCodegen> = PhantomData;
const X86_64_UNKNOWN_LINUX_GNU: PhantomData<X86_64UnknownLinuxGnuCodegen> = PhantomData;

pub struct Compiler<A, N, R, W>
where
    A: Arch,
    N: Into<String>,
    R: Read,
    W: Write,
{
    filename: N,
    r: R,
    w: W,
    options: A::Options,
}

impl<N, R, W> Compiler<Empty, N, R, W>
where
    N: Into<String>,
    R: Read,
    W: Write,
{
    pub fn new<C: Arch>(filename: N, r: R, w: W) -> Compiler<C, N, R, W> {
        Compiler {
            filename,
            r,
            w,
            options: C::Options::default(),
        }
    }
}

impl<A, N, R, W> Compiler<A, N, R, W>
where
    A: Arch,
    N: Into<String>,
    R: Read,
    W: Write,
{
    pub fn with_options(mut self, options: A::Options) -> Self {
        self.options = options;
        self
    }

    pub fn compile(self) -> Result<(), Error> {
        A::compile(self.filename, self.r, self.w, self.options)
    }
}

pub trait Arch: sealed::ArchInner {}

mod sealed {
    use std::io::{Read, Write};

    use crate::Error;

    pub trait ArchInner {
        type Options: Default;

        fn compile<N, R, W>(filename: N, r: R, w: W, options: Self::Options) -> Result<(), Error>
        where
            N: Into<String>,
            R: Read,
            W: Write;
    }
}

pub struct Empty;
impl Arch for Empty {}
impl ArchInner for Empty {
    type Options = ();

    fn compile<N, R, O>(_: N, _: R, _: O, _: Self::Options) -> Result<(), Error>
    where
        N: Into<String>,
        R: Read,
        O: Write,
    {
        unreachable!()
    }
}

pub struct Aarch64AppleDarwin;
impl Arch for Aarch64AppleDarwin {}
impl ArchInner for Aarch64AppleDarwin {
    type Options = ();

    fn compile<N, R, O>(filename: N, r: R, o: O, _: Self::Options) -> Result<(), Error>
    where
        N: Into<String>,
        R: Read,
        O: Write,
    {
        compile_unixlike(filename, r, o, AARCH64_APPLE_DARWIN)
    }
}

pub struct X86_64AppleDarwin;
impl Arch for X86_64AppleDarwin {}
impl ArchInner for X86_64AppleDarwin {
    type Options = ();

    fn compile<N, R, O>(filename: N, r: R, o: O, _: Self::Options) -> Result<(), Error>
    where
        N: Into<String>,
        R: Read,
        O: Write,
    {
        compile_unixlike(filename, r, o, X86_64_APPLE_DARWIN)
    }
}

pub struct X86_64UnknownLinuxGnu;
impl Arch for X86_64UnknownLinuxGnu {}
impl ArchInner for X86_64UnknownLinuxGnu {
    type Options = ();

    fn compile<N, R, O>(filename: N, r: R, o: O, _: Self::Options) -> Result<(), Error>
    where
        N: Into<String>,
        R: Read,
        O: Write,
    {
        compile_unixlike(filename, r, o, X86_64_UNKNOWN_LINUX_GNU)
    }
}

pub struct Wasm32UnknownUnknown;
impl Arch for Wasm32UnknownUnknown {}
impl ArchInner for Wasm32UnknownUnknown {
    type Options = WasmCompilerOption;

    fn compile<N, R, O>(filename: N, r: R, o: O, options: Self::Options) -> Result<(), Error>
    where
        N: Into<String>,
        R: Read,
        O: Write,
    {
        if options.wat {
            compile_wat(filename, r, o)
        } else {
            compile_wasm(filename, r, o)
        }
    }
}
#[derive(Debug, Default, Clone)]

pub struct WasmCompilerOption {
    wat: bool,
}

impl WasmCompilerOption {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn wat(mut self, wat: bool) -> Self {
        self.wat = wat;
        self
    }
}

fn compile_unixlike<C, N, R, O>(
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

    let (hir, tcx) = semantic_analyzer.analyze(ast)?;
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

fn compile_wasm<N, R, O>(filename: N, r: R, mut o: O) -> Result<(), Error>
where
    N: Into<String>,
    R: Read,
    O: Write,
{
    let ast = parser::parse(filename, r)?;

    let semantic_analyzer = Semant::new_with_base();

    let (hir, tcx) = semantic_analyzer.analyze(ast)?;
    let mut module = wasm::translate(&tcx, &hir);

    let mut encoder = WasmEncoder::new();
    encoder.rewrite_module(&mut module);

    o.write_all(&encoder.encode_module(&module))?;

    Ok(())
}

fn compile_wat<N, R, O>(filename: N, r: R, mut o: O) -> Result<(), Error>
where
    N: Into<String>,
    R: Read,
    O: Write,
{
    let ast = parser::parse(filename, r)?;

    let semantic_analyzer = Semant::new_with_base();

    let (hir, tcx) = semantic_analyzer.analyze(ast)?;
    let mut module = wasm::translate(&tcx, &hir);

    let mut encoder = WatEncoder::new();
    encoder.rewrite_module(&mut module);

    o.write_all(WatEncoder::encode_module(&module).as_bytes())?;

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
