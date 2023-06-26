use std::{env, fs::File, io, path::PathBuf, str::FromStr};

use anyhow::{Context, Result};
use clap::Parser;
use tiger_lib::{
    Aarch64AppleDarwin, Compiler, Wasm32UnknownUnknown, WasmCompilerOption, X86_64AppleDarwin,
    X86_64UnknownLinuxGnu,
};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Specify the target architecture for the code generation.
    /// If not provided, the default is the target architecture of the machine running the compiler.
    /// Valid options include: 'aarch64-apple-darwin', 'x86_64-apple-darwin', 'x86_64-linux-gnu', and 'wasm32-unknown-unknown'.
    #[arg(short, long, default_value = env!("TARGET_TRIPLE"))]
    arch: Arch,

    /// If set, generates '.wat' files instead of '.wasm' files.
    /// This option is only applicable when 'arch' is set to 'wasm32-unknown-unknown'.
    #[clap(short, long, requires_if("wasm32-unknown-unknown", "arch"))]
    wat: bool,

    /// Path to the source code file.
    #[arg(value_name = "FILE")]
    src: PathBuf,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Arch {
    Aarch64AppleDarwin,
    X86_64AppleDarwin,
    X86_64UnknownLinuxGnu,
    Wasm32UnknownUnknown,
}

impl FromStr for Arch {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "aarch64-apple-darwin" => Ok(Arch::Aarch64AppleDarwin),
            "x86_64-apple-darwin" => Ok(Arch::X86_64AppleDarwin),
            "x86_64-unknown-linux-gnu" => Ok(Arch::X86_64UnknownLinuxGnu),
            "wasm32-unknown-unknown" => Ok(Arch::Wasm32UnknownUnknown),
            _ => Err(format!("unknown arch {}", s)),
        }
    }
}

fn main() -> Result<()> {
    let args = Args::parse();

    let filepath = args.src;
    let file = File::open(filepath.as_path())
        .with_context(|| format!("failed to open file {}", filepath.display()))?;

    let filename = filepath
        .file_name()
        .with_context(|| format!("failed to get filename from path {}", filepath.display()))?;
    let filename = filename
        .to_str()
        .with_context(|| format!("failed to convert filename {:?} to str", filename))?
        .to_string();

    match args.arch {
        Arch::Aarch64AppleDarwin => {
            Compiler::new::<Aarch64AppleDarwin>(&filename, file, io::stdout()).compile()
        }
        Arch::X86_64AppleDarwin => {
            Compiler::new::<X86_64AppleDarwin>(&filename, file, io::stdout()).compile()
        }
        Arch::X86_64UnknownLinuxGnu => {
            Compiler::new::<X86_64UnknownLinuxGnu>(&filename, file, io::stdout()).compile()
        }
        Arch::Wasm32UnknownUnknown => {
            let options = WasmCompilerOption::new().wat(args.wat);
            Compiler::new::<Wasm32UnknownUnknown>(&filename, file, io::stdout())
                .with_options(options)
                .compile()
        }
    }
    .with_context(|| format!("failed to compile {}", filename))
}
