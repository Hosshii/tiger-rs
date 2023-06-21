use std::{env, fs::File, io};

use tiger::{
    Aarch64AppleDarwin, Compiler, Wasm32UnknownUnknown, WasmCompilerOption, X86_64AppleDarwin,
    X86_64LinuxGnu,
};

fn main() {
    let mut args = env::args();
    let filename = args.nth(1).expect("expect filename");
    let file = match File::open(filename.as_str()) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("cannot open file {}", filename);
            eprintln!("{}", e);
            return;
        }
    };

    let arch = if let Some(x) = args.next() {
        assert_eq!(x, "--arch");
        args.next().expect("expect arch")
    } else {
        "aarch64-apple-darwin".to_string()
    };

    let result = match arch.as_str() {
        "aarch64-apple-darwin" => {
            Compiler::new::<Aarch64AppleDarwin>(filename, file, io::stdout()).compile()
        }
        "x86_64-apple-darwin" => {
            Compiler::new::<X86_64AppleDarwin>(filename, file, io::stdout()).compile()
        }
        "x86_64-linux-gnu" => {
            Compiler::new::<X86_64LinuxGnu>(filename, file, io::stdout()).compile()
        }
        "wasm32-unknown-unknown" => {
            let is_wat = args.next().map_or(false, |x| x == "--wat");
            let options = WasmCompilerOption::new().wat(is_wat);
            Compiler::new::<Wasm32UnknownUnknown>(filename, file, io::stdout())
                .with_options(options)
                .compile()
        }
        x => panic!("unknown arch {}", x),
    };

    if let Err(err) = result {
        eprintln!("{}", err)
    }
}
