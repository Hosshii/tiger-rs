use std::{env, fs::File, io};

use tiger::{AARCH64_APPLE_DARWIN, X86_64_APPLE_DARWIN};

fn main() {
    let mut args = env::args();
    let filename = args.nth(1).expect("expect filename");
    let file = File::open(filename.as_str()).unwrap();

    let arch = if let Some(x) = args.next() {
        assert_eq!(x, "--arch");
        args.next().expect("expect arch")
    } else {
        "aarch64-apple-darwin".to_string()
    };

    let result = match arch.as_str() {
        "aarch64-apple-darwin" => {
            tiger::compile(filename, file, io::stdout(), AARCH64_APPLE_DARWIN)
        }
        "x86_64-apple-darwin" => tiger::compile(filename, file, io::stdout(), X86_64_APPLE_DARWIN),
        "wasm32-unknown-unknown" => tiger::compile_wasm(filename, file, io::stdout()),
        x => panic!("unknown arch {}", x),
    };

    if let Err(err) = result {
        println!("{}", err)
    }
}
