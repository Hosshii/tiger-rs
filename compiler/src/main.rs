use std::{env, fs::File, io};

use tiger::ARM64_APPLE_DARWIN;

fn main() {
    let filename = env::args().nth(1).expect("expect filename");
    let file = File::open(filename.as_str()).unwrap();

    if let Err(err) = tiger::compile(filename, file, io::stdout(), ARM64_APPLE_DARWIN) {
        println!("{}", err)
    }
}
