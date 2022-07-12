use std::{env, fs::File};

use tiger::ARM64;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = env::args().nth(1).expect("expect filename");
    let file = File::open(filename.as_str())?;

    tiger::compile(filename, file, ARM64)
}
