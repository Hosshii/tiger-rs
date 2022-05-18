use std::{env, fs::File};

use ch3::parser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let filename = env::args().nth(1).expect("expect filename");
    let filename = "./testcases/a.tig".to_string();
    let file = File::open(filename.as_str())?;

    let ast = parser::parse(filename, file).map_err(|e| anyhow::format_err!("{:?}", e))?;

    println!("{:?}", ast);

    Ok(())
}