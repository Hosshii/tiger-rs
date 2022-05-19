use std::{env, fs::File};

use tiger::parser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = env::args().nth(1).expect("expect filename");
    let file = File::open(filename.as_str())?;

    let ast = parser::parse(filename, file).map_err(|e| anyhow::format_err!("{}", e))?;

    println!("{:#?}", ast);

    Ok(())
}
