use std::{env, fs::File};

use ch2::Lexer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = env::args().nth(1).expect("expect filename");
    let file = File::open(filename.as_str())?;

    let mut lexer = Lexer::new(filename, file);
    let tokens = match lexer.tokenize() {
        Ok(token) => token,
        Err(e) => {
            println!("{}", &e);
            return Err(e.into());
        }
    };

    tokens
        .into_iter()
        .for_each(|t| print!("{} ", t.kind.to_string()));

    Ok(())
}
