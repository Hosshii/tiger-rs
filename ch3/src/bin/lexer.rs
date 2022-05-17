use std::{env, fs::File, io::Write};

use ch3::lexer::Lexer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = env::args().nth(1).expect("expect filename");
    // let filename = "./testcases/a.tig".to_string();
    let file = File::open(filename.as_str())?;

    let lexer = Lexer::new(filename, file);
    for tokresult in lexer {
        match tokresult {
            Ok(token) => {
                print!("{} ", token.1.to_string());
                std::io::stdout().flush().unwrap();
            }
            Err(e) => {
                println!("{}", &e);
                return Err(e.into());
            }
        }
    }

    Ok(())
}
