use std::{env, fs::File};

use tiger::{
    parser::{self, ast::Program},
    semant::Semant,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = env::args().nth(1).expect("expect filename");
    let file = File::open(filename.as_str())?;

    let ast = parser::parse(filename, file).map_err(|e| anyhow::format_err!("{}", e))?;

    let semantic_analyzer = Semant::new_x86();

    match ast {
        Program::Expr(e) => match semantic_analyzer.trans_prog(e) {
            Ok(_) => println!("success!"),
            Err(e) => println!("fail! {}", e),
        },
        Program::Decls(_) => panic!(),
    }

    Ok(())
}
