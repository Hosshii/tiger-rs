use std::{env, fs::File};

use tiger::{
    codegen::{arm64::ARM64, Codegen as _},
    ir::{self},
    parser,
    semant::Semant,
    Fragment,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let filename = env::args().nth(1).expect("expect filename");
    let filename = "./testcases/test67.tig".to_string();
    let file = File::open(filename.as_str())?;

    let ast = parser::parse(filename, file).map_err(|e| anyhow::format_err!("{:?}", e))?;

    dbg!(&ast);

    let semantic_analyzer = Semant::new_arm64();

    let fragments = semantic_analyzer
        .trans_prog(ast)
        .map_err(|e| anyhow::format_err!("{:?}", e))?;

    dbg!(&fragments);

    ARM64::debug();

    for fragment in fragments {
        match fragment {
            Fragment::Proc(body, frame) => {
                let stmts = ir::linearize(body);
                let (basic_blocks, done_label) = ir::basic_blocks(stmts);
                dbg!(&basic_blocks.iter().any(|v| v.is_empty()));
                dbg!(&basic_blocks);
                let stmts = ir::trace_schedule(basic_blocks, done_label);
                dbg!(&stmts);

                for stmt in stmts {
                    let instruction = ARM64::codegen(frame.borrow().clone(), stmt);
                    dbg!(instruction);
                }
            }
            Fragment::String(label, st) => {
                dbg!(label, st);
            }
        }
    }

    Ok(())
}
