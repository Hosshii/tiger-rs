use tiger_lib::{Compiler, Wasm32UnknownUnknown};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(src: String) -> Result<Vec<u8>, String> {
    let mut out = Vec::new();

    Compiler::new::<Wasm32UnknownUnknown>("main", src.as_bytes(), &mut out)
        .compile()
        .map_err(|e| e.to_string())?;
    Ok(out)
}
