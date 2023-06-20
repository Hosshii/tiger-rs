use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(src: String) -> Result<Vec<u8>, String> {
    let mut out = Vec::new();
    tiger::compile_wasm("main", src.as_bytes(), &mut out).map_err(|e| e.to_string())?;
    Ok(out)
}
