mod common;
pub use common::*;

#[cfg(target_arch = "wasm32")]
mod wasm;
#[cfg(target_arch = "wasm32")]
pub use wasm::*;

#[cfg(not(target_arch = "wasm32"))]
mod not_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub use not_wasm::*;
