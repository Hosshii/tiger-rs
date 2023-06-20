use super::{TigerInt, TigerSting};
use std::{cell::RefCell, thread::LocalKey};
use wasm_bindgen::prelude::*;

thread_local! {
    static STDOUT: RefCell<Vec<u8>> = RefCell::new(Vec::new());
    static STDIN: RefCell<Vec<u8>> = RefCell::new(Vec::new());
}

// Calling function named `exit` is not working correctry.
// So rename it to `tiger_exit`.
#[no_mangle]
pub extern "C" fn tiger_exit(i: TigerInt) -> ! {
    // TODO
    std::process::exit(i as i32)
}

fn write(s: *const TigerSting, key: &'static LocalKey<RefCell<Vec<u8>>>) {
    let s = unsafe { std::slice::from_raw_parts((*s).data, (*s).len as usize) };

    key.with(|stdio| {
        stdio.borrow_mut().extend_from_slice(s);
    });
}

fn read(key: &'static LocalKey<RefCell<Vec<u8>>>) -> *mut TigerSting {
    let mut buf = Vec::new();
    key.with(|stdio| {
        buf.extend_from_slice(&stdio.borrow());
        stdio.borrow_mut().clear();
    });

    let buf = Box::leak(Box::new(buf));

    Box::into_raw(Box::new(TigerSting {
        len: buf.len() as TigerInt,
        data: buf.as_mut_ptr(),
    }))
}

/// # Safety
/// `s` must be point to valid `TigerString`.
#[no_mangle]
pub unsafe extern "C" fn print(s: *const TigerSting) {
    write(s, &STDOUT)
}

#[no_mangle]
pub extern "C" fn flush() {}

#[no_mangle]
pub extern "C" fn getchar() -> *mut TigerSting {
    read(&STDIN)
}

#[wasm_bindgen]
pub fn write_stdin(s: String) {
    let s = s.into();
    write(&s as *const TigerSting, &STDIN)
}

#[wasm_bindgen]
pub fn read_stdout() -> String {
    let s = read(&STDOUT);
    let s = unsafe { std::slice::from_raw_parts((*s).data, (*s).len as usize) };
    let s = std::str::from_utf8(s).unwrap();
    s.to_string()
}

#[no_mangle]
pub extern "C" fn allocString(byte_size: TigerInt) -> *mut TigerSting {
    let v: Vec<u8> = vec![0; byte_size as usize];
    let v = Box::leak(Box::new(v));

    Box::leak(Box::new(TigerSting {
        len: v.len() as TigerInt,
        data: v.as_mut_ptr(),
    }))
}

#[no_mangle]
/// # Safety
/// addr must be valid
pub unsafe extern "C" fn loadi32(addr: *const i32) -> i32 {
    unsafe { core::ptr::read_unaligned(addr) }
}

#[no_mangle]
/// # Safety
/// addr must be valid
pub unsafe extern "C" fn loadi64(addr: *const i64) -> i64 {
    unsafe { core::ptr::read_unaligned(addr) }
}

#[no_mangle]
/// # Safety
/// addr must be valid
pub unsafe extern "C" fn storei32(addr: *mut i32, val: i32) {
    unsafe { core::ptr::write_unaligned(addr, val) }
}

#[no_mangle]
/// # Safety
/// addr must be valid
pub unsafe extern "C" fn storei64(addr: *mut i64, val: i64) {
    unsafe { core::ptr::write_unaligned(addr, val) }
}
