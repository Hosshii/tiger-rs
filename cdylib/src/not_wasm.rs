use super::{TigerInt, TigerSting};
use std::io::{Read, Write};

// Calling function named `exit` is not working correctry.
// So rename it to `tiger_exit`.
#[no_mangle]
pub extern "C" fn tiger_exit(i: TigerInt) -> ! {
    std::process::exit(i as i32)
}

/// # Safety
/// `s` must be point to valid `TigerString`.
#[no_mangle]
pub unsafe extern "C" fn print(s: *const TigerSting) {
    let s = unsafe { std::slice::from_raw_parts((*s).data, (*s).len as usize) };
    let s = std::str::from_utf8(s).unwrap();

    print!("{}", s);
}

#[no_mangle]
pub extern "C" fn flush() {
    std::io::stdout().flush().unwrap();
}

#[no_mangle]
pub extern "C" fn getchar() -> *mut TigerSting {
    let mut buf = Vec::new();
    std::io::stdin().read_to_end(&mut buf).unwrap();
    let buf = Box::leak(Box::new(buf));

    Box::into_raw(Box::new(TigerSting {
        len: buf.len() as TigerInt,
        data: buf.as_mut_ptr(),
    }))
}
