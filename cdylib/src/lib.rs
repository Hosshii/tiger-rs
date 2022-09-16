#![allow(unused_unsafe)]

use std::io::{Read, Write};

type TigerInt = i64;

#[repr(C)]
pub struct TigerArray {
    len: TigerInt,
    data: *mut TigerInt,
}

#[no_mangle]
pub extern "C" fn initArray(size: TigerInt) -> *mut TigerArray {
    let v: Vec<TigerInt> = vec![0; size as usize];
    let v = Box::leak(Box::new(v));

    Box::leak(Box::new(TigerArray {
        len: v.len() as TigerInt,
        data: v.as_mut_ptr(),
    }))
}

#[repr(C)]
pub struct TigerRecord {
    len: TigerInt,
    data: *mut TigerInt,
}

#[no_mangle]
pub extern "C" fn allocRecord(size: TigerInt) -> *mut TigerRecord {
    let v: Vec<TigerInt> = vec![0; size as usize];
    let v = Box::leak(Box::new(v));

    Box::leak(Box::new(TigerRecord {
        len: v.len() as TigerInt,
        data: v.as_mut_ptr(),
    }))
}

#[repr(C)]
pub struct TigerSting {
    len: TigerInt,
    data: *mut u8,
}

#[no_mangle]
pub unsafe extern "C" fn stringEqual(a: *const TigerSting, b: *const TigerSting) -> TigerInt {
    let a = unsafe { std::slice::from_raw_parts((*a).data, (*a).len as usize) };
    let b = unsafe { std::slice::from_raw_parts((*b).data, (*b).len as usize) };

    (a == b) as TigerInt
}

#[no_mangle]
pub unsafe extern "C" fn stringOrd(a: *const TigerSting, b: *const TigerSting) -> TigerInt {
    let a = unsafe { std::slice::from_raw_parts((*a).data, (*a).len as usize) };
    let b = unsafe { std::slice::from_raw_parts((*b).data, (*b).len as usize) };

    a.cmp(b) as TigerInt
}

#[no_mangle]
pub unsafe extern "C" fn print(s: *const TigerSting) {
    let s = unsafe { std::slice::from_raw_parts((*s).data, (*s).len as usize) };
    let s = std::str::from_utf8(s).unwrap();

    println!("{}", s);
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

#[no_mangle]
pub unsafe extern "C" fn ord(s: *const TigerSting) -> TigerInt {
    let s = unsafe { std::slice::from_raw_parts((*s).data, (*s).len as usize) };
    let s = std::str::from_utf8(s).unwrap();

    s.chars().next().unwrap() as TigerInt
}

#[no_mangle]
pub unsafe extern "C" fn concat(a: *const TigerSting, b: *const TigerSting) -> *mut TigerSting {
    let a = unsafe { std::slice::from_raw_parts((*a).data, (*a).len as usize) };
    let b = unsafe { std::slice::from_raw_parts((*b).data, (*b).len as usize) };

    let mut buf = Vec::new();
    buf.extend_from_slice(a);
    buf.extend_from_slice(b);
    let buf = Box::leak(Box::new(buf));

    Box::into_raw(Box::new(TigerSting {
        len: buf.len() as TigerInt,
        data: buf.as_mut_ptr(),
    }))
}
