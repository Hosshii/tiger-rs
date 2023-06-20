pub type TigerInt = isize;

#[repr(C)]
pub struct TigerArray {
    pub(crate) len: TigerInt,
    pub(crate) data: *mut TigerInt,
}

impl Drop for TigerArray {
    fn drop(&mut self) {
        unsafe {
            Vec::from_raw_parts(self.data, self.len as usize, self.len as usize);
        }
    }
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
    pub(crate) len: TigerInt,
    pub(crate) data: *mut TigerInt,
}

impl Drop for TigerRecord {
    fn drop(&mut self) {
        unsafe {
            Vec::from_raw_parts(self.data, self.len as usize, self.len as usize);
        }
    }
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

// TODO: cap
#[repr(C)]
pub struct TigerSting {
    pub(crate) len: TigerInt,
    pub(crate) data: *mut u8,
}

impl From<String> for TigerSting {
    fn from(s: String) -> Self {
        let buf = s.into_bytes();
        let buf = Box::leak(Box::new(buf));

        TigerSting {
            len: buf.len() as TigerInt,
            data: buf.as_mut_ptr(),
        }
    }
}

impl Drop for TigerSting {
    fn drop(&mut self) {
        unsafe {
            Vec::from_raw_parts(self.data, self.len as usize, self.len as usize);
        }
    }
}

/// # Safety
/// `a` and `b` must be point to valid `TigerString`.
#[no_mangle]
pub unsafe extern "C" fn stringEqual(a: *const TigerSting, b: *const TigerSting) -> TigerInt {
    let a = unsafe { std::slice::from_raw_parts((*a).data, (*a).len as usize) };
    let b = unsafe { std::slice::from_raw_parts((*b).data, (*b).len as usize) };

    (a == b) as TigerInt
}

/// calculate `a op b`
/// If result is true, then return 1, otherwise return 0
/// `op` must between 1 and 4
/// op | 1  | 2 | 3  | 4
///    | <= | < | >= | >
/// # Safety
/// `a` and `b` must be point to valid `TigerString`.
#[no_mangle]
pub unsafe extern "C" fn stringOrd(
    op: TigerInt,
    a: *const TigerSting,
    b: *const TigerSting,
) -> TigerInt {
    let a = unsafe { std::slice::from_raw_parts((*a).data, (*a).len as usize) };
    let b = unsafe { std::slice::from_raw_parts((*b).data, (*b).len as usize) };

    let a = std::str::from_utf8(a).unwrap();
    let b = std::str::from_utf8(b).unwrap();

    let result = match op {
        1 => a <= b,
        2 => a < b,
        3 => a >= b,
        4 => a > b,
        _ => panic!("invalid op"),
    };

    result as TigerInt
}

/// # Safety
/// `s` must be point to valid `TigerString`.
#[no_mangle]
pub unsafe extern "C" fn ord(s: *const TigerSting) -> TigerInt {
    let s = unsafe { std::slice::from_raw_parts((*s).data, (*s).len as usize) };
    let s = std::str::from_utf8(s).unwrap();

    if let Some(s) = s.chars().next() {
        s as TigerInt
    } else {
        -1
    }
}

#[no_mangle]
pub extern "C" fn chr(i: TigerInt) -> *mut TigerSting {
    if !is_ascii(i) {
        panic!("chr: invalid ascii code {}", i);
    }

    let buf = vec![i as u8];
    let buf = Box::leak(Box::new(buf));

    Box::into_raw(Box::new(TigerSting {
        len: buf.len() as TigerInt,
        data: buf.as_mut_ptr(),
    }))
}

fn is_ascii(i: TigerInt) -> bool {
    (0x41..=0x5a).contains(&i) || (0x61..=0x7a).contains(&i) || (0x30..=0x39).contains(&i)
}

/// # Safety
/// `s` must be point to valid `TigerString`.
#[no_mangle]
pub unsafe extern "C" fn size(s: *const TigerSting) -> TigerInt {
    let s = unsafe { std::slice::from_raw_parts((*s).data, (*s).len as usize) };
    let s = std::str::from_utf8(s).unwrap();
    s.chars().count() as TigerInt
}

/// # Safety
/// `s` must be point to valid `TigerString`.
#[no_mangle]
pub unsafe extern "C" fn substring(
    s: *const TigerSting,
    first: TigerInt,
    n: TigerInt,
) -> *mut TigerSting {
    let s = unsafe { std::slice::from_raw_parts((*s).data, (*s).len as usize) };
    let s = std::str::from_utf8(s).unwrap();
    let mut s = s
        .chars()
        .skip(first as usize)
        .take(n as usize)
        .collect::<String>()
        .into_bytes();

    let p = Box::into_raw(Box::new(TigerSting {
        len: s.len() as TigerInt,
        data: s.as_mut_ptr(),
    }));
    Box::leak(Box::new(s));
    p
}

/// # Safety
/// `a` and `b` must be point to valid `TigerString`.
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

#[no_mangle]
pub extern "C" fn not(i: TigerInt) -> TigerInt {
    (i == 0) as TigerInt
}
