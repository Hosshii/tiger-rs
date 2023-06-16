use std::collections::HashMap;

use once_cell::sync::Lazy;

use super::ast::{FuncType, NumType, Param, ValType};

pub const INIT_ARRAY: &str = "initArray";
pub const LOAD_I32: &str = "loadi32";
pub const LOAD_I64: &str = "loadi64";
pub const STORE_I32: &str = "storei32";
pub const STORE_I64: &str = "storei64";

pub static BUILTIN_FUNCS: Lazy<HashMap<&'static str, FuncType>> = Lazy::new(|| {
    let builtin_funcs = [
        (
            INIT_ARRAY,
            FuncType {
                params: vec![Param::new(ValType::Num(NumType::I32), None)],
                result: vec![ValType::Num(NumType::I32).into()],
            },
        ),
        (
            LOAD_I32,
            FuncType {
                params: vec![Param::new(ValType::Num(NumType::I32), None)],
                result: vec![ValType::Num(NumType::I32).into()],
            },
        ),
        (
            LOAD_I64,
            FuncType {
                params: vec![Param::new(ValType::Num(NumType::I32), None)],
                result: vec![ValType::Num(NumType::I64).into()],
            },
        ),
        (
            STORE_I32,
            FuncType {
                params: vec![
                    Param::new(ValType::Num(NumType::I32), None),
                    Param::new(ValType::Num(NumType::I32), None),
                ],
                result: vec![],
            },
        ),
        (
            STORE_I64,
            FuncType {
                params: vec![
                    Param::new(ValType::Num(NumType::I32), None),
                    Param::new(ValType::Num(NumType::I64), None),
                ],
                result: vec![],
            },
        ),
    ];

    builtin_funcs.into_iter().collect()
});
