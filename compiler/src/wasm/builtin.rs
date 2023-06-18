use std::collections::HashMap;

use once_cell::sync::Lazy;

use super::ast::{FuncType, NumType, Param, ValType};

pub use crate::semant::builtin::{
    BUILTIN_FUNCS as SEMANT_BUILDIN_FUNCS, CHR, CONCAT, EXIT, FLUSH, GETCHAR, NOT, ORD, PRINT,
    SIZE, SUBSTRING,
};
use crate::semant::{builtin::BUILTIN_FUNC_NAME_MAP, types::TypeId};

pub const INIT_ARRAY: &str = "initArray";
pub const LOAD_I32: &str = "loadi32";
pub const LOAD_I64: &str = "loadi64";
pub const STORE_I32: &str = "storei32";
pub const STORE_I64: &str = "storei64";
pub const ALLOC_RECORD: &str = "allocRecord";
pub const ALLOC_STRING: &str = "allocString";

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
        (
            ALLOC_RECORD,
            FuncType {
                params: vec![Param::new(ValType::Num(NumType::I32), None)],
                result: vec![ValType::Num(NumType::I32).into()],
            },
        ),
        (
            ALLOC_STRING,
            FuncType {
                params: vec![Param::new(ValType::Num(NumType::I32), None)],
                result: vec![ValType::Num(NumType::I32).into()],
            },
        ),
    ];

    let builtin_funcs = SEMANT_BUILDIN_FUNCS
        .iter()
        .map(|(name, _, args, result)| {
            let name = BUILTIN_FUNC_NAME_MAP[name];
            let args = args
                .iter()
                .map(|ty| {
                    let ty = conver_ty(*ty).unwrap();
                    Param::new(ValType::Num(ty), None)
                })
                .collect::<Vec<_>>();
            let result = conver_ty(*result)
                .iter()
                .map(|ty| ValType::Num(*ty).into())
                .collect::<Vec<_>>();
            (name, FuncType::new(args, result))
        })
        .chain(builtin_funcs)
        .collect::<Vec<_>>();

    builtin_funcs.into_iter().collect()
});

fn conver_ty(id: TypeId) -> Option<NumType> {
    match id {
        TypeId::STRING | TypeId::INT => Some(NumType::I32),
        TypeId::NIL | TypeId::UNIT => None,

        _ => panic!("not support type"),
    }
}
