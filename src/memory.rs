use crate::ast::*;

use std::collections::HashMap;

use lazy_static;
use std::sync::Mutex;
#[derive(Debug, PartialEq, Clone)]
pub enum ExprRep {
    Int(i32),
    Var(String),
    // Var(String),
    // Const(Box<IntRep>),
    Bool(bool),
    // Function(Vec<Box<ExprTree>>, Type, ExprTree),
    // Undefined(Type),

    // TypeError(String),

    // NewLine,
    Null,
}

lazy_static! {
    static ref MEMORY: Mutex<HashMap<&'static str, ExprRep>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
    static ref SCOPE: Mutex<Vec<Mutex<HashMap<&'static str, ExprRep>>>> = {
        let s = Vec::new();
        Mutex::new(s)
    };
    static ref FUNCTION_MAP: Mutex<HashMap<&'static str, Vec<Vec<ExprRep>>>> = {
        let f = HashMap::new();
        Mutex::new(f)
    };
}

// pub fn insert_function(var: Content, func: Vec<Vec<Expr>>) -> Content {
//     match var {
//         Content::Str(v) => {
//             let mut map = FUNCTION_MAP.lock().unwrap();
//             map.insert(Box::leak(v.into_boxed_str()), func);
//         }
//         _ => panic!("ERROR: Can't func '{:?}' assign to var", func),
//     }
//     // println!("hashmap = {:#?}", FUNCTION_MAP.lock().unwrap());
//     return Content::Null;
// }

pub fn insert_var(var: ExprRep, val: ExprRep) -> ExprRep {
    match var {
        ExprRep::Var(v) => {
            let mut map = MEMORY.lock().unwrap();
            map.insert(Box::leak(v.into_boxed_str()), val);
        }
        _ => panic!("ERROR: Can't assign val '{:?}' to var", val),
    }
    // println!("hashmap = {:#?}", MEMORY.lock().unwrap());
    return ExprRep::Null;
}

pub fn read_from_var(var: &str) -> ExprRep {
    let map = MEMORY.lock().unwrap();
    // println!("map = {:#?}", map);

    match map.get(&var) {
        Some(var) => match var {
            ExprRep::Int(num) => ExprRep::Int(*num),
            ExprRep::Bool(b) => ExprRep::Bool(*b),
            ExprRep::Var(v) => ExprRep::Var(v.to_string()),
            _ => panic!("Could not match var '{:#?}' in HashMap", var),
        },
        None => {
            panic!("ERROR: Var '{:?}' not found in scope", var);
        }
    }
}

// pub fn read_from_func(key: &str) -> (&str, Vec<Vec<Expr>>) {
//     let map = FUNCTION_MAP.lock().unwrap();
//     let value = map.get(key);

//     return (key, value.unwrap().to_vec());
// }
