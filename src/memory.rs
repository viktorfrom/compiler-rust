use crate::ast::*;

use std::collections::HashMap;

use lazy_static;
use std::sync::Mutex;
#[derive(Debug, PartialEq, Clone)]
pub enum ExprRep {
    Int(i32),
    Var(String),
    Bool(bool),

    Fn(Vec<(Expr, Type)>, Type, Vec<Expr>),

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
    static ref FUNCTION_MAP: Mutex<HashMap<&'static str, ExprRep>> = {
        let f = HashMap::new();
        Mutex::new(f)
    };
}

pub fn insert_fn(var: ExprRep, func: ExprRep) -> ExprRep {
    match var {
        ExprRep::Var(v) => {
            let mut map = FUNCTION_MAP.lock().unwrap();
            map.insert(Box::leak(v.into_boxed_str()), func);
        }
        _ => panic!("Could not insert fn {:#?} into map", func),
    }
    return ExprRep::Null;
}

pub fn insert_var(key: ExprRep, val: ExprRep) -> ExprRep {
    match key {
        ExprRep::Var(v) => {
            let mut map = MEMORY.lock().unwrap();
            map.insert(Box::leak(v.into_boxed_str()), val);
        }
        _ => panic!("Could not insert key '{:?}' into map", key),
    }
    return ExprRep::Null;
}

pub fn read_var(key: &str) -> ExprRep {
    let map = MEMORY.lock().unwrap();

    match map.get(&key) {
        Some(var) => match var {
            ExprRep::Int(num) => ExprRep::Int(*num),
            ExprRep::Bool(b) => ExprRep::Bool(*b),
            ExprRep::Var(v) => ExprRep::Var(v.to_string()),
            _ => panic!("Could not find var '{:#?}' in map", var),
        },
        None => ExprRep::Null,
    }
}

pub fn read_fn(key: &str) -> ExprRep {
    let map = FUNCTION_MAP.lock().unwrap();
    match map.get(key) {
        Some(fn_expr) => fn_expr.clone(),
        None => {
            panic!("Could not find fn {:#?} in map", key)
        }
    }
}
