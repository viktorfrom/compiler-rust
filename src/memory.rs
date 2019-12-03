use crate::ast::content_tree::*;
use crate::ast::expr_tree::*;

use std::collections::HashMap;

use lazy_static;
use std::sync::Mutex;

lazy_static! {
    static ref MEMORY: Mutex<HashMap<&'static str, Content>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
    static ref SCOPE: Mutex<Vec<Mutex<HashMap<&'static str, Content>>>> = {
        let s = Vec::new();
        Mutex::new(s)
    };
    static ref FUNCTION_MAP: Mutex<HashMap<&'static str, Vec<Vec<Expr>>>> = {
        let f = HashMap::new();
        Mutex::new(f)
    };
}

pub fn insert_function(name: Content, val: Vec<Vec<Expr>>) -> Content {
    match name {
        Content::Str(n) => {
            let mut map = FUNCTION_MAP.lock().unwrap();
            map.insert(Box::leak(n.into_boxed_str()), val);
        }
        _ => panic!("ERROR: Can't assign to var"),
    }
    // println!("hashmap = {:#?}", FUNCTION_MAP.lock().unwrap());
    return Content::Null;
}

pub fn insert_var(name: Content, val: Content) -> Content {
    match name {
        Content::Str(n) => {
            let mut map = MEMORY.lock().unwrap();
            map.insert(Box::leak(n.into_boxed_str()), val);
        }
        _ => panic!("ERROR: Can't assign to var"),
    }
    // println!("hashmap = {:#?}", MEMORY.lock().unwrap());
    return Content::Null;
}

pub fn read_from_var(var: &str) -> Content {
    let map = MEMORY.lock().unwrap();

    match map.get(&var) {
        Some(var) => match var {
            Content::Num(num) => Content::Num(*num),
            Content::Bool(b) => Content::Bool(*b),
            Content::Str(n) => Content::Str(n.to_string()),
            _ => panic!("Could not match var in HashMap"),
        },
        None => {
            panic!("ERROR: Var not found in scope");
        }
    }
}

pub fn read_from_func(key: &str) -> (&str, Vec<Vec<Expr>>) {
    let map = FUNCTION_MAP.lock().unwrap();
    let value = map.get(key);

    return (key, value.unwrap().to_vec());
}
