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
    // println!("{:#?} {:#?}", name, val);
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

pub fn assign_var(name: Content, val: Content) -> Content {
    // println!("{:#?} {:#?}", name, val);
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
    // let scope = SCOPE.lock().unwrap();
    // println!("blop = {:#?}", scope);
    // match scope.last() {
    //     Some(m) => {
    let map = MEMORY.lock().unwrap();

    // println!("{:#?}", var);
    match map.get(&var) {
        Some(var) => match var {
            Content::Num(num) => Content::Num(*num),
            Content::Bool(b) => Content::Bool(*b),
            // IntRep::Undefined(t) => IntRep::Undefined(*t),
            Content::Str(n) => Content::Str(n.to_string()),
            // IntRep::Const(val) => IntRep::Const((*val).clone()),
            // IntRep::TypeError(e) => IntRep::TypeError(e.to_string()),
            _ => panic!("Could not match var in HashMap"),
        },
        None => {
            panic!("ERROR: Var not found in scope");
        }
    }
}

pub fn read_from_func(var: &str) -> Content {
    // let scope = SCOPE.lock().unwrap();
    // println!("blop = {:#?}", scope);
    // match scope.last() {
    //     Some(m) => {
    let map = FUNCTION_MAP.lock().unwrap();

    // println!("hashmap = {:#?}", map);
    println!("var = {:#?}", var);
    println!("result from hashmap = {:#?}", map.get(var));

    return Content::Null;
}


//         None => panic!("ERROR: No scope found"),
//     }
// }
