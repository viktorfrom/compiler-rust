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
}

pub fn eval_scope(scope: Vec<Expr>) -> Content {
    let mut res: Content = Content::Null;
    for expr in scope.iter() {
        res = eval_expr(expr.clone());
        match res {
            // Content::Return(_, _) => break,
            _ => continue,
        }
    }
    return res
}

fn eval_expr(input: Expr) -> Content {
    match input {
        Expr::Num(i) => Content::Num(i),
        Expr::Bool(b) => Content::Bool(b),
        Expr::Str(s) => Content::Str(s),

        Expr::ArithOp(op) => match op {
            ArithOp::Add => Content::ContentOp(ContentOp::Add),
            ArithOp::Sub => Content::ContentOp(ContentOp::Sub),
            ArithOp::Mult => Content::ContentOp(ContentOp::Mult),
            ArithOp::Div => Content::ContentOp(ContentOp::Div),
        },

        Expr::LogicOp(op) => match op {
            LogicOp::And => Content::ContentOp(ContentOp::And),
            LogicOp::Or => Content::ContentOp(ContentOp::Or),
            LogicOp::Not => Content::ContentOp(ContentOp::Not),
        },

        Expr::AssignOp(op) => match op {
            AssignOp::Equ => Content::ContentOp(ContentOp::Equ),
            AssignOp::PluEqu => Content::ContentOp(ContentOp::PluEqu),
            AssignOp::SubEqu => Content::ContentOp(ContentOp::SubEqu),
            AssignOp::DivEqu => Content::ContentOp(ContentOp::DivEqu),
        },

        Expr::RelOp(op) => match op {
            RelOp::EquEqu => Content::ContentOp(ContentOp::EquEqu),
            RelOp::NotEqu => Content::ContentOp(ContentOp::NotEqu),
            RelOp::LesEqu => Content::ContentOp(ContentOp::LesEqu),
            RelOp::GreEqu => Content::ContentOp(ContentOp::GreEqu),
            RelOp::Les => Content::ContentOp(ContentOp::Les),
            RelOp::Gre => Content::ContentOp(ContentOp::Gre),
        },

        Expr::Type(op) => match op {
            Type::Integer => Content::ContentOp(ContentOp::Integer),
            Type::Bool => Content::ContentOp(ContentOp::Bool),
            Type::Str => Content::ContentOp(ContentOp::Str),
        },

        Expr::Return(return_param, var) => match *var {
            Expr::Str(var) => eval_return(&return_param, &var.to_string()),
            Expr::Num(var) => Content::Num(var),
            Expr::Bool(var) => Content::Bool(var),
            _ => panic!("Invalid Input!"),
        },

        Expr::Func(func_name, params, block) => eval_func(eval_expr(*func_name), params, block),
        Expr::While(_while_param, var, block) => eval_if_while(eval_expr(*var), block),
        Expr::If(if_param, block) => eval_if_while(eval_expr(*if_param), block),

        Expr::Param(param, _param_type) => match *param {
            Expr::Str(param) => {
                assign_var(Content::Str(param.clone()), Content::Str(param.clone()))
            }
            _ => panic!("Invalid Input!"),
        },

        Expr::FuncInput(var, func_name, block) => match *func_name {
            Expr::Str(func_name) => eval_func_input(eval_expr(*var), &func_name.to_string(), block),
            _ => panic!("Invalid Input!"),
        },
        
        Expr::Let(left, operator, right) => match *left {
            Expr::Num(left) => eval_i32(
                eval_expr(Expr::Num(left)),
                eval_expr(*operator),
                eval_expr(*right),
            ),
            Expr::Bool(left) => eval_bool(
                eval_expr(Expr::Bool(left)),
                eval_expr(*operator),
                eval_expr(*right),
            ),
            Expr::Str(left) => eval_let(
                eval_expr(Expr::Str(left)),
                eval_expr(*operator),
                eval_expr(*right),
            ),
            _ => (panic!("Invalid input!")),
        },
        _ => (panic!("Invalid input!")),
    }
}

fn eval_if_while(if_param: Content, block: Vec<Expr>) -> Content {
    match if_param {
        Content::Bool(true) => eval_block(block),
        Content::Bool(false) => Content::Null,
        _ => (panic!("Invalid input!")),
    }
}

fn eval_params(params: Vec<Expr>) {
    for expr in params.iter() {
        eval_expr(expr.clone());
    }
}

fn eval_func(func_name: Content, params: Vec<Expr>, block: Vec<Expr>) -> Content {
    assign_var(func_name, Content::Str("Function name".to_string()));
    eval_params(params);
    let res = eval_block(block);
    println!("res = {:#?}", res);
    return res;
}

fn eval_func_input(var: Content, func_name: &str, block: Vec<Expr>) -> Content {
    read_from_var(func_name);
    let res = eval_block(block);

    println!("hashmap = {:#?}", MEMORY.lock().unwrap());
    match var {
        Content::Str(var) => return Content::Return(var, Box::new(res)),
        _ => (panic!("Invalid input!")),
    }
}

fn assign_var(name: Content, val: Content) -> Content {
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

fn eval_return(_return_param: &str, var: &str) -> Content {
    // assign_var(Content::Str(return_param.to_string()), Content::Str(var.to_string()));

    let value = read_from_var(var);
    return Content::Return(var.to_string(), Box::new(value));
}

fn read_from_var(var: &str) -> Content {
    // let scope = SCOPE.lock().unwrap();
    // println!("blop = {:#?}", scope);
    // match scope.last(){
    // Some(m) => {
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

// None => panic!("ERROR: No scope found"),
// }
// }

fn eval_block(block: Vec<Expr>) -> Content {
    // println!("block = {:#?}", block);
    let mut res: Content = Content::Null;
    for expr in block.iter() {
        res = eval_expr(expr.clone());
        match res {
            Content::Return(_, _) => break,
            _ => continue,
        }
    }
    return res;
}

fn eval_i32(left: Content, operator: Content, right: Content) -> Content {
    match (left, operator, right) {
        (Content::Num(left), Content::ContentOp(ContentOp::Add), Content::Num(right)) => {
            Content::Num(left + right)
        }
        (Content::Num(left), Content::ContentOp(ContentOp::Sub), Content::Num(right)) => {
            Content::Num(left - right)
        }
        (Content::Num(left), Content::ContentOp(ContentOp::Div), Content::Num(right)) => {
            Content::Num(left / right)
        }
        (Content::Num(left), Content::ContentOp(ContentOp::Mult), Content::Num(right)) => {
            Content::Num(left * right)
        }
        _ => panic!("Invalid input!"),
    }
}

fn eval_bool(left: Content, operator: Content, right: Content) -> Content {
    match (left, operator, right) {
        (Content::Bool(left), Content::ContentOp(ContentOp::And), Content::Bool(right)) => {
            Content::Bool(left && right)
        }
        (Content::Bool(left), Content::ContentOp(ContentOp::Or), Content::Bool(right)) => {
            Content::Bool(left || right)
        }
        _ => panic!("Invalid input!"),
    }
}

fn eval_let(left: Content, operator: Content, right: Content) -> Content {
    match (left, operator, right) {
        (Content::Str(left), Content::ContentOp(ContentOp::Integer), Content::Num(right)) => {
            assign_var(Content::Str(left), Content::Num(right))
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Integer), Content::Str(right)) => {
            assign_var(Content::Str(left), read_from_var(&right.to_string()))
            // assign_var(Content::Str(left), Content::Num(right))
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Bool), right) => {
            assign_var(Content::Str(left), right)
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Str), right) => {
            // assign_var(Content::Str(left), read_from_var(&right.to_string()))
            assign_var(Content::Str(left), right)
        }
        _ => panic!("Invalid input!"),
    }
}

#[cfg(test)]
mod interp_tests {
    use super::*;

    #[test]
    fn test_interp() {
        assert_eq!(eval_expr(Expr::Num(1)), Content::Num(1));
        assert_eq!(eval_expr(Expr::Bool(true)), Content::Bool(true));
    }

    #[test]
    fn test_interp_node() {
        assert_eq!(
            eval_expr(Expr::Let(
                Box::new(Expr::Num(2)),
                Box::new(Expr::ArithOp(ArithOp::Mult)),
                Box::new(Expr::Num(3))
            )),
            Content::Num(6)
        );
        assert_eq!(
            eval_expr(Expr::Let(
                Box::new(Expr::Bool(true)),
                Box::new(Expr::LogicOp(LogicOp::And)),
                Box::new(Expr::Bool(false))
            )),
            Content::Bool(false)
        );
    }
}
