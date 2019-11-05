use crate::ast::content_tree::*;
use crate::ast::expr_tree::*;

use std::collections::HashMap;


use std::sync::Mutex;
use lazy_static;



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


pub fn eval_expr(input: Expr) -> Content {
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

        Expr::Return(_return_param, var) => match *var {
            Expr::Str(var) => eval_return(&var.to_string()),
            _ => panic!("asdasd"),
            }

        // Expr::While(while_param, var, block) => eval_if(eval_expr(*var), block),
        Expr::If(if_param, block) => eval_if(eval_expr(*if_param), block),

        Expr::Node(left, operator, right) => match *left {
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

fn eval_if(if_param: Content, block: Vec<Expr>) -> Content {
    match if_param {
        Content::Bool(true) => eval_block(block),
        Content::Bool(false) => Content::Null,
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
    println!("hashmap = {:#?}", MEMORY.lock().unwrap());
    return Content::Str("hej".to_string());
}

fn eval_return(var: &str) -> Content {
        // let scope = SCOPE.lock().unwrap();
        // println!("blop = {:#?}", scope);
        // match scope.last(){
            // Some(m) => {
                let map = MEMORY.lock().unwrap();

                // println!("{:#?}", map);
                match map.get(&var) {
                    Some(var) => match var {
                        Content::Num(num) => Content::Num(*num),
                        Content::Bool(b) => Content::Bool(*b),
                        // IntRep::Undefined(t) => IntRep::Undefined(*t),
                        Content::Str(n) => Content::Str(n.to_string()),
                        // IntRep::Const(val) => IntRep::Const((*val).clone()),
                        // IntRep::TypeError(e) => IntRep::TypeError(e.to_string()),
                        _ => panic!("hej"),
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
        (Content::Str(left), Content::ContentOp(ContentOp::Integer), right) => {
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
            eval_expr(Expr::Node(
                Box::new(Expr::Num(2)),
                Box::new(Expr::ArithOp(ArithOp::Mult)),
                Box::new(Expr::Num(3))
            )),
            Content::Num(6)
        );
        assert_eq!(
            eval_expr(Expr::Node(
                Box::new(Expr::Bool(true)),
                Box::new(Expr::LogicOp(LogicOp::And)),
                Box::new(Expr::Bool(false))
            )),
            Content::Bool(false)
        );
    }
}
