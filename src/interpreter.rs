use crate::ast::content_tree::*;
use crate::ast::expr_tree::*;
use crate::content;

pub fn interp_expr(input: Expr) -> Content {
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

        Expr::Return(return_param, var) => match *return_param {
            _ => interp_expr(*var),
        },

        // Expr::While(while_param, var, _block) => eval_while(interp_expr(*var), block),
        Expr::If(if_param, block) => eval_if(interp_expr(*if_param), block),

        Expr::Node(left, operator, right) => match *left {
            Expr::Num(left) => eval_i32(
                interp_expr(Expr::Num(left)),
                interp_expr(*operator),
                interp_expr(*right),
            ),
            Expr::Bool(left) => eval_bool(
                interp_expr(Expr::Bool(left)),
                interp_expr(*operator),
                interp_expr(*right),
            ),
            Expr::Str(left) => eval_let(
                interp_expr(Expr::Str(left)),
                interp_expr(*operator),
                interp_expr(*right),
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

fn eval_block(block: Vec<Expr>) -> Content {
    use std::collections::HashMap;
    pub type Scope<T> = HashMap<String, T>;
    let mut scope: Scope<Content> = HashMap::new();
    // let mut result: Content = Content::Null;

    for expr in block.iter() {
        let result = interp_expr(expr.clone());
        println!("block_item = {:#?}", result);

        let (key, value) = match result {
            Content::Tuple(left, right) => (left, *right),
            _ => continue,
        };
        // println!("key = {}, value = {:#?}", key, value);
        scope.insert(key, value);
        let val = scope.entry("a".to_string());
        // println!("val!!! {:#?}", val);
    }

    for (key, value) in &scope {
        println!("test!!! {}: \"{:#?}\"", key, value);
    }
    let val = scope.entry("a".to_string());
    println!("val!!! {:#?}", val);
    return Content::Num(1);
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
            Content::Tuple(left, Box::new(right))
        }
        _ => panic!("Invalid input!"),
    }
}

#[cfg(test)]
mod interp_tests {
    use super::*;

    #[test]
    fn test_interp() {
        assert_eq!(interp_expr(Expr::Num(1)), Content::Num(1));
        assert_eq!(interp_expr(Expr::Bool(true)), Content::Bool(true));
    }

    #[test]
    fn test_interp_node() {
        assert_eq!(
            interp_expr(Expr::Node(
                Box::new(Expr::Num(2)),
                Box::new(Expr::ArithOp(ArithOp::Mult)),
                Box::new(Expr::Num(3))
            )),
            Content::Num(6)
        );
        assert_eq!(
            interp_expr(Expr::Node(
                Box::new(Expr::Bool(true)),
                Box::new(Expr::LogicOp(LogicOp::And)),
                Box::new(Expr::Bool(false))
            )),
            Content::Bool(false)
        );
    }
}
