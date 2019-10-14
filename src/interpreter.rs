use crate::ast::expr_tree::{
    Expr::{Bool, Num, Str},
    *,
};
use std::collections::HashMap;

pub type Hashmap = HashMap<String, Expr>;

#[derive(Debug, PartialEq, Eq)]
pub enum Content {
    Num(i32),
    ContentOp(ContentOp),
    Bool(bool),
    Str(String),
    Tuple(String, i32),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ContentOp {
    Add,
    Sub,
    Mult,
    Div,

    And,
    Or,
    Not,

    Equ,
    PluEqu,
    SubEqu,
    DivEqu,

    EquEqu,
    NotEqu,
    LesEqu,
    GreEqu,
    Les,
    Gre,

    Integer,
    Bool,
    Str,
}

// pub fn interp_tree(input: Expr) -> Content {
//     let expr = interp_expr(input);

//     match expr {
//         Content::Tuple(String, i32) => {
//             let key = Content::Tuple(String, i32).1;
//         }
//         _ => panic!("Invalid input!")
//     }

//     // let hashmap = Hashmap::new();
//     // hashmap.insert(key.to_string(), value);
//     // println!("{:#?}", hashmap);

//     return expr; 
// }

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
        _ => (panic!("Invalid expr!")),
    }
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
            Content::Tuple(left, right)
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
