use crate::ast::content_tree::*;
use crate::ast::expr_tree::*;

use crate::memory::*;

pub fn type_scope(scope: Vec<Expr>) -> bool {

    let mut res: Content = Content::Null;
    for expr in scope.iter() {
        res = type_expr(expr.clone());
        match res {
            _ => continue,
        }
    }

    println!("{:#?}", res);
    if res == Content::Null {
        return false;
    }

    return true;
}

fn type_expr(input: Expr) -> Content {
    match input {
        Expr::Num(_) => Content::Num(0),
        Expr::Bool(_) => Content::Bool(false),
        Expr::Str(_) => Content::Str("string".to_string()),

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

        Expr::Let(left, operator, right) => match *left {
            Expr::Num(left) => eval_i32(
                type_expr(Expr::Num(left)),
                type_expr(*operator),
                type_expr(*right),
            ),
            Expr::Bool(left) => eval_bool(
                type_expr(Expr::Bool(left)),
                type_expr(*operator),
                type_expr(*right),
            ),
            Expr::Str(left) => eval_let(
                type_expr(Expr::Str(left)),
                type_expr(*operator),
                type_expr(*right),
            ),
            _ => Content::Null,
        }

        _ => Content::Null,
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
        _ => Content::Null,
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
        _ => Content::Null,
    }
}

fn eval_let(left: Content, operator: Content, right: Content) -> Content {
    match (left, operator, right) {
        (Content::Str(left), Content::ContentOp(ContentOp::Integer), Content::Num(right)) => {
            insert_var(Content::Str(left), Content::Num(right))
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Integer), Content::Str(right)) => {
            insert_var(Content::Str(left), read_from_var(&right.to_string()))
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Bool), right) => {
            insert_var(Content::Str(left), right)
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Str), right) => {
            insert_var(Content::Str(left), right)
        }
        _ => Content::Null,
    }
}