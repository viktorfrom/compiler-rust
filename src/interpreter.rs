use crate::parser::ArithOp;
use crate::parser::Expr;
use crate::parser::LogicOp;

#[derive(Debug)]
pub enum Content {
    Num(i32),
    ContentOp(ContentOp),
    Bool(bool),
}

#[derive(Debug)]
pub enum ContentOp {
    Add,
    Sub,
    Mult,
    Div,
    And,
    Or,
    Les,
    Gre,
}

pub fn interp_expr(input: Expr) -> Content {
    match input {
        Expr::Num(int) => Content::Num(int),
        Expr::Bool(b) => Content::Bool(b),

        Expr::ArithOp(op) => match op {
            ArithOp::Add => Content::ContentOp(ContentOp::Add),
            ArithOp::Sub => Content::ContentOp(ContentOp::Sub),
            ArithOp::Mult => Content::ContentOp(ContentOp::Mult),
            ArithOp::Div => Content::ContentOp(ContentOp::Div),
        },

        Expr::LogicOp(op) => match op {
            LogicOp::And => Content::ContentOp(ContentOp::And),
            LogicOp::Or => Content::ContentOp(ContentOp::Or),
            LogicOp::Les => Content::ContentOp(ContentOp::Les),
            LogicOp::Gre => Content::ContentOp(ContentOp::Gre),
        },

        Expr::Node(left, operator, right) => bool_eval(
            interp_expr(*left),
            interp_expr(*operator),
            interp_expr(*right),
        ),
        _ => (panic!("Invalid input!")),
    }
}

fn bool_eval(left: Content, operator: Content, right: Content) -> Content {
    let l: bool;
    let r: bool;

    match left {
        Content::Bool(b) => l = b,
        _ => panic!("Invalid input! Left leaf not of type bool!"),
    }

    match right {
        Content::Bool(b) => r = b,
        _ => panic!("Invalid input! Right leaf not of type bool!"),
    }

    match operator {
        Content::ContentOp(ContentOp::And) => Content::Bool(l && r),
        Content::ContentOp(ContentOp::Or) => Content::Bool(l || r),
        _ => panic!("Invalid input! Operator not of logic type!"),
    }
}

fn i32_eval(left: Content, operator: Content, right: Content) -> Content {
    let l: i32;
    let r: i32;

    match left {
        Content::Num(num) => l = num,
        _ => panic!("Invalid input! Left leaf NaN!"),
    }

    match right {
        Content::Num(num) => r = num,
        _ => panic!("Invalid input! Right leaf NaN!"),
    }

    match operator {
        Content::ContentOp(ContentOp::Add) => Content::Num(l + r),
        Content::ContentOp(ContentOp::Sub) => Content::Num(l - r),
        Content::ContentOp(ContentOp::Mult) => Content::Num(l * r),
        Content::ContentOp(ContentOp::Div) => Content::Num(l / r),
        _ => panic!("Invalid input! Operator not of arithmetic type!"),
    }
}
