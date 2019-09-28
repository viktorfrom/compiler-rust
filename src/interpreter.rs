use crate::parser::Expr;
use crate::parser::ArithOp;

#[derive(Debug)]
pub enum Content {
    Num(i32),
    ContentOp(ContentOp),
}

#[derive(Debug)]
pub enum ContentOp {
    Add,
    Sub,
    Mult,
    Div,
}

pub fn interp_expr(input: Expr) -> Content {
    match input {
        Expr::ArithOp(Op) => match Op {
            ArithOp::Add => Content::ContentOp(ContentOp::Add),
            ArithOp::Sub => Content::ContentOp(ContentOp::Sub),
            ArithOp::Mult => Content::ContentOp(ContentOp::Mult),
            ArithOp::Div => Content::ContentOp(ContentOp::Div),
            _ => (panic!("Invalid input!")),
        },

        Expr::Num(int) => Content::Num(int),
        Expr::Node(left, operator, right) => eval_expr(
            interp_expr(*left),
            interp_expr(*operator),
            interp_expr(*right),
        ),
        _ => (panic!("Invalid input!")),
    }
}

fn eval_expr(left: Content, operator: Content, right: Content) -> Content {
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
        _ => panic!("Invalid input!"),
    }
}
