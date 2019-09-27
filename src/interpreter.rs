
use crate::parser::Expr;

#[derive(Debug)]
pub enum Content {
    Num(i32),
    Add,
}

pub fn interp_expr(input: Expr) -> Content {

    match input {
        Expr::AriOp(Add) => Content::Add,
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
        _ => panic!("Invalid input!"),
    }

    match right {
        Content::Num(num) => r = num,
        _ => panic!("Invalid input!"),
    }

    match operator {
        Content::Add => Content::Num(l + r),
        _ => panic!("Invalid input!"),
    }
}