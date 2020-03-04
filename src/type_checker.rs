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
    
    return true;
}

fn type_expr(input: Expr) -> Content {
    match input {
        Expr::Num(_) => Content::Num(0),
        Expr::Bool(_) => Content::Bool(false),

        _ => (panic!("Invalid input!")),
    }
}