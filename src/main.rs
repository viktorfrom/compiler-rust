mod ast;
mod interpreter;
mod parser;

use crate::interpreter::*;

fn main() {
    // let string = " fn testfunc(arg1: i32, arg2: i32) { asd }";
    // let string = "((1 + 2) - (1 + 3))";
    let string = "let a: i32 = 1 +2 ;";
    // let string = "1 +2 +3;";

    let tree = parser::parse_expr(string);
    // println!("Tree = {:#?}", tree);

    let expr = interp_expr(tree.unwrap().1);
    println!("Interp = {:#?}", expr);
}
