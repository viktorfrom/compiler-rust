#[macro_use]
extern crate lazy_static;

mod ast;
mod interpreter;
mod parser;

use crate::interpreter::*;

fn main() {
    // let string = " fn testfunc(arg1: i32, arg2: i32) { asd }";
    // let string = "((1 + 2) - (1 + 3))";
    // let string = "let a: i32 = 1;";
    // let string = "1 -2 +3;";
    // let string = "if true {let a: i32 = 1;return a;}";
    let string = "if true {let a:i32 =1;let b:i32 = a; return b;}";
    let tree = parser::parse_expr(string);
    // println!("Tree = {:#?}", tree);

    let expr = eval_expr(tree.unwrap().1);
    println!("Eval = {:#?}", expr);
}
