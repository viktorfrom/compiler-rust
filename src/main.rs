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
    // let string = "if true {let a:i32 =1;let b:i32 = a; return b;}";
    // let string = "while true {let a:i32 =1;let b:i32 = a; return b;}";
    // let string = "fn test (a:i32) {let b:i32 = a;let c:i32 = b; return true;}";
    // let string = "let a = testfunc(1,2, 3);";
    let string = "fn testfunc(a:i32) {let b:i32 = a; return b;}; let c = testfunc(1);";
    // let string = "let c = testfunc(1);";


    let tree = parser::parse_expr(string);
    println!("Tree = {:#?}", tree);

    let expr = eval_scope(tree.unwrap().1);
    println!("Eval = {:#?}", expr);
}
