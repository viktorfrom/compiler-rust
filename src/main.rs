#[macro_use]
extern crate lazy_static;

mod ast;
mod interpreter;
mod memory;
mod parser;
mod type_checker;

use crate::interpreter::*;
use crate::parser::*;
use crate::type_checker::*;

fn main() {
    // let string = " fn testfunc(arg1: i32, arg2: i32) { asd }";
    // let string = "((1 + 2) - (1 + 3))";
    // let string = "let a: i32 = 1;";
    // let string = "1 + 2 +3;";
    // let string = "if true {let a: i32 = 1;return a;}";
    // let string = "if true {let a:i32 =1;let b:i32 = a; return b;}";
    // let string = "while true {let a:i32 =1;let b:i32 = a; return b;}";
    // let string = "pub fn testfunc(a:i32) {let b:i32 = a; return true;}; let a = testfunc(3);";
    // let string = "let a = testfunc(1,2, 3);";
    // let string = "pub fn testfunc() {return true;}; let b: bool = testfunc();";
    // let string = "let b:i32 = 1;return 1;";


    let string = "let a: i32 = 1;";

    let tree = parse_expr(string);
    // println!("Tree = {:#?}", tree);

    if type_scope(tree.clone().unwrap().1) {
        println!("Type checker passed!");
        let expr = eval_scope(tree.clone().unwrap().1);
        println!("Eval = {:#?}", expr);
    }
}
