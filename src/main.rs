#[macro_use]
extern crate lazy_static;

mod ast;
mod interpreter;
mod llvm;
mod memory;
mod parser;
mod type_checker;

use crate::interpreter::*;
use crate::llvm::*;
use crate::parser::*;
use crate::type_checker::*;

fn main() {
    let program1 = " 
        fn main() -> i32 {
            let a: i32 = 1;
            let b: i32  = a;

            let c: i32 = b;
            return c;

        };";

    let program2 = " 
        pub fn testfn(b:i32) -> i32 {
            return b;
        };

        let a = testfn(3);
        ";

    let program3 = "     
        pub fn testfn() -> i32 {
            let b: bool = true && true;
            if b {
                return 50;
            };
        };

        let a = testfn();
        ";

    let tree = parse_expr(program1).unwrap().1;
    // println!("Tree = {:#?}", tree);

    if type_scope(tree.clone()) {
        println!("Type checker passed!");
        compiler(tree);
        // let expr = eval_scope(tree.clone());
        //println!("{:#?}", interpreter::run(ast.clone()));
    } else {
        panic!("ERROR: Typechecker failed!");
    }
}
