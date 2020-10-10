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
        fn testfn() -> i32 {
            let a: i32 = 1;
            let b: i32  = a;

            let c: i32 = b;
            return c;
        };

        let a = testfn();
        ";

    let program2 = " 
        fn testfn(b:i32) -> i32 {
            let c:i32 = b;
            return c;
        };

        let a = testfn(3);
        ";

    let program3 = " 
        fn testfn() -> i32 {
            let b: bool = true && true;
            if b {
                return 50;
            };
        };

        let a = testfn();
        ";

    let test = " 
        fn testfn() -> i32 {
            let b: bool = true && true;
            while true {
                return b;
            };
        };
        ";

    let tree = parse_expr(test).unwrap().1;
    // println!("Tree = {:#?}", tree);

    if type_scope(tree.clone()) {
        // println!("Type checker passed!");
        // let expr = eval_scope(tree.clone());
        // println!("eval = {:#?}", expr);
        let res = compiler(tree);
        println!("res = {:#?}", res);
    } else {
        panic!("ERROR: Typechecker failed!");
    }
}
