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
    // Interpreter
    // let test = "
    //     fn testfn(c: i32) -> i32 {
    //         let a: i32 = c;
    //         return a;
    //     };

    //     fn main() -> i32 {
    //         let b: i32 = testfn(3);
    //         return b;
    //     };

    //     let res: i32 = main();
    //     ";

    // let tree = parse_expr(test).unwrap().1;
    // println!("Tree = {:#?}", tree);

    // if type_scope(tree.clone()) {
    //     println!("Type checker passed!");
    //     let expr = eval_scope(tree.clone());
    //     println!("eval = {:#?}", expr);
    // } else {
    //     panic!("ERROR: Typechecker failed!");
    // }

    // LLVM
    let program1 = " 
        fn testfn() -> i32 {
            let a: i32 = 1;
            let b: i32 = a;
            let c: i32 = b;
            return c;
        };
        ";

    let program2 = " 
        fn testfn(c: i32) -> i32 {
            let a: i32 = c;
            return a;
        };

        fn main() -> i32 {
            let b: i32 = testfn(3);
            return b;
        };

        let res: i32 = main();
        ";

    let program3 = " 
        fn testfn() -> i32 {
            let b: bool = true && true;
            if b {
                return 50;
            };
            return 1;
        };
        ";

    let program4 = " 
        fn testfn2() -> i32 {
            return 2;
        };

        fn testfn3() -> i32 {
            let b: bool = true && true;
            if b {
                return 50;
            };
            return 1;
        };

        fn testfn() -> i32 {
            let a: i32 = testfn2(); 
            let b: i32 = testfn3();
            return b;
        };
        ";


    let tree = parse_expr(program4).unwrap().1;
    println!("Tree = {:#?}", tree);

    if type_scope(tree.clone()) {
        let res = compiler(tree);
        println!("eval = {:#?}", res);
    } else {
        panic!("ERROR: Typechecker failed!");
    }
}
