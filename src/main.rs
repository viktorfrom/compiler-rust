#[macro_use]
extern crate lazy_static;

mod ast;
mod cli;
mod interpreter;
mod llvm;
mod memory;
mod parser;
mod program;
mod type_checker;

fn main() {
    cli::cli();

    // // Interpreter
    // // let test = "
    // //     fn testfn(c: i32) -> i32 {
    // //         let a: i32 = c;
    // //         return a;
    // //     };

    // //     fn main() -> i32 {
    // //         let b: i32 = testfn(3);
    // //         return b;
    // //     };

    // //     let res: i32 = main();
    // //     ";

    // // let tree = parse_expr(test).unwrap().1;
    // // println!("Tree = {:#?}", tree);

    // // if type_scope(tree.clone()) {
    // //     println!("Type checker passed!");
    // //     let expr = eval_scope(tree.clone());
    // //     println!("eval = {:#?}", expr);
    // // } else {
    // //     panic!("ERROR: Typechecker failed!");
    // // }

    // // LLVM

    // let tree = parse_expr(program).unwrap().1;
    // println!("Tree = {:#?}", tree);

    // if type_scope(tree.clone()) {
    //     let res = compiler(tree);
    //     println!("eval = {:#?}", res);
    // } else {
    //     panic!("ERROR: Typechecker failed!");
    // }
}
