use structopt::StructOpt;

// use crate::interpreter::*;
use crate::parser::*;
use crate::program::*;
use crate::type_checker::*;
use crate::{interpreter::interpreter, llvm::*};

use crate::ast::*;
use crate::memory::*;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "compiler",
    about = "
execute examples:
cargo run -- 
cargo run -- -l
cargo run -- --help
"
)]
struct Opt {
    #[structopt(short, long, default_value = "compiler")]
    filename: String,

    #[structopt(short, long)]
    llvm: bool,
}

pub fn cli() {
    let opt = Opt::from_args();

    if opt.llvm {
        // let tree = parser(&program()).unwrap().1;
        // println!("Tree = {:#?}", tree);

        // if type_checker(tree.clone()) {
        //     let res = compiler(tree);
        //     println!("eval = {:#?}", res);
        // } else {
        //     panic!("ERROR: Typechecker failed!");
        // }
    } else {
        // let tree = parser(&program()).unwrap().1;
        // println!("Tree = {:#?}", tree);

        let eval = interpreter(vec![Expr::IfElse(
            Box::new(Expr::Bool(false)),
            vec![Expr::Return(Box::new(Expr::Int(1)))],
            vec![Expr::Return(Box::new(Expr::Int(2)))],
        )]);

        println!("eval = {:#?}", eval);

        // if type_scope(tree.clone()) {
        //     let res = interpreter(tree);
        //     println!("eval = {:#?}", res);
        // } else {
        //     panic!("ERROR: Typechecker failed!");
        // }
    }
}
