use structopt::StructOpt;

// use crate::interpreter::*;
use crate::program::*;
use crate::type_checker::*;
use crate::{interpreter::interpreter, llvm::*};
use crate::{parser::*, program};

use crate::ast::*;
use crate::memory::*;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "compiler",
    about = "
execute examples:
cargo run -- 
cargo run -- -l
cargo run -- -l -t
cargo run -- --help
"
)]
struct Opt {
    #[structopt(short, long, default_value = "compiler")]
    filename: String,

    #[structopt(short, long)]
    llvm: bool,

    #[structopt(short, long)]
    tree: bool,
}

pub fn cli() {
    let opt = Opt::from_args();

    let tree = parser("fn testfn(a: i32) -> i32 { return a } return testfn(5)")
        .unwrap()
        .1;

    if opt.tree {
        println!("Tree = {:#?}", tree);
    }

    if opt.llvm {
        println!("llvm");
    } else {
        println!("interpreter");
        let res = interpreter(tree);
        println!("res = {:#?}", res);
        // if type_scope(tree.clone()) {
        //     let res = interpreter(tree);
        //     println!("eval = {:#?}", res);
        // } else {
        //     panic!("ERROR: Typechecker failed!");
        // }
    }
}
