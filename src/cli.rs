use structopt::StructOpt;

use crate::interpreter::*;
use crate::program::*;
use crate::type_checker::*;
use crate::{interpreter::interpreter, llvm::*};
use crate::{parser::*, program};


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
    ast: bool,
}

pub fn cli() {
    let opt = Opt::from_args();

    let test = "
        fn testfn(a:bool, b:bool) -> bool {
            let f:bool = a && b;
            return f
        }
        return testfn(true, true)
    ";
   let ast = match parser(test) {
       Ok(res) => {res}
       Err(e) => {panic!("Error: {:#}", e)}
   };

    if opt.ast {
        println!("Tree = {:#?}", ast);
    }


    if opt.llvm {
        println!("llvm");
    } else {
        println!("interpreter");
        let res = interpreter(ast.1);
        println!("res = {:#?}", res);
        // if type_scope(tree.clone()) {
        //     let res = interpreter(tree);
        //     println!("eval = {:#?}", res);
        // } else {
        //     panic!("ERROR: Typechecker failed!");
        // }
    }
}
