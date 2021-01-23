use structopt::StructOpt;

use crate::parser::*;
use crate::program::*;
use crate::type_checker::*;
use crate::{ast::*, llvm};
use crate::{interpreter::interpreter, llvm::*};

#[derive(Debug, StructOpt)]
#[structopt(
    name = "compiler",
    about = "
execute examples:
cargo run -- 
cargo run -- -l
cargo run -- -l -a
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

    let test = " fn main() -> i32 {fn testfn() -> i32 {return 1}; return testfn()} ";

    let p = program();
    let mut ast = match parser(&test) {
        Ok(res) => res,
        Err(e) => {
            panic!("Error: {:#}", e)
        }
    };

    if opt.ast {
        println!("ast = {:#?}", ast);
    }

    if type_checker(ast.clone().1) {
        if opt.llvm {
            let _res = llvm(ast.1);
        } else {
            ast.1.push(Expr::Return(Box::new(Expr::FnCall(
                Box::new(Expr::Var("main".to_string())),
                vec![],
            ))));

            let res = interpreter(ast.1);
            println!("interp:  {:#?}", res);
        }
    } else {
        panic!("ERROR: Typechecker failed!");
    }
}
