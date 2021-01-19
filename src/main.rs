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
}
