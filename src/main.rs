mod parser;
mod tree;

pub use crate::parser::stringparser;
pub use crate::tree::treebuilder;


fn main() {
    let string = "1 + 2 + 1";
    
    let parsed = stringparser::try_parse(string);
    println!("{:#?}", parsed);

    // let tree = treebuilder::build_tree("+", 1, 2);
    // println!("{:#?}", tree);
}
