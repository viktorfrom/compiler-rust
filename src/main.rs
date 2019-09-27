mod parser;
mod interpreter;

use crate::parser::parse_expr;
use crate::interpreter::interp_expr;

fn main() {
    // let string = "        11 + 2 -1 / (5     *      3)                 ;";
    // let string = "            true && false >>           true       ;";
    // let string = "((1 + 2) - (1 + 3))";
    // let string = "(1 + (2 - (3)))";
    // let string = "(((1) - 2) + 3)";
    let string = "1 + 2";
    // cat(tag(")")) for att parsa "("                  ")"

    let tree = interp_expr(parse_expr(string).unwrap().1);
    println!("{:#?}", tree);
}
