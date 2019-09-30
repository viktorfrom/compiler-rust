mod interpreter;
mod parser;

use crate::interpreter::interp_expr;
use crate::parser::parse_expr;

fn main() {
    // let string = "        11 + 2 -1 / (5     *      3)                 ;";
    let string = "true && false";
    // let string = "((1 + 2) - (1 + 3))";
    // let string = "(1 + (2 - (3)))";
    // let string = "(((1) - 2) + 3)";
    // let string = "3 * 2";
    // cat(tag(")")) for att parsa "("                  ")"

    // let tree = parser::parse_expr(string);
    // println!("{:#?}", tree);

    let tree = parse_expr(string).unwrap().1;
    println!("{:#?}", tree);
    let expr = interp_expr(tree);
    println!("{:#?}", expr);
}
