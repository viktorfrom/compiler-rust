mod interpreter;
mod parser;

use crate::interpreter::interp_expr;
use crate::parser::parse_expr;

fn main() {
    // let string = "        11 + 2 -1 / (5     *      3)                 ;";
    // let string = "true && false";
    // let string = "((1 + 2) - (1 + 3))";
    // let string = "(1 + (2 - (3)))";
    // let string = "(((1) - 2) + 3)";
    // let string = "3 * 2";
    // cat(tag(")")) for att parsa "("                  ")"

    let string = "let a: i32 = 3 + 2 + 4";
    let tree = parser::parse_let(string);
    println!("{:#?}", tree);

    // let tree = parse_expr(string).unwrap().1;
    // println!("{:#?}", tree);
    // let expr = interp_expr(tree);
    // println!("{:#?}", expr);
}
