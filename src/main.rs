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
    // let string = "let a: i32 = 3 + 2 + 4";
    // let string = "fn testfunc(arg1: i32, arg2: i32) { sdfsdf} ";
    // let string = "a: bool";
    // let string = " if a == true {  let a: i32 =3 + 2 + 4;let a: i32 = 3 + 2 + 4;}";
    // let string = " if a == true {}";
    // let string = " if true {}";
    // let string = "a == 1";
    // let string = "input1: i32, input2: i32";
    // let string = "i32";
    // let string = "{let a: i32 =3 + 2 + 4;let a: i32 = 3 + 2 + 4;}";
    // let string = "    -> i32";
    // let string = "    while     true   { let a: i32 = 3 + 2 + 4;    }";
    let string = "    while     a == 1   { let a: i32 = 3 + 2 + 4;    }";
    let tree = parser::parse_while(string);
    println!("{:#?}", tree);

    // let tree = parse_expr(string).unwrap().1;
    // println!("{:#?}", tree);
    // let expr = interp_expr(tree);
    // println!("{:#?}", expr);
}
