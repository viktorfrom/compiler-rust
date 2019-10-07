mod ast;
mod interpreter;
mod parser;

fn main() {
    let string = " fn testfunc(arg1: i32, arg2: i32) { asd }";


    let tree = parser::parse_expr(string);
    println!("{:#?}", tree);

    // let tree = parse_expr(string).unwrap().1;
    // println!("{:#?}", tree);
    // let expr = interp_expr(tree);
    // println!("{:#?}", expr);
}
