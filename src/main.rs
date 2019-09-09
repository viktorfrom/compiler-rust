extern crate nom;

use nom::{
    character::complete::{multispace0, digit1},
    IResult,
    sequence::{preceded},
    bytes::complete::tag,
    branch::alt,
};

fn parse_expr(string: &str) -> IResult <&str, &str> {
    let a: IResult<&str, &str> = preceded(multispace0, 
            alt((tag("+"), digit1)
            ))(string);
    return a

}

fn main() {
    let string = "   1 + 2 + 1";


    let res = parse_expr(string);
    println!("{:?}", res.unwrap().1);
}

#[derive(Debug)]
pub enum Tree {
    Root(char, Box<Tree>, Box<Tree>),
    Leaf(i32),
}

fn build_tree(operator: &str, left: i32, right: i32) -> Tree { // ska vara Box<Tree> ist for i32 vid recursion
    let tree = Tree::Root(operator.parse().unwrap(), 
                    Box::new(Tree::Leaf(left)), 
                    Box::new(Tree::Leaf(right)));
    return tree;
}