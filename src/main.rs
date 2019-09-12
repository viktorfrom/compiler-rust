extern crate nom;

use nom::{
    character::complete::{multispace0, digit1},
    IResult,
    sequence::{preceded},
    bytes::complete::tag,
    branch::alt,
};

fn parse_expr(string: &str) -> IResult <&str, &str> {
    let result: IResult<&str, &str> = preceded(multispace0, 
            alt((tag("+"), digit1)
            ))(string);
    return result
}

#[derive(Debug)]
pub enum Node {
    Root(char, Box<Node>, Box<Node>),
    Leaf(i32),
}

fn build_tree(string: &str) -> Node { 
    let result = parse_expr(string);

    if result.clone().unwrap().0.len() == 0 {
        Node::Leaf(result.clone().unwrap().1.parse::<i32>().unwrap())

    } else {
        let right = parse_expr(result.clone().unwrap().0).unwrap().0;
        let left = result.clone().unwrap().1;
        let op = parse_expr(result.clone().unwrap().0).unwrap().1;

        println!("{:#?}, {:#?}, {:#?}", left, op, right);
        let tree = Node::Root(op.parse().unwrap(), 
                        Box::new(Node::Leaf(left.parse::<i32>().unwrap())), 
                        Box::new(build_tree(right)));
        return tree;
    }
}


fn main() {
    let string = "        1 + 2 +1";
    let tree = build_tree(string);

    println!("{:#?}", tree);
}