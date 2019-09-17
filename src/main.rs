extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, multispace0},
    combinator::map,
    sequence::preceded,
    IResult,
};

#[derive(Debug)]
pub enum Node {
    Root(Op, Box<Node>, Box<Node>),
    Leaf(i32),
}

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
}

fn parse_op(input: &str) -> IResult<&str, Op> {
    preceded(
        multispace0,
        alt((map(tag("+"), |_| Op::Add), map(tag("-"), |_| Op::Sub))),
    )(input)
}

fn parse_i32(input: &str) -> IResult<&str, i32> {
    let (substring, digit) = preceded(multispace0, digit1)(input)?;

    Ok((substring, digit.parse::<i32>().unwrap()))
}

fn build_tree(string: &str) -> IResult<&str, Node> {
    let (substring, digit) = parse_i32(string)?;
    if substring == "" {
        return Ok(("", Node::Leaf(digit)));
    } else {
        let op = parse_op(substring).unwrap().1;
        let right = parse_op(substring).unwrap().0;

        let tree = Node::Root(
            op,
            Box::new(Node::Leaf(digit)),
            Box::new(build_tree(right).unwrap().1),
        );

        return Ok(("", tree));
    }
}

fn main() {
    let string = "        1 + 2 +1";
    let tree = build_tree(string);
    // println!("{:#?}", tree);
    println!("{:#?}", tree);
}
