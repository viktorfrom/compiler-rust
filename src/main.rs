extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, multispace0},
    combinator::map,
    sequence::delimited,
    IResult,
};

#[derive(Debug)]
pub enum Tree {
    AriNode(AriOp, Box<Tree>, Box<Tree>),
    LogNode(LogOp, Box<Tree>, Box<Tree>),
    Num(i32),
    Bool(Bool),
    Id(String),
    // UnOp(Op, Box<Tree>),
    // Application(Id, vec<Tree>)
}

#[derive(Debug)]
pub enum AriOp {
    Add,
    Sub,
    Mult,
    Div,
}

#[derive(Debug)]
pub enum Bool {
    True,
    False,
}

#[derive(Debug)]
pub enum LogOp {
    And,
    Or,
}

fn parse_binOp(input: &str) -> IResult<&str, LogOp> {
    delimited(
        multispace0,
        alt((
            map(tag("&&"), |_| LogOp::And),
            map(tag("||"), |_| LogOp::Or),
        )),
        multispace0,
    )(input)
}

fn parse_bool(input: &str) -> IResult<&str, Bool> {
    delimited(
        multispace0,
        alt((
            map(tag("true"), |_| Bool::True),
            map(tag("false"), |_| Bool::False),
        )),
        multispace0,
    )(input)
}

fn parse_op(input: &str) -> IResult<&str, AriOp> {
    delimited(
        multispace0,
        alt((
            map(tag("+"), |_| AriOp::Add),
            map(tag("-"), |_| AriOp::Sub),
            map(tag("*"), |_| AriOp::Mult),
            map(tag("/"), |_| AriOp::Div),
        )),
        multispace0,
    )(input)
}

fn parse_i32(input: &str) -> IResult<&str, i32> {
    let (substring, digit) = delimited(multispace0, digit1, multispace0)(input)?;

    Ok((substring, digit.parse::<i32>().unwrap()))
}

// for interpreter
// fn eval_type(input: Tree) -> Tree {
//     match input {
//     input => Num(i32),
//     _ => println!("something else"),
// }


fn build_AriTree(string: &str) -> IResult<&str, Tree> {
    let (substring, digit) = parse_i32(string)?;
    if substring == ";" {
        return Ok(("", Tree::Num(digit)));
    } else {
        let op = parse_op(substring).unwrap().1;
        let right = parse_op(substring).unwrap().0;

        let tree = Tree::AriNode(
            op,
            Box::new(Tree::Num(digit)),
            Box::new(build_AriTree(right).unwrap().1),
        );

        return Ok(("", tree));
    }
}

fn build_LogTree(string: &str) -> IResult<&str, Tree> {
    let (substring, boolean) = parse_bool(string)?;
    if substring == ";" {
        return Ok(("", Tree::Bool(boolean)));
    } else {
        let op = parse_binOp(substring).unwrap().1;
        let right = parse_binOp(substring).unwrap().0;

        let tree = Tree::LogNode(
            op,
            Box::new(Tree::Bool(boolean)),
            Box::new(build_LogTree(right).unwrap().1),
        );

        return Ok(("", tree));
    }
}

fn parse_expr(input: &str) -> IResult<&str, Tree> {
    alt((
        build_AriTree,
        build_LogTree
    ))(input)
}
// cat(tag(")")) for att parsa "("                  ")"
fn main() {
    // let string = "        1 + 2 -1 / 5     *      3       ;";
    let string = "            true && false ||           true       ;";
    let tree = parse_expr(string);
    // println!("{:#?}", tree);
    println!("{:#?}", tree);
}
