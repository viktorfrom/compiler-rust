extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, multispace0},
    combinator::map,
    sequence::{delimited, tuple},
    IResult,
};

#[derive(Debug)]
pub enum Expr {
    AriNode(Box<Expr>, Box<Expr>, Box<Expr>),
    Num(i32),
    Bool(Bool),
    LogOp(LogOp),
    AriOp(AriOp),
    // Id(String),
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
    Les,
    Gre,
}

fn parse_binop(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("&&"), |_| Expr::LogOp(LogOp::And)),
            map(tag("||"), |_| Expr::LogOp(LogOp::Or)),
            map(tag("<<"), |_| Expr::LogOp(LogOp::Les)),
            map(tag(">>"), |_| Expr::LogOp(LogOp::Gre)),
        )),
        multispace0,
    )(input)
}

fn parse_bool(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("true"), |_| Expr::Bool(Bool::True)),
            map(tag("false"), |_| Expr::Bool(Bool::False)),
        )),
        multispace0,
    )(input)
}

fn parse_op(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("+"), |_| Expr::AriOp(AriOp::Add)),
            map(tag("-"), |_| Expr::AriOp(AriOp::Sub)),
            map(tag("*"), |_| Expr::AriOp(AriOp::Mult)),
            map(tag("/"), |_| Expr::AriOp(AriOp::Div)),
        )),
        multispace0,
    )(input)
}

fn parse_i32(input: &str) -> IResult<&str, Expr> {
    let (substring, digit) = delimited(multispace0, digit1, multispace0)(input)?;

    Ok((substring, Expr::Num(digit.parse::<i32>().unwrap())))
}

fn parse_expr(string: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(
                tuple((
                    alt((parse_i32, parse_bool)),
                    alt((parse_op, parse_binop)),
                    parse_expr,
                )),
                |(left, op, right)| Expr::AriNode(Box::new(left), Box::new(op), Box::new(right)),
            ),
            parse_i32,
            parse_paren,
        )),
        multispace0,
    )(string)
}

fn parse_paren(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        delimited(tag("("), parse_expr, tag(")")),
        multispace0,
    )(input)
}

fn main() {
    // let string = "        11 + 2 -1 / (5     *      3)                 ;";
    // let string = "            true && false >>           true       ;";
    // let string = "((1 + 2) - (1 + 3))";
    let string = "(1 + (2 - (3)))";

    let tree = parse_expr(string);
    println!("{:#?}", tree.unwrap().1);
}
