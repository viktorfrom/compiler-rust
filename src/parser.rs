extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric0, digit1, multispace0},
    combinator::map,
    sequence::{preceded, delimited, terminated, tuple},
    IResult,
};

#[derive(Debug)]
pub enum Expr {
    Node(Box<Expr>, Box<Expr>, Box<Expr>),
    Num(i32),
    Bool(bool),
    LogicOp(LogicOp),
    ArithOp(ArithOp),
    Type(Type),
    Func(Func),
    // Id(String),
    // UnOp(Op, Box<Tree>),
    // Application(Id, vec<Tree>)
}

#[derive(Debug)]
pub enum ArithOp {
    Add,
    Sub,
    Mult,
    Div,
}

#[derive(Debug)]
pub enum LogicOp {
    And,
    Or,
    Les,
    Gre,
}

#[derive(Debug)]
pub enum Type {
    Integer,
    Bool
}

#[derive(Debug)]
pub enum Func {
    Let,
    Value,
}

fn parse_binop(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("&&"), |_| Expr::LogicOp(LogicOp::And)),
            map(tag("||"), |_| Expr::LogicOp(LogicOp::Or)),
            map(tag("<<"), |_| Expr::LogicOp(LogicOp::Les)),
            map(tag(">>"), |_| Expr::LogicOp(LogicOp::Gre)),
        )),
        multispace0,
    )(input)
}

fn parse_op(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("+"), |_| Expr::ArithOp(ArithOp::Add)),
            map(tag("-"), |_| Expr::ArithOp(ArithOp::Sub)),
            map(tag("*"), |_| Expr::ArithOp(ArithOp::Mult)),
            map(tag("/"), |_| Expr::ArithOp(ArithOp::Div)),
        )),
        multispace0,
    )(input)
}

fn parse_bool(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("true"), |_| Expr::Bool(true)),
            map(tag("false"), |_| Expr::Bool(false)),
        )),
        multispace0,
    )(input)
}

fn parse_paren(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        delimited(tag("("), parse_expr, tag(")")),
        multispace0,
    )(input)
}

fn parse_i32(input: &str) -> IResult<&str, Expr> {
    let (substring, digit) = delimited(multispace0, digit1, multispace0)(input)?;

    Ok((substring, Expr::Num(digit.parse::<i32>().unwrap())))
}

fn parse_type(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("i32"), |_| Expr::Type(Type::Integer)),
            map(tag("bool"), |_| Expr::Type(Type::Bool)),
        )),
        multispace0,
    )(input)
}

pub fn parse_expr(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(
                tuple((
                    alt((parse_paren, parse_i32, parse_bool)),
                    alt((parse_op, parse_binop, parse_type)),
                    parse_expr,
                )),
                |(left, operator, right)| {
                    Expr::Node(Box::new(left), Box::new(operator), Box::new(right))
                },
            ),
            parse_bool,
            parse_i32,
            parse_paren,
        )),
        multispace0,
    )(input)
}
