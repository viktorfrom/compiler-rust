extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric0, digit1, multispace0},
    combinator::map,
    sequence::{delimited, preceded, tuple},
    IResult,
};

#[derive(Debug)]
pub enum Expr {
    Node(Box<Expr>, Box<Expr>, Box<Expr>),
    Num(i32),
    Bool(bool),
    LogicOp(LogicOp),
    ArithOp(ArithOp),
    AssignOp(AssignOp),
    Type(Type),
    Str(String),
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
    Not,
}

#[derive(Debug)]
pub enum AssignOp {
    Equ,
    PluEq,
    MinEq,
    DivEq,
}

#[derive(Debug)]
pub enum Type {
    Integer,
    Bool,
}

fn parse_ari_op(input: &str) -> IResult<&str, Expr> {
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

fn parse_log_op(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("&&"), |_| Expr::LogicOp(LogicOp::And)),
            map(tag("||"), |_| Expr::LogicOp(LogicOp::Or)),
            map(tag("!"), |_| Expr::LogicOp(LogicOp::Not)),
        )),
        multispace0,
    )(input)
}

fn parse_assign_op(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("="), |_| Expr::AssignOp(AssignOp::Equ)),
            map(tag("+="), |_| Expr::AssignOp(AssignOp::PluEq)),
            map(tag("-="), |_| Expr::AssignOp(AssignOp::MinEq)),
            map(tag("/="), |_| Expr::AssignOp(AssignOp::DivEq)),
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

fn parse_var(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        map(alphanumeric0, |var: &str| Expr::Str(var.to_string())),
        multispace0,
    )(input)
}

pub fn parse_let(input: &str) -> IResult<&str, Expr> {
    let (substring, (var, _, _)) = delimited(
        preceded(multispace0, tag("let")),
        tuple((
            parse_var,
            preceded(tag(":"), parse_type),
            parse_assign_op,
        )),
        multispace0,
    )(input)?;

    Ok((substring, var))
}
pub fn get_var(input: &str) -> IResult<&str, Expr> {
    let var = parse_let(input).unwrap().1;
    Ok(("", var))
}

pub fn get_expr(input: &str) -> IResult<&str, Expr> {
    let expr = parse_let(input).unwrap().0;
    Ok(("", Expr::Str(expr.to_string())))
}

pub fn parse_expr(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(
                tuple((
                    alt((parse_paren, parse_i32, parse_bool, get_var)),
                    alt((parse_ari_op, parse_log_op, parse_type)),
                    parse_expr,
                )),
                |(left, operator, right)| {
                    Expr::Node(Box::new(left), Box::new(operator), Box::new(right))
                },
            ),
            parse_bool,
            parse_i32,
            parse_paren,
            get_expr
        )),
        multispace0,
    )(input)
}
