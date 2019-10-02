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
    RelOp(RelOp),
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
    PluEqu,
    MinEqu,
    DivEqu,
}

#[derive(Debug)]
pub enum RelOp {
    EquEqu,
    NotEqu,
    LesEqu,
    GreEqu,
    Les, 
    Gre,
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
            map(tag("+="), |_| Expr::AssignOp(AssignOp::PluEqu)),
            map(tag("-="), |_| Expr::AssignOp(AssignOp::MinEqu)),
            map(tag("/="), |_| Expr::AssignOp(AssignOp::DivEqu)),
        )),
        multispace0,
    )(input)
}

fn parse_rel_op(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("=="), |_| Expr::RelOp(RelOp::EquEqu)),
            map(tag("!="), |_| Expr::RelOp(RelOp::NotEqu)),
            map(tag("<="), |_| Expr::RelOp(RelOp::LesEqu)),
            map(tag(">="), |_| Expr::RelOp(RelOp::GreEqu)),
            map(tag("<"), |_| Expr::RelOp(RelOp::Les)),
            map(tag(">="), |_| Expr::RelOp(RelOp::Gre)),
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

pub fn parse_if(input: &str) -> IResult<&str, Expr> {
    let (substring, (var, var_type, expr)) = delimited(
        delimited(multispace0, tag("if"), multispace0),
        tuple((
            parse_var,
            parse_rel_op,
            alt((parse_var, parse_i32)),
        )),
        multispace0,
    )(input)?;

    println!("var = {:#?}, type = {:#?}, expr = {:#?}", var, var_type, expr);

    // Ok((substring, var))
    Ok((substring, Expr::Node(Box::new(var), Box::new(var_type), Box::new(expr))))
}
pub fn parse_let(input: &str) -> IResult<&str, Expr> {
    let (substring, (var, var_type, expr)) = delimited(
        delimited(multispace0, tag("let"), multispace0),
        tuple((
            parse_var,
            preceded(tag(":"), parse_type),
            preceded(parse_assign_op, parse_expr),
        )),
        multispace0,
    )(input)?;
    // println!("var = {:#?}, type = {:#?}, expr = {:#?}", var, var_type, expr);

    // Ok((substring, var))
    Ok((substring, Expr::Node(Box::new(var), Box::new(var_type), Box::new(expr))))
}

pub fn parse_expr(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(
                tuple((
                    alt((parse_paren, parse_i32, parse_bool)),
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
        )),
        multispace0,
    )(input)
}
