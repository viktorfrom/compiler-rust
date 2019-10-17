extern crate nom;

use crate::ast::expr_tree::*;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric0, digit1, multispace0},
    combinator::map,
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

pub fn parse_expr(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            parse_func,
            parse_let,
            parse_return,
            parse_if,
            parse_right_expr,
            parse_while,
        )),
        multispace0,
    )(input)
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
            map(tag("-="), |_| Expr::AssignOp(AssignOp::SubEqu)),
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
            map(tag(">"), |_| Expr::RelOp(RelOp::Gre)),
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

fn parse_i32(input: &str) -> IResult<&str, Expr> {
    let (substring, digit) = delimited(multispace0, digit1, multispace0)(input)?;

    Ok((substring, Expr::Num(digit.parse::<i32>().unwrap())))
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

fn parse_type(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(tag("i32"), |_| Expr::Type(Type::Integer)),
            map(tag("bool"), |_| Expr::Type(Type::Bool)),
            map(tag("String"), |_| Expr::Type(Type::Str)),
        )),
        multispace0,
    )(input)
}

fn parse_paren(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        delimited(tag("("), parse_right_expr, tag(")")),
        multispace0,
    )(input)
}

pub fn parse_block(input: &str) -> IResult<&str, Vec<Expr>> {
    delimited(
        alt((tag("{"), multispace0)),
        many0(terminated(
            alt((parse_let, parse_func, parse_if)),
            terminated(tag(";"), multispace0),
        )),
        alt((tag("}"), multispace0)),
    )(input)
}

fn parse_param(input: &str) -> IResult<&str, Vec<Expr>> {
    delimited(
        multispace0,
        many0(map(
            tuple((
                terminated(parse_var, tag(":")),
                terminated(parse_type, alt((tag(","), multispace0))),
            )),
            |(arg, arg_type)| Expr::Param(Box::new(arg), Box::new(arg_type)),
        )),
        multispace0,
    )(input)
}

fn parse_return(input: &str) -> IResult<&str, Expr> {
    let (substring, (return_param, var)) = delimited(
        multispace0,
        tuple((tag("return"), parse_right_expr)),
        delimited(multispace0, tag(";"), multispace0),
    )(input)?;

    Ok((
        substring,
        Expr::Return(Box::new(return_param).to_string(), Box::new(var)),
    ))
}

fn parse_if(input: &str) -> IResult<&str, Expr> {
    let (substring, (expr, block)) = delimited(
        delimited(multispace0, tag("if"), multispace0),
        tuple((
            parse_right_expr,
            delimited(multispace0, parse_block, multispace0),
        )),
        multispace0,
    )(input)?;

    // println!("expr = {:#?}, block = {:#?}", expr, block);

    Ok((substring, Expr::If(Box::new(expr), block)))
}

fn parse_while(input: &str) -> IResult<&str, Expr> {
    let (substring, (var, var_type, block)) = delimited(
        multispace0,
        tuple((
            parse_var,
            alt((parse_bool, parse_right_expr)),
            delimited(multispace0, parse_block, multispace0),
        )),
        multispace0,
    )(input)?;

    Ok((
        substring,
        Expr::While(Box::new(var), Box::new(var_type), block),
    ))
}

fn parse_let(input: &str) -> IResult<&str, Expr> {
    let (substring, (var, var_type, expr)) = delimited(
        delimited(multispace0, tag("let"), multispace0),
        tuple((
            parse_var,
            preceded(tag(":"), parse_type),
            preceded(parse_assign_op, parse_right_expr),
        )),
        multispace0,
    )(input)?;

    Ok((
        substring,
        Expr::Node(Box::new(var), Box::new(var_type), Box::new(expr)),
    ))
}

fn parse_func(input: &str) -> IResult<&str, Expr> {
    let (substring, (func_name, params, block)) = delimited(
        delimited(
            multispace0,
            alt((
                tag("fn"),
                preceded(tag("pub"), preceded(multispace0, tag("fn"))),
            )),
            multispace0,
        ),
        tuple((
            parse_var,
            delimited(tag("("), parse_param, tag(")")),
            delimited(multispace0, parse_block, multispace0),
        )),
        multispace0,
    )(input)?;

    Ok((substring, Expr::Func(Box::new(func_name), params, block)))
}

fn parse_right_expr(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((
            map(
                tuple((
                    alt((parse_paren, parse_i32, parse_bool, parse_var)),
                    alt((parse_ari_op, parse_log_op, parse_rel_op, parse_type)),
                    parse_right_expr,
                )),
                |(left, operator, right)| {
                    Expr::Node(Box::new(left), Box::new(operator), Box::new(right))
                },
            ),
            parse_bool,
            parse_i32,
            parse_paren,
            parse_var,
        )),
        multispace0,
    )(input)
}

#[cfg(test)]
mod parse_tests {
    use super::*;

    #[test]
    fn test_parse_type() {
        assert_eq!(parse_type("i32"), Ok(("", Expr::Type(Type::Integer))));
        assert_eq!(parse_type("bool"), Ok(("", Expr::Type(Type::Bool))));
        assert_eq!(parse_type("String"), Ok(("", Expr::Type(Type::Str))));
    }

    #[test]
    fn test_parse_i32() {
        assert_eq!(parse_i32("1"), Ok(("", Expr::Num(1))));
    }

    #[test]
    fn test_right_expr_arith() {
        assert!(parse_right_expr("        11 + 2 -1 / (5     *      3)                 ;").is_ok());
        assert!(parse_right_expr("((1 + 2) - (1 + 3))").is_ok());
        assert!(parse_right_expr("((1 + 2) - (1 + 3))").is_ok());
        assert!(parse_right_expr("true && false").is_ok());
        assert!(parse_right_expr("a == 1").is_ok());
    }

    #[test]
    fn test_parse_let() {
        assert!(parse_let("let a: i32 = 3 + 2 + 4").is_ok());
    }

    #[test]
    fn test_parse_func() {
        assert!(parse_func("fn testfunc(arg1: i32, arg2: i32) { asd } ").is_ok());
    }

    #[test]
    fn test_parse_param() {
        assert!(parse_param("a: bool").is_ok());
        assert!(parse_param("input1: i32, input2: i32").is_ok());
    }

    #[test]
    fn test_parse_if() {
        assert!(
            parse_param(" if a == true {  let a: i32 =3 + 2 + 4;let a: i32 = 3 + 2 + 4;}").is_ok()
        );
        assert!(parse_param(" if a == true {}").is_ok());
        assert!(parse_param(" if true {}").is_ok());
    }

    #[test]
    fn test_parse_block() {
        assert!(parse_param("{let a: i32 =3 + 2 + 4;let a: i32 = 3 + 2 + 4;}").is_ok());
    }

    #[test]
    fn test_parse_while() {
        assert!(parse_param("    while     true   { let a: i32 = 3 + 2 + 4;    }").is_ok());
        assert!(parse_param("    while     a == 1   { let a: i32 = 3 + 2 + 4;    }").is_ok());
    }

    #[test]
    fn test_parse_return() {
        assert!(parse_param("   return     1 + 2+ 3   ;").is_ok());
    }
}
