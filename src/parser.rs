extern crate nom;
use crate::ast::*;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric0, digit1, multispace0},
    combinator::map,
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

pub fn parser(input: &str) -> IResult<&str, Vec<Expr>> {
    many0(delimited(
        multispace0,
        alt((
            // parse_func,
            //         parse_let_func,
            //         parse_let,
            parse_return,
            //         parse_if,
            parse_bin_expr,
            //         parse_while,
        )),
        alt((tag(";"), multispace0)),
    ))(input)
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

fn parse_op(input: &str) -> IResult<&str, Op> {
    delimited(
        multispace0,
        alt((parse_ari_op, parse_log_op, parse_ass_op, parse_rel_op)),
        multispace0,
    )(input)
}

fn parse_ari_op(input: &str) -> IResult<&str, Op> {
    delimited(
        multispace0,
        alt((
            map(tag("+"), |_| Op::AriOp(AriOp::Add)),
            map(tag("-"), |_| Op::AriOp(AriOp::Sub)),
            map(tag("*"), |_| Op::AriOp(AriOp::Mul)),
            map(tag("/"), |_| Op::AriOp(AriOp::Div)),
        )),
        multispace0,
    )(input)
}

fn parse_ass_op(input: &str) -> IResult<&str, Op> {
    delimited(
        multispace0,
        alt((
            map(tag("="), |_| Op::AssOp(AssOp::Equ)),
            map(tag("+="), |_| Op::AssOp(AssOp::PluEqu)),
            map(tag("-="), |_| Op::AssOp(AssOp::SubEqu)),
            map(tag("/="), |_| Op::AssOp(AssOp::DivEqu)),
            map(tag("*="), |_| Op::AssOp(AssOp::MulEqu)),
        )),
        multispace0,
    )(input)
}

fn parse_log_op(input: &str) -> IResult<&str, Op> {
    delimited(
        multispace0,
        alt((
            map(tag("&&"), |_| Op::LogOp(LogOp::And)),
            map(tag("||"), |_| Op::LogOp(LogOp::Or)),
            // map(tag("!"), |_| Expr::LogicOp(LogOp::Not)),
        )),
        multispace0,
    )(input)
}

fn parse_rel_op(input: &str) -> IResult<&str, Op> {
    delimited(
        multispace0,
        alt((
            map(tag("=="), |_| Op::RelOp(RelOp::Eq)),
            map(tag("!="), |_| Op::RelOp(RelOp::Neq)),
            map(tag("<="), |_| Op::RelOp(RelOp::Leq)),
            map(tag(">="), |_| Op::RelOp(RelOp::Geq)),
            map(tag("<"), |_| Op::RelOp(RelOp::Les)),
            map(tag(">"), |_| Op::RelOp(RelOp::Gre)),
        )),
        multispace0,
    )(input)
}

fn parse_bin_expr(input: &str) -> IResult<&str, Expr> {
    alt((
        map(
            tuple((
                alt((
                    parse_bool,
                    parse_i32,
                    parse_paren,
                    // parse_fn_call,
                    parse_var,
                )),
                parse_op,
                parse_bin_expr,
            )),
            |(left, op, right)| Expr::BinOp(Box::new(left), op, Box::new(right)),
        ),
        parse_bool,
        parse_i32,
        parse_paren,
        // parse_fn_call,
        parse_var,
    ))(input)
}

fn parse_return(input: &str) -> IResult<&str, Expr> {
    let (substring, val) = delimited(
        multispace0,
        preceded(tag("return"), parse_bin_expr),
        multispace0,
    )(input)?;

    Ok((substring, Expr::Return(Box::new(val))))
}

fn parse_paren(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        delimited(tag("("), parse_bin_expr, tag(")")),
        multispace0,
    )(input)
}

fn parse_var(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        map(alphanumeric0, |var: &str| Expr::Var(var.to_string())),
        multispace0,
    )(input)
}

#[cfg(test)]
mod parse_tests {
    use super::*;

    #[test]
    fn test_parse_i32() {
        assert_eq!(parse_i32("1"), Ok(("", Expr::Num(1))));
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse_bool("false"), Ok(("", Expr::Bool(false))));
        assert_eq!(parse_bool("true"), Ok(("", Expr::Bool(true))));
    }

    #[test]
    fn test_parse_op() {
        assert_eq!(parse_op("+"), Ok(("", Op::AriOp(AriOp::Add))));
        assert_eq!(parse_op("="), Ok(("", Op::AssOp(AssOp::Equ))));
        assert_eq!(parse_op("&&"), Ok(("", Op::LogOp(LogOp::And))));
        assert_eq!(parse_op(">"), Ok(("", Op::RelOp(RelOp::Gre))));
    }

    #[test]
    fn test_parse_ari_op() {
        assert_eq!(parse_ari_op("+"), Ok(("", Op::AriOp(AriOp::Add))));
        assert_eq!(parse_ari_op("-"), Ok(("", Op::AriOp(AriOp::Sub))));
        assert_eq!(parse_ari_op("*"), Ok(("", Op::AriOp(AriOp::Mul))));
        assert_eq!(parse_ari_op("/"), Ok(("", Op::AriOp(AriOp::Div))));
    }

    #[test]
    fn test_parse_ass_op() {
        assert_eq!(parse_ass_op("="), Ok(("", Op::AssOp(AssOp::Equ))));
        assert_eq!(parse_ass_op("+="), Ok(("", Op::AssOp(AssOp::PluEqu))));
        assert_eq!(parse_ass_op("/="), Ok(("", Op::AssOp(AssOp::DivEqu))));
        assert_eq!(parse_ass_op("*="), Ok(("", Op::AssOp(AssOp::MulEqu))));
    }

    #[test]
    fn test_parse_log_op() {
        assert_eq!(parse_log_op("&&"), Ok(("", Op::LogOp(LogOp::And))));
        assert_eq!(parse_log_op("||"), Ok(("", Op::LogOp(LogOp::Or))));
    }

    #[test]
    fn test_parse_rel_op() {
        assert_eq!(parse_rel_op("=="), Ok(("", Op::RelOp(RelOp::Eq))));
        assert_eq!(parse_rel_op("!="), Ok(("", Op::RelOp(RelOp::Neq))));
        assert_eq!(parse_rel_op("<"), Ok(("", Op::RelOp(RelOp::Les))));
        assert_eq!(parse_rel_op(">"), Ok(("", Op::RelOp(RelOp::Gre))));
        assert_eq!(parse_rel_op("<="), Ok(("", Op::RelOp(RelOp::Leq))));
        assert_eq!(parse_rel_op(">="), Ok(("", Op::RelOp(RelOp::Geq))));
    }

    #[test]
    fn test_parse_bin_expr() {
        assert_eq!(parse_bin_expr("false"), Ok(("", Expr::Bool(false))));
        assert_eq!(parse_bin_expr("1"), Ok(("", Expr::Num(1))));
        assert_eq!(parse_bin_expr("(1)"), Ok(("", Expr::Num(1))));
        // assert_eq!(parse_bin_expr("+"), Ok(("", Expr::AriOp(AriOp::Add))));
    }

    #[test]
    fn test_parse_return() {
        assert_eq!(parse_return("return true"), Ok(("", Expr::Return(Box::new(Expr::Bool(true))))));
        assert_eq!(parse_return("return false"), Ok(("", Expr::Return(Box::new(Expr::Bool(false))))));
    }

    #[test]
    fn test_parse_paren() {
        assert_eq!(parse_paren("(1)"), Ok(("", Expr::Num(1))));
    }

    #[test]
    fn test_parse_var() {
        assert_eq!(parse_var("a"), Ok(("", Expr::Var("a".to_string()))));
    }
}
