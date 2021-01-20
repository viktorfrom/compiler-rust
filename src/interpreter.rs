use core::panic;

use crate::ast::*;
use crate::memory::*;

pub fn interpreter(ast: Vec<Expr>) -> ExprRep {
    let mut res = ExprRep::Null;
    for expr in ast.iter() {
        res = eval_expr(expr.clone());
        match res {
            _ => continue,
        }
    }
    return res;
}

fn eval_expr(expr: Expr) -> ExprRep {
    match expr {
        Expr::Int(i) => ExprRep::Int(i),
        Expr::Bool(b) => ExprRep::Bool(b),
        Expr::Var(n) => read_var(&n),

        Expr::BinExpr(l, op, r) => eval_bin_expr(*l, op, *r),
        Expr::VarExpr(var, op, expr) => eval_var_expr(*var, op, *expr),

        Expr::Let(var, var_type, expr) => eval_let(*var, var_type, *expr),

        Expr::If(cond, block) => eval_if(*cond, block),
        Expr::IfElse(cond, block1, block2) => eval_if_else(*cond, block1, block2),
        Expr::While(cond, block) => eval_while(*cond, block),

        Expr::Fn(fn_var, params, ret_type, block) => eval_fn(*fn_var, params, ret_type, block),
        Expr::FnCall(fn_var, args) => eval_fn_call(*fn_var, args),
        Expr::Return(expr) => eval_return(*expr),
    }
}

fn eval_fn(fn_var: Expr, params: Vec<(Expr, Type)>, ret_type: Type, block: Vec<Expr>) -> ExprRep {
    match fn_var {
        Expr::Var(a) => {
            insert_fn(
                ExprRep::Var(a.to_string()),
                ExprRep::Fn(params, ret_type, block),
            );
        }
        _ => panic!("Fn stmt fail!"),
    }
    return ExprRep::Null;
}

fn eval_fn_call(fn_var: Expr, args: Vec<Expr>) -> ExprRep {
    match fn_var {
        Expr::Var(fn_var) => match read_fn(&fn_var) {
            ExprRep::Fn(params, ret_type, block) => {
                if params.len() != args.clone().len() {
                    panic!("params != args")
                }

                for i in 0..params.len() {
                    for x in params.clone() {
                        match &x {
                            (Expr::Var(v), t) => {
                                let eval_arg = eval_expr(args[i].clone());
                                match (t, eval_arg.clone()) {
                                    (Type::Int, ExprRep::Int(_)) => {
                                        insert_var(ExprRep::Var(v.to_string()), eval_arg)
                                    }
                                    (Type::Bool, ExprRep::Bool(_)) => {
                                        insert_var(ExprRep::Var(v.to_string()), eval_arg)
                                    }
                                    _ => panic!("Return type does not match!"),
                                };
                            }
                            _ => panic!("Invalid param var!"),
                        }
                    }
                }

                let res = interpreter(block);
                match (ret_type, res.clone()) {
                    (Type::Int, ExprRep::Int(_)) => res,
                    (Type::Bool, ExprRep::Bool(_)) => res,
                    _ => panic!("Return type does not match!"),
                }
            }
            _ => panic!("Could not find fn_var in map!"),
        },
        _ => panic!("Invalid fn_var!"),
    }
}

fn eval_if(cond: Expr, block: Vec<Expr>) -> ExprRep {
    match eval_expr(cond) {
        ExprRep::Bool(c) => {
            if c {
                return interpreter(block);
            }
            return ExprRep::Null;
        }
        _ => panic!("If stmt fail!"),
    }
}

fn eval_if_else(cond: Expr, block1: Vec<Expr>, block2: Vec<Expr>) -> ExprRep {
    match eval_expr(cond) {
        ExprRep::Bool(c) => {
            if c {
                return interpreter(block1);
            } else {
                return interpreter(block2);
            }
        }
        _ => panic!("IfElse stmt fail!"),
    }
}

fn eval_while(cond: Expr, block: Vec<Expr>) -> ExprRep {
    match eval_expr(cond) {
        ExprRep::Bool(c) => {
            if c {
                return interpreter(block);
            }
            return ExprRep::Null;
        }
        _ => panic!("While stmt fail!"),
    }
}

fn eval_let(var: Expr, _var_type: Type, expr: Expr) -> ExprRep {
    match (var, eval_expr(expr)) {
        (Expr::Var(v), ExprRep::Int(val)) => insert_var(ExprRep::Var(v), ExprRep::Int(val)),
        (Expr::Var(v), ExprRep::Bool(val)) => insert_var(ExprRep::Var(v), ExprRep::Bool(val)),
        _ => panic!("Invalid let expr!"),
    }
}

fn eval_return(expr: Expr) -> ExprRep {
    return eval_expr(expr);
}

fn eval_bin_expr(l: Expr, op: Op, r: Expr) -> ExprRep {
    match (eval_expr(l), eval_expr(r.clone())) {
        (ExprRep::Int(left), ExprRep::Int(right)) => eval_int_expr(left, op, right),
        (ExprRep::Var(v), ExprRep::Int(right)) => match read_var(&v) {
            ExprRep::Int(val) => eval_int_expr(val, op, right),
            _ => ExprRep::Int(right),
        },
        (ExprRep::Bool(left), ExprRep::Bool(right)) => eval_bool_expr(left, op, right),
        (ExprRep::Var(v), ExprRep::Bool(right)) => match read_var(&v) {
            ExprRep::Bool(val) => eval_bool_expr(val, op, right),
            _ => ExprRep::Bool(right),
        },
        (ExprRep::Null, _) => eval_expr(r),
        _ => panic!("Invalid bin expr!"),
    }
}

/// Updates existing value in memory
fn eval_var_expr(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match op {
        Op::AriOp(_) => var_ari_op(var, op, expr),
        Op::AssOp(_) => var_ass_op(var, op, expr),
        Op::LogOp(_) => var_log_op(var, op, expr),
        Op::RelOp(_) => var_rel_op(var, op, expr),
    }
}

fn eval_int_expr(l: i32, op: Op, r: i32) -> ExprRep {
    match op {
        Op::AriOp(AriOp::Add) => ExprRep::Int(l + r),
        Op::AriOp(AriOp::Sub) => ExprRep::Int(l - r),
        Op::AriOp(AriOp::Div) => ExprRep::Int(l / r),
        Op::AriOp(AriOp::Mul) => ExprRep::Int(l * r),
        Op::RelOp(RelOp::Eq) => ExprRep::Bool(l == r),
        Op::RelOp(RelOp::Neq) => ExprRep::Bool(l != r),
        Op::RelOp(RelOp::Leq) => ExprRep::Bool(l <= r),
        Op::RelOp(RelOp::Geq) => ExprRep::Bool(l >= r),
        Op::RelOp(RelOp::Les) => ExprRep::Bool(l < r),
        Op::RelOp(RelOp::Gre) => ExprRep::Bool(l > r),
        _ => panic!("Invalid Int expr!"),
    }
}
fn eval_bool_expr(l: bool, op: Op, r: bool) -> ExprRep {
    match op {
        Op::LogOp(LogOp::And) => ExprRep::Bool(l && r),
        Op::LogOp(LogOp::Or) => ExprRep::Bool(l || r),
        Op::RelOp(RelOp::Eq) => ExprRep::Bool(l == r),
        Op::RelOp(RelOp::Neq) => ExprRep::Bool(l != r),
        Op::RelOp(RelOp::Leq) => ExprRep::Bool(l <= r),
        Op::RelOp(RelOp::Geq) => ExprRep::Bool(l >= r),
        Op::RelOp(RelOp::Les) => ExprRep::Bool(l < r),
        Op::RelOp(RelOp::Gre) => ExprRep::Bool(l > r),
        _ => panic!("Invalid Bool expr!"),
    }
}

fn var_ari_op(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match (var, op, expr) {
        (Expr::Var(v), op, Expr::Var(expr)) => match (read_var(&v), read_var(&expr)) {
            (ExprRep::Int(v1), ExprRep::Int(v2)) => eval_int_expr(v1, op, v2),
            _ => panic!("Var(Int) Var(Int) op fail!"),
        },
        (Expr::Var(v), op, Expr::Int(expr)) => match read_var(&v) {
            ExprRep::Int(v1) => eval_int_expr(v1, op, expr),
            _ => panic!("Var(Int) Int op fail!"),
        },
        _ => panic!("Invalid Var Log op!"),
    }
}

fn var_ass_op(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match (var.clone(), op, eval_expr(expr)) {
        (Expr::Var(v), Op::AssOp(AssOp::Eq), ExprRep::Int(val)) => {
            insert_var(ExprRep::Var(v), ExprRep::Int(val))
        }
        (Expr::Var(v), Op::AssOp(AssOp::Eq), ExprRep::Bool(val)) => {
            insert_var(ExprRep::Var(v), ExprRep::Bool(val))
        }
        (Expr::Var(v), Op::AssOp(AssOp::AddEq), ExprRep::Int(new_val)) => match eval_expr(var) {
            ExprRep::Int(old_val) => insert_var(ExprRep::Var(v), ExprRep::Int(old_val + new_val)),
            _ => panic!("Var Add update fail!"),
        },
        (Expr::Var(v), Op::AssOp(AssOp::SubEq), ExprRep::Int(new_val)) => match eval_expr(var) {
            ExprRep::Int(old_val) => insert_var(ExprRep::Var(v), ExprRep::Int(old_val - new_val)),
            _ => panic!("Var Sub update fail!"),
        },
        (Expr::Var(v), Op::AssOp(AssOp::DivEq), ExprRep::Int(new_val)) => match eval_expr(var) {
            ExprRep::Int(old_val) => insert_var(ExprRep::Var(v), ExprRep::Int(old_val / new_val)),
            _ => panic!("Var Div update fail!"),
        },
        (Expr::Var(v), Op::AssOp(AssOp::MulEq), ExprRep::Int(new_val)) => match eval_expr(var) {
            ExprRep::Int(old_val) => insert_var(ExprRep::Var(v), ExprRep::Int(old_val * new_val)),
            _ => panic!("Var Mul update fail!"),
        },
        _ => panic!("Var update fail!"),
    }
}

fn var_log_op(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match (eval_expr(var), op, eval_expr(expr)) {
        (ExprRep::Bool(b1), op, ExprRep::Bool(b2)) => eval_bool_expr(b1, op, b2),
        _ => panic!("Invalid Var Log op!"),
    }
}

fn var_rel_op(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match (eval_expr(var), op, eval_expr(expr)) {
        (ExprRep::Bool(b1), op, ExprRep::Bool(b2)) => eval_bool_expr(b1, op, b2),
        (ExprRep::Int(b1), op, ExprRep::Int(b2)) => eval_int_expr(b1, op, b2),
        _ => panic!("Invalid Var Log op!"),
    }
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;

    #[test]
    fn test_eval_int() {
        assert_eq!(interpreter(vec![Expr::Int(1)]), ExprRep::Int(1));
    }

    #[test]
    fn test_eval_bool() {
        assert_eq!(interpreter(vec![Expr::Bool(true)]), ExprRep::Bool(true));
        assert_eq!(interpreter(vec![Expr::Bool(false)]), ExprRep::Bool(false));
        assert_ne!(interpreter(vec![Expr::Bool(false)]), ExprRep::Bool(true));
    }

    #[test]
    fn test_eval_var() {
        insert_var(ExprRep::Var("a1".to_string()), ExprRep::Int(1));
        assert_eq!(
            interpreter(vec![Expr::Var("a1".to_string())]),
            ExprRep::Int(1)
        );
        insert_var(ExprRep::Var("a2".to_string()), ExprRep::Bool(true));
        assert_eq!(
            interpreter(vec![Expr::Var("a2".to_string())]),
            ExprRep::Bool(true)
        );
        insert_var(
            ExprRep::Var("a3".to_string()),
            ExprRep::Var("a4".to_string()),
        );
        assert_eq!(
            interpreter(vec![Expr::Var("a3".to_string())]),
            ExprRep::Var("a4".to_string())
        );
    }

    #[test]
    fn test_eval_int_expr() {
        assert_eq!(eval_int_expr(1, Op::AriOp(AriOp::Add), 2), ExprRep::Int(3));
        assert_eq!(eval_int_expr(3, Op::AriOp(AriOp::Sub), 2), ExprRep::Int(1));
        assert_eq!(eval_int_expr(10, Op::AriOp(AriOp::Div), 2), ExprRep::Int(5));
        assert_eq!(eval_int_expr(2, Op::AriOp(AriOp::Mul), 5), ExprRep::Int(10));
        assert_eq!(
            eval_int_expr(1, Op::RelOp(RelOp::Eq), 1),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_int_expr(2, Op::RelOp(RelOp::Neq), 1),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_int_expr(1, Op::RelOp(RelOp::Leq), 5),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_int_expr(5, Op::RelOp(RelOp::Geq), 5),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_int_expr(4, Op::RelOp(RelOp::Les), 5),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_int_expr(6, Op::RelOp(RelOp::Gre), 5),
            ExprRep::Bool(true)
        );
    }

    #[test]
    fn test_eval_bool_expr() {
        assert_eq!(
            eval_bool_expr(true, Op::LogOp(LogOp::And), false),
            ExprRep::Bool(false)
        );
        assert_eq!(
            eval_bool_expr(true, Op::LogOp(LogOp::Or), false),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_bool_expr(true, Op::RelOp(RelOp::Eq), true),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_bool_expr(true, Op::RelOp(RelOp::Neq), false),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_bool_expr(false, Op::RelOp(RelOp::Leq), true),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_bool_expr(true, Op::RelOp(RelOp::Geq), false),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_bool_expr(false, Op::RelOp(RelOp::Les), true),
            ExprRep::Bool(true)
        );
        assert_eq!(
            eval_bool_expr(true, Op::RelOp(RelOp::Gre), false),
            ExprRep::Bool(true)
        );
    }

    #[test]
    fn test_eval_bin_expr() {
        assert_eq!(
            interpreter(vec![Expr::BinExpr(
                Box::new(Expr::Int(1)),
                Op::AriOp(AriOp::Add),
                Box::new(Expr::Int(2)),
            )]),
            ExprRep::Int(3)
        );
    }

    #[test]
    fn test_eval_var_expr() {
        interpreter(vec![
            Expr::Let(
                Box::new(Expr::Var("b1".to_string())),
                Type::Int,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(2)),
                )),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("b1".to_string())),
                Op::AssOp(AssOp::AddEq),
                Box::new(Expr::Int(2)),
            ),
        ]);
        assert_eq!(read_var("b1"), ExprRep::Int(4));
        let res = interpreter(vec![
            Expr::Let(
                Box::new(Expr::Var("b2".to_string())),
                Type::Bool,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Bool(false)),
                )),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("b2".to_string())),
                Op::RelOp(RelOp::Eq),
                Box::new(Expr::Bool(false)),
            ),
        ]);
        assert_eq!(res, ExprRep::Bool(true));
        let res = interpreter(vec![
            Expr::Let(
                Box::new(Expr::Var("b3".to_string())),
                Type::Bool,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Bool(false)),
                )),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("b3".to_string())),
                Op::RelOp(RelOp::Neq),
                Box::new(Expr::Bool(true)),
            ),
        ]);
        assert_eq!(res, ExprRep::Bool(true));
        let res = interpreter(vec![
            Expr::Let(
                Box::new(Expr::Var("b4".to_string())),
                Type::Int,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(1)),
                )),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("b4".to_string())),
                Op::RelOp(RelOp::Leq),
                Box::new(Expr::Int(5)),
            ),
        ]);
        assert_eq!(res, ExprRep::Bool(true));
        let res = interpreter(vec![
            Expr::Let(
                Box::new(Expr::Var("b5".to_string())),
                Type::Int,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(7)),
                )),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("b5".to_string())),
                Op::RelOp(RelOp::Geq),
                Box::new(Expr::Int(5)),
            ),
        ]);
        assert_eq!(res, ExprRep::Bool(true));
        let res = interpreter(vec![
            Expr::Let(
                Box::new(Expr::Var("b6".to_string())),
                Type::Int,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(1)),
                )),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("b6".to_string())),
                Op::RelOp(RelOp::Les),
                Box::new(Expr::Int(5)),
            ),
        ]);
        assert_eq!(res, ExprRep::Bool(true));
        let res = interpreter(vec![
            Expr::Let(
                Box::new(Expr::Var("b7".to_string())),
                Type::Int,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(6)),
                )),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("b7".to_string())),
                Op::RelOp(RelOp::Gre),
                Box::new(Expr::Int(5)),
            ),
        ]);
        assert_eq!(res, ExprRep::Bool(true));
    }

    #[test]
    fn test_eval_let() {
        interpreter(vec![Expr::Let(
            Box::new(Expr::Var("c1".to_string())),
            Type::Int,
            Box::new(Expr::BinExpr(
                Box::new(Expr::Var("".to_string())),
                Op::AssOp(AssOp::Eq),
                Box::new(Expr::Int(1)),
            )),
        )]);
        assert_eq!(read_var("c1"), ExprRep::Int(1));
        interpreter(vec![Expr::Let(
            Box::new(Expr::Var("c2".to_string())),
            Type::Bool,
            Box::new(Expr::BinExpr(
                Box::new(Expr::Var("".to_string())),
                Op::AssOp(AssOp::Eq),
                Box::new(Expr::Bool(true)),
            )),
        )]);
        assert_eq!(read_var("c2"), ExprRep::Bool(true));
        interpreter(vec![Expr::Let(
            Box::new(Expr::Var("c3".to_string())),
            Type::Bool,
            Box::new(Expr::BinExpr(
                Box::new(Expr::Bool(false)),
                Op::LogOp(LogOp::And),
                Box::new(Expr::Bool(true)),
            )),
        )]);
        assert_eq!(read_var("c3"), ExprRep::Bool(false));
        interpreter(vec![
            Expr::Fn(
                Box::new(Expr::Var("fnc1".to_string())),
                vec![(Expr::Var("c4".to_string()), Type::Int)],
                Type::Int,
                vec![Expr::Return(Box::new(Expr::Var("c4".to_string())))],
            ),
            Expr::Let(
                Box::new(Expr::Var("c5".to_string())),
                Type::Int,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::FnCall(
                        Box::new(Expr::Var("fnc1".to_string())),
                        vec![Expr::Int(5)],
                    )),
                )),
            ),
        ]);
        assert_eq!(read_var("c5"), ExprRep::Int(5));
        interpreter(vec![
            Expr::Fn(
                Box::new(Expr::Var("fnc2".to_string())),
                vec![(Expr::Var("c6".to_string()), Type::Int)],
                Type::Int,
                vec![Expr::Return(Box::new(Expr::Var("c6".to_string())))],
            ),
            Expr::Fn(
                Box::new(Expr::Var("fnc3".to_string())),
                vec![(Expr::Var("c7".to_string()), Type::Int)],
                Type::Int,
                vec![Expr::Return(Box::new(Expr::Var("c7".to_string())))],
            ),
            Expr::Fn(
                Box::new(Expr::Var("fnc4".to_string())),
                vec![(Expr::Var("c8".to_string()), Type::Int)],
                Type::Int,
                vec![Expr::Return(Box::new(Expr::Var("c8".to_string())))],
            ),
            Expr::Fn(
                Box::new(Expr::Var("fnc5".to_string())),
                vec![(Expr::Var("c9".to_string()), Type::Int)],
                Type::Int,
                vec![Expr::Return(Box::new(Expr::Var("c9".to_string())))],
            ),
            Expr::Let(
                Box::new(Expr::Var("c10".to_string())),
                Type::Int,
                Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::FnCall(
                            Box::new(Expr::Var("fnc2".to_string())),
                            vec![Expr::Int(5)],
                        )),
                        Op::AriOp(AriOp::Add),
                        Box::new(Expr::BinExpr(
                            Box::new(Expr::FnCall(
                                Box::new(Expr::Var("fnc3".to_string())),
                                vec![Expr::Int(2)],
                            )),
                            Op::AriOp(AriOp::Add),
                            Box::new(Expr::BinExpr(
                                Box::new(Expr::FnCall(
                                    Box::new(Expr::Var("fnc4".to_string())),
                                    vec![Expr::Int(3)],
                                )),
                                Op::AriOp(AriOp::Add),
                                Box::new(Expr::FnCall(
                                    Box::new(Expr::Var("fnc5".to_string())),
                                    vec![Expr::Int(5)],
                                )),
                            )),
                        )),
                    )),
                )),
            ),
        ]);
        assert_eq!(read_var("c10"), ExprRep::Int(15));
    }

    #[test]
    fn test_eval_return() {
        assert_eq!(
            interpreter(vec![Expr::Return(Box::new(Expr::Int(1)))]),
            ExprRep::Int(1)
        );
        assert_eq!(
            interpreter(vec![Expr::Return(Box::new(Expr::Bool(true)))]),
            ExprRep::Bool(true)
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("d1".to_string())),
                    Type::Int,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Int(2)),
                    )),
                ),
                Expr::Return(Box::new(Expr::Var("d1".to_string())))
            ]),
            ExprRep::Int(2)
        );
        assert_eq!(
            interpreter(vec![Expr::Return(Box::new(Expr::BinExpr(
                Box::new(Expr::Int(1)),
                Op::AriOp(AriOp::Add),
                Box::new(Expr::Int(2)),
            )))]),
            ExprRep::Int(3)
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("d2".to_string())),
                    Type::Int,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Int(2)),
                    )),
                ),
                Expr::Let(
                    Box::new(Expr::Var("d3".to_string())),
                    Type::Int,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Int(2)),
                    )),
                ),
                Expr::Return(Box::new(Expr::VarExpr(
                    Box::new(Expr::Var("d2".to_string())),
                    Op::AriOp(AriOp::Add),
                    Box::new(Expr::Var("d3".to_string())),
                )))
            ]),
            ExprRep::Int(4)
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("d4".to_string())),
                    Type::Int,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Int(2)),
                    )),
                ),
                Expr::VarExpr(
                    Box::new(Expr::Var("d4".to_string())),
                    Op::AssOp(AssOp::AddEq),
                    Box::new(Expr::Int(1))
                ),
                Expr::Return(Box::new(Expr::VarExpr(
                    Box::new(Expr::Var("d4".to_string())),
                    Op::AriOp(AriOp::Add),
                    Box::new(Expr::Int(1)),
                )))
            ]),
            ExprRep::Int(4)
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("d5".to_string())),
                    Type::Int,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Int(2)),
                    )),
                ),
                Expr::Return(Box::new(Expr::VarExpr(
                    Box::new(Expr::Var("d5".to_string())),
                    Op::AriOp(AriOp::Sub),
                    Box::new(Expr::Int(1)),
                )))
            ]),
            ExprRep::Int(1)
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("d6".to_string())),
                    Type::Int,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Int(10)),
                    )),
                ),
                Expr::Return(Box::new(Expr::VarExpr(
                    Box::new(Expr::Var("d6".to_string())),
                    Op::AriOp(AriOp::Div),
                    Box::new(Expr::Int(5)),
                )))
            ]),
            ExprRep::Int(2)
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("d7".to_string())),
                    Type::Int,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Int(2)),
                    )),
                ),
                Expr::Return(Box::new(Expr::VarExpr(
                    Box::new(Expr::Var("d7".to_string())),
                    Op::AriOp(AriOp::Mul),
                    Box::new(Expr::Int(5)),
                )))
            ]),
            ExprRep::Int(10)
        );
    }

    #[test]
    fn test_eval_if() {
        assert_eq!(
            interpreter(vec![Expr::If(
                Box::new(Expr::Bool(true)),
                vec![Expr::Return(Box::new(Expr::Int(1)))]
            )]),
            ExprRep::Int(1)
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("f1".to_string())),
                    Type::Bool,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Bool(true)),
                    )),
                ),
                Expr::Let(
                    Box::new(Expr::Var("f2".to_string())),
                    Type::Bool,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Bool(true)),
                    )),
                ),
                Expr::If(
                    Box::new(Expr::VarExpr(
                        Box::new(Expr::Var("f1".to_string())),
                        Op::RelOp(RelOp::Eq),
                        Box::new(Expr::Var("f2".to_string()))
                    )),
                    vec![Expr::Return(Box::new(Expr::Int(1)))]
                )
            ]),
            ExprRep::Int(1)
        );
    }

    #[test]
    fn test_eval_if_else() {
        assert_eq!(
            interpreter(vec![Expr::IfElse(
                Box::new(Expr::Bool(false)),
                vec![Expr::Return(Box::new(Expr::Int(1)))],
                vec![Expr::Return(Box::new(Expr::Int(2)))],
            )]),
            ExprRep::Int(2)
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("g1".to_string())),
                    Type::Bool,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Bool(true)),
                    )),
                ),
                Expr::IfElse(
                    Box::new(Expr::Var("g1".to_string())),
                    vec![Expr::Return(Box::new(Expr::Int(2)))],
                    vec![Expr::Return(Box::new(Expr::Int(1)))],
                )
            ]),
            ExprRep::Int(2)
        );
    }

    #[test]
    fn test_eval_while() {
        assert_eq!(
            interpreter(vec![Expr::While(
                Box::new(Expr::Bool(true)),
                vec![Expr::Return(Box::new(Expr::Bool(false)))]
            )]),
            ExprRep::Bool(false),
        );
        assert_eq!(
            interpreter(vec![
                Expr::Let(
                    Box::new(Expr::Var("h1".to_string())),
                    Type::Bool,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Bool(true)),
                    )),
                ),
                Expr::Let(
                    Box::new(Expr::Var("h2".to_string())),
                    Type::Bool,
                    Box::new(Expr::BinExpr(
                        Box::new(Expr::Var("".to_string())),
                        Op::AssOp(AssOp::Eq),
                        Box::new(Expr::Bool(false)),
                    )),
                ),
                Expr::While(
                    Box::new(Expr::VarExpr(
                        Box::new(Expr::Var("h1".to_string())),
                        Op::RelOp(RelOp::Neq),
                        Box::new(Expr::Var("h2".to_string()))
                    )),
                    vec![Expr::Return(Box::new(Expr::Int(2)))]
                )
            ]),
            ExprRep::Int(2),
        );
    }
    #[test]
    fn test_eval_fn_call() {
        assert_eq!(
            interpreter(vec![
                Expr::Fn(
                    Box::new(Expr::Var("testfn1".to_string())),
                    vec![(Expr::Var("i1".to_string()), Type::Int,),],
                    Type::Int,
                    vec![Expr::Return(Box::new(Expr::Var("i1".to_string())))]
                ),
                Expr::Return(Box::new(Expr::FnCall(
                    Box::new(Expr::Var("testfn1".to_string())),
                    vec![Expr::Int(5)]
                )))
            ]),
            ExprRep::Int(5)
        );
    }

    #[test]
    fn test_interpreter() {
        assert_eq!(
            interpreter(vec![
                Expr::Fn(
                    Box::new(Expr::Var("testfn1".to_string())),
                    vec![(Expr::Var("a".to_string()), Type::Bool)],
                    Type::Int,
                    vec![
                        Expr::Let(
                            Box::new(Expr::Var("c".to_string())),
                            Type::Int,
                            Box::new(Expr::BinExpr(
                                Box::new(Expr::Var("".to_string())),
                                Op::AssOp(AssOp::Eq),
                                Box::new(Expr::Int(2)),
                            )),
                        ),
                        Expr::IfElse(
                            Box::new(Expr::Var("a".to_string())),
                            vec![
                                Expr::Let(
                                    Box::new(Expr::Var("b".to_string())),
                                    Type::Int,
                                    Box::new(Expr::BinExpr(
                                        Box::new(Expr::Var("".to_string())),
                                        Op::AssOp(AssOp::Eq),
                                        Box::new(Expr::Int(1)),
                                    )),
                                ),
                                Expr::Return(Box::new(Expr::Var("b".to_string()))),
                            ],
                            vec![Expr::Return(Box::new(Expr::Var("c".to_string())))],
                        ),
                    ],
                ),
                Expr::Fn(
                    Box::new(Expr::Var("testfn2".to_string())),
                    vec![],
                    Type::Int,
                    vec![Expr::Return(Box::new(Expr::FnCall(
                        Box::new(Expr::Var("testfn1".to_string())),
                        vec![Expr::Bool(true)],
                    )))],
                ),
                Expr::Fn(
                    Box::new(Expr::Var("testfn3".to_string())),
                    vec![
                        (Expr::Var("b".to_string()), Type::Bool),
                        (Expr::Var("c".to_string()), Type::Bool),
                    ],
                    Type::Int,
                    vec![
                        Expr::Let(
                            Box::new(Expr::Var("d".to_string())),
                            Type::Bool,
                            Box::new(Expr::VarExpr(
                                Box::new(Expr::Var("b".to_string())),
                                Op::LogOp(LogOp::And),
                                Box::new(Expr::Var("c".to_string())),
                            )),
                        ),
                        Expr::Let(
                            Box::new(Expr::Var("n".to_string())),
                            Type::Int,
                            Box::new(Expr::BinExpr(
                                Box::new(Expr::Var("".to_string())),
                                Op::AssOp(AssOp::Eq),
                                Box::new(Expr::Int(0)),
                            )),
                        ),
                        Expr::While(
                            Box::new(Expr::VarExpr(
                                Box::new(Expr::Var("d".to_string())),
                                Op::RelOp(RelOp::Eq),
                                Box::new(Expr::Bool(true)),
                            )),
                            vec![
                                Expr::VarExpr(
                                    Box::new(Expr::Var("n".to_string())),
                                    Op::AssOp(AssOp::AddEq),
                                    Box::new(Expr::Int(1)),
                                ),
                                Expr::VarExpr(
                                    Box::new(Expr::Var("d".to_string())),
                                    Op::AssOp(AssOp::Eq),
                                    Box::new(Expr::Bool(false)),
                                ),
                            ],
                        ),
                        Expr::Return(Box::new(Expr::Var("n".to_string()))),
                    ],
                ),
                Expr::Fn(
                    Box::new(Expr::Var("main".to_string())),
                    vec![],
                    Type::Int,
                    vec![
                        Expr::Let(
                            Box::new(Expr::Var("a".to_string())),
                            Type::Int,
                            Box::new(Expr::BinExpr(
                                Box::new(Expr::Var("".to_string())),
                                Op::AssOp(AssOp::Eq),
                                Box::new(Expr::FnCall(
                                    Box::new(Expr::Var("testfn2".to_string())),
                                    vec![],
                                )),
                            )),
                        ),
                        Expr::Let(
                            Box::new(Expr::Var("b".to_string())),
                            Type::Int,
                            Box::new(Expr::BinExpr(
                                Box::new(Expr::Var("".to_string())),
                                Op::AssOp(AssOp::Eq),
                                Box::new(Expr::FnCall(
                                    Box::new(Expr::Var("testfn3".to_string())),
                                    vec![Expr::Bool(true), Expr::Bool(true)],
                                )),
                            )),
                        ),
                        Expr::Return(Box::new(Expr::VarExpr(
                            Box::new(Expr::Var("a".to_string())),
                            Op::AriOp(AriOp::Add),
                            Box::new(Expr::Var("b".to_string())),
                        ))),
                    ],
                ),
                Expr::Return(Box::new(Expr::FnCall(
                    Box::new(Expr::Var("main".to_string())),
                    vec![]
                ))),
            ]),
            ExprRep::Int(2)
        );
    }
}
