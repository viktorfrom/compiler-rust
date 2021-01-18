use core::panic;

use crate::memory::*;
use crate::{ast::*, memory};

pub fn interpreter(tree: Vec<Expr>) -> ExprRep {
    let mut res = ExprRep::Null;
    for expr in tree.iter() {
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
        Expr::Var(n) => read_from_var(&n),

        Expr::BinExpr(l, op, r) => eval_bin_expr(*l, op, *r),
        Expr::VarExpr(var, op, expr) => eval_var_expr(*var, op, *expr),

        Expr::Let(var, var_type, expr) => eval_let(*var, var_type, *expr),

        Expr::If(cond, block) => eval_if(*cond, block),
        Expr::IfElse(cond, block1, block2) => eval_if_else(*cond, block1, block2),

        // Expr::FnCall(fn_var, args) => eval_fn_call(fn_var, args),
        Expr::Return(expr) => eval_return(*expr),
        _ => panic!("Invalid expr!"),
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

// fn eval_fn_call(fn_var: Box<Expr>, args: Vec<Expr>) -> ExprRep {
//     println!("var = {:#?}, args = {:#?}", fn_var, args);
//     return ExprRep::Null
// }

fn eval_let(var: Expr, _var_type: Type, expr: Expr) -> ExprRep {
    match (var, eval_expr(expr)) {
        (Expr::Var(n), ExprRep::Int(val)) => insert_var(ExprRep::Var(n), ExprRep::Int(val)),
        _ => panic!("Invalid let expr!"),
    }
}

fn eval_return(expr: Expr) -> ExprRep {
    return eval_expr(expr);
}

fn eval_bin_expr(l: Expr, op: Op, r: Expr) -> ExprRep {
    match (l, r) {
        (Expr::Int(left), Expr::Int(right)) => eval_op(left, op, right),
        (Expr::Var(v), Expr::Int(right)) => match read_from_var(&v) {
            ExprRep::Int(c) => ExprRep::Int(c + right),
            _ => ExprRep::Int(right),
        },
        _ => panic!("Invalid bin expr!"),
    }
}

/// Inserts the variable into memory or updates existing value
fn eval_var_expr(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match op {
        Op::AriOp(AriOp::Add) => match (var, expr) {
            (Expr::Var(v), Expr::Var(expr)) => match (read_from_var(&v), read_from_var(&expr)) {
                (ExprRep::Int(v1), ExprRep::Int(v2)) => ExprRep::Int(v1 + v2),
                _ => ExprRep::Null,
            },
            _ => panic!("Var add fail!"),
        },

        Op::AssOp(AssOp::Eq) => match (var.clone(), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(val)) => insert_var(ExprRep::Var(v), ExprRep::Int(val)),
            (Expr::Var(v), ExprRep::Bool(val)) => insert_var(ExprRep::Var(v), ExprRep::Bool(val)),
            _ => panic!("Var insert fail!"),
        },
        Op::AssOp(AssOp::AddEq) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Int(old_val + new_val))
            }
            _ => panic!("Var Add update fail!"),
        },
        Op::AssOp(AssOp::SubEq) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Int(old_val - new_val))
            }
            _ => panic!("Var Sub update fail!"),
        },
        Op::AssOp(AssOp::DivEq) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Int(old_val / new_val))
            }
            _ => panic!("Var Div update fail!"),
        },
        Op::AssOp(AssOp::MulEq) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Int(old_val * new_val))
            }
            _ => panic!("Var Mul update fail!"),
        },
        Op::LogOp(LogOp::And) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Bool(old_val), ExprRep::Bool(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Bool(old_val && new_val))
            }
            _ => panic!("Var And update fail!"),
        },
        Op::LogOp(LogOp::Or) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Bool(old_val), ExprRep::Bool(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Bool(old_val || new_val))
            }
            _ => panic!("Var Or update fail!"),
        },

        Op::RelOp(RelOp::Eq) => match (eval_expr(var), eval_expr(expr)) {
            (ExprRep::Bool(v1), ExprRep::Bool(v2)) => {
                if v1 == v2 {
                    return ExprRep::Bool(true);
                }
                ExprRep::Bool(false)
            }

            _ => ExprRep::Null,
        },

        _ => panic!("Invalid var expr!"),
    }
}

fn eval_op(l: i32, op: Op, r: i32) -> ExprRep {
    match op {
        Op::AriOp(op) => eval_ari_op(l, op, r),
        // Op::AssOp(op) => eval_ass_op(l, op, r),
        // Op::LogOp(op) => eval_log_op(l, op, r),
        // Op::RelOp(op) => eval_rel_op(l, op, r),
        _ => panic!("Invalid op expr!"),
    }
}

fn eval_ari_op(l: i32, ari_op: AriOp, r: i32) -> ExprRep {
    match ari_op {
        AriOp::Add => ExprRep::Int(l + r),
        AriOp::Sub => ExprRep::Int(l - r),
        AriOp::Div => ExprRep::Int(l / r),
        AriOp::Mul => ExprRep::Int(l * r),
    }
}

// fn eval_ari_op(l: i32, op: Op, r: i32) -> ExprRep {
//     match op {
//         Op::AriOp(AriOp::Add) => ExprRep::Int(l + r),
//         Op::AriOp(AriOp::Sub) => ExprRep::Int(l - r),
//         Op::AriOp(AriOp::Div) => ExprRep::Int(l / r),
//         Op::AriOp(AriOp::Mul) => ExprRep::Int(l * r),
//         _ => panic!("Invalid ari op!"),
//     }
// }

// fn eval_ass_op(l: i32, ass_op: AssOp, r: i32) -> ExprRep {
//     match ass_op {
//         AssOp::Eq => ExprRep::Int(l + r),
//         AssOp::AddEq => ExprRep::Int(l + r),
//         AssOp::SubEq => ExprRep::Int(l + r),
//         AssOp::DivEq => ExprRep::Int(l + r),
//         AssOp::MulEq => ExprRep::Int(l + r),
//     }
// }

fn eval_log_op(l: bool, log_op: LogOp, r: bool) -> ExprRep {
    match log_op {
        LogOp::And => ExprRep::Bool(l && r),
        LogOp::Or => ExprRep::Bool(l || r),
    }
}

// fn eval_rel_op(l: i32, ari_op: RelOp, r: i32) -> ExprRep {
//     match ari_op {
//         RelOp::Eq => ExprRep::Bool(l == r),
//         RelOp::Neq => ExprRep::Bool(l != r),
//         RelOp::Leq => ExprRep::Bool(l <= r),
//         RelOp::Geq => ExprRep::Bool(l >= r),
//         RelOp::Les => ExprRep::Bool(l < r),
//         RelOp::Gre => ExprRep::Bool(l > r),
//     }
// }

#[cfg(test)]
mod interpreter_tests {
    use super::*;

    #[test]
    fn test_int() {
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
        insert_var(ExprRep::Var("a".to_string()), ExprRep::Int(1));
        assert_eq!(
            interpreter(vec![Expr::Var("a".to_string())]),
            ExprRep::Int(1)
        );
        insert_var(ExprRep::Var("b".to_string()), ExprRep::Bool(true));
        assert_eq!(
            interpreter(vec![Expr::Var("b".to_string())]),
            ExprRep::Bool(true)
        );
        insert_var(ExprRep::Var("c".to_string()), ExprRep::Var("d".to_string()));
        assert_eq!(
            interpreter(vec![Expr::Var("c".to_string())]),
            ExprRep::Var("d".to_string())
        );
    }

    #[test]
    fn test_eval_ari_op() {
        assert_eq!(eval_ari_op(1, AriOp::Add, 2), ExprRep::Int(3));
        assert_eq!(eval_ari_op(3, AriOp::Sub, 2), ExprRep::Int(1));
        assert_eq!(eval_ari_op(10, AriOp::Div, 2), ExprRep::Int(5));
        assert_eq!(eval_ari_op(2, AriOp::Mul, 5), ExprRep::Int(10));
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
            Expr::VarExpr(
                Box::new(Expr::Var("e".to_string())),
                Op::AssOp(AssOp::Eq),
                Box::new(Expr::Int(2)),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("e".to_string())),
                Op::AssOp(AssOp::AddEq),
                Box::new(Expr::Int(2)),
            ),
        ]);
        assert_eq!(read_from_var("e"), ExprRep::Int(4));

        let eval = interpreter(vec![
            Expr::VarExpr(
                Box::new(Expr::Var("f".to_string())),
                Op::AssOp(AssOp::Eq),
                Box::new(Expr::Bool(false)),
            ),
            Expr::Return(Box::new(Expr::VarExpr(
                Box::new(Expr::Var("f".to_string())),
                Op::LogOp(LogOp::And),
                Box::new(Expr::Bool(true)),
            ))),
        ]);
        assert_eq!(read_from_var("f"), ExprRep::Bool(false));
    }

    #[test]
    fn test_eval_let() {
        interpreter(vec![Expr::Let(
            Box::new(Expr::Var("g".to_string())),
            Type::Int,
            Box::new(Expr::BinExpr(
                Box::new(Expr::Var("".to_string())),
                Op::AssOp(AssOp::Eq),
                Box::new(Expr::Int(1)),
            )),
        )]);
        assert_eq!(read_from_var("g"), ExprRep::Int(1));
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
                Expr::VarExpr(
                    Box::new(Expr::Var("h".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(2)),
                ),
                Expr::Return(Box::new(Expr::Var("h".to_string())))
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
                Expr::VarExpr(
                    Box::new(Expr::Var("i".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(2)),
                ),
                Expr::VarExpr(
                    Box::new(Expr::Var("j".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(2)),
                ),
                Expr::Return(Box::new(Expr::VarExpr(
                    Box::new(Expr::Var("i".to_string())),
                    Op::AriOp(AriOp::Add),
                    Box::new(Expr::Var("j".to_string())),
                )))
            ]),
            ExprRep::Int(4)
        );
        assert_eq!(
            interpreter(vec![
                Expr::VarExpr(
                    Box::new(Expr::Var("k".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Int(2)),
                ),
                Expr::Return(Box::new(Expr::BinExpr(
                    Box::new(Expr::Var("k".to_string())),
                    Op::AriOp(AriOp::Add),
                    Box::new(Expr::Int(1)),
                ))),
            ]),
            ExprRep::Int(3)
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
                Expr::VarExpr(
                    Box::new(Expr::Var("l".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Bool(true)),
                ),
                Expr::VarExpr(
                    Box::new(Expr::Var("m".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Bool(true)),
                ),
                Expr::If(
                    Box::new(Expr::VarExpr(
                        Box::new(Expr::Var("l".to_string())),
                        Op::RelOp(RelOp::Eq),
                        Box::new(Expr::Var("m".to_string()))
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
                Expr::VarExpr(
                    Box::new(Expr::Var("n".to_string())),
                    Op::AssOp(AssOp::Eq),
                    Box::new(Expr::Bool(true)),
                ),
                Expr::IfElse(
                    Box::new(Expr::Var("n".to_string())),
                    vec![Expr::Return(Box::new(Expr::Int(2)))],
                    vec![Expr::Return(Box::new(Expr::Int(1)))],
                )
            ]),
            ExprRep::Int(2)
        );
    }
}
