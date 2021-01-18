use core::panic;

use crate::ast::*;
use crate::memory::*;

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
        Expr::While(cond, block) => eval_while(*cond, block),

        // Expr::FnCall(fn_var, args) => eval_fn_call(fn_var, args),
        Expr::Return(expr) => eval_return(*expr),
        _ => panic!("Invalid expr!"),
    }
}

fn eval_if(cond: Expr, block: Vec<Expr>) -> ExprRep {
    println!("cond = {:#?}", eval_expr(cond.clone()));
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
    println!("cond = {:#?}", eval_expr(cond.clone()));
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

// fn eval_fn_call(fn_var: Box<Expr>, args: Vec<Expr>) -> ExprRep {
//     println!("var = {:#?}, args = {:#?}", fn_var, args);
//     return ExprRep::Null
// }

fn eval_let(var: Expr, _var_type: Type, expr: Expr) -> ExprRep {
    // println!("var = {:#?}", var);
    // println!("expr = {:#?}", eval_expr(expr.clone()));
    match (var, eval_expr(expr)) {
        (Expr::Var(n), ExprRep::Int(val)) => insert_var(ExprRep::Var(n), ExprRep::Int(val)),
        (Expr::Var(n), ExprRep::Bool(val)) => insert_var(ExprRep::Var(n), ExprRep::Bool(val)),
        _ => panic!("Invalid let expr!"),
    }
}

fn eval_return(expr: Expr) -> ExprRep {
    return eval_expr(expr);
}

fn eval_bin_expr(l: Expr, op: Op, r: Expr) -> ExprRep {
    match (l, r.clone()) {
        (Expr::Int(left), Expr::Int(right)) => eval_ari_op(left, op, right),
        (Expr::Int(v), Expr::BinExpr(_, _, _)) => match eval_expr(r) {
            ExprRep::Int(r) => eval_ari_op(v, op, r),
            _ => ExprRep::Null,
        },
        (Expr::Var(v), Expr::Int(right)) => match read_from_var(&v) {
            ExprRep::Int(val) => ExprRep::Int(val + right),
            _ => ExprRep::Int(right),
        },
        (Expr::Bool(left), Expr::Bool(right)) => eval_log_op(left, op, right),
        (Expr::Bool(v), Expr::BinExpr(_, _, _)) => match eval_expr(r) {
            ExprRep::Bool(r) => eval_log_op(v, op, r),
            _ => ExprRep::Null,
        },
        (Expr::Var(v), Expr::Bool(right)) => match read_from_var(&v) {
            ExprRep::Bool(val) => eval_log_op(val, op, right),
            _ => ExprRep::Bool(right),
        },
        (Expr::Var(_), Expr::BinExpr(_, _, _)) => eval_expr(r),

        _ => panic!("Invalid bin expr!"),
    }
}

/// Inserts the variable into memory or updates existing value
fn eval_var_expr(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match op {
        Op::AriOp(op) => var_ari_op(var, op, expr),
        Op::AssOp(op) => var_ass_op(var, op, expr),
        Op::LogOp(op) => var_log_op(var, op, expr),
        Op::RelOp(op) => var_rel_op(var, op, expr),
    }
}

fn eval_ari_op(l: i32, op: Op, r: i32) -> ExprRep {
    match op {
        Op::AriOp(AriOp::Add) => ExprRep::Int(l + r),
        Op::AriOp(AriOp::Sub) => ExprRep::Int(l - r),
        Op::AriOp(AriOp::Div) => ExprRep::Int(l / r),
        Op::AriOp(AriOp::Mul) => ExprRep::Int(l * r),
        _ => panic!("Invalid Ari op!"),
    }
}
fn eval_log_op(l: bool, op: Op, r: bool) -> ExprRep {
    match op {
        Op::LogOp(LogOp::And) => ExprRep::Bool(l && r),
        Op::LogOp(LogOp::Or) => ExprRep::Bool(l || r),
        _ => panic!("Invalid Log op!"),
    }
}

fn var_ari_op(var: Expr, op: AriOp, expr: Expr) -> ExprRep {
    println!("var = {:#?}, Expr = {:#?}", var, expr);
    match op {
        AriOp::Add => match (var, expr) {
            (Expr::Var(v), Expr::Var(expr)) => match (read_from_var(&v), read_from_var(&expr)) {
                (ExprRep::Int(v1), ExprRep::Int(v2)) => ExprRep::Int(v1 + v2),
                _ => ExprRep::Null,
            },
            (Expr::Var(v), Expr::Int(expr)) => match read_from_var(&v) {
                ExprRep::Int(v1) => ExprRep::Int(v1 + expr),
                _ => ExprRep::Null,
            },
            _ => panic!("Var Add fail!"),
        },
        AriOp::Sub => match (var, expr) {
            (Expr::Var(v), Expr::Var(expr)) => match (read_from_var(&v), read_from_var(&expr)) {
                (ExprRep::Int(v1), ExprRep::Int(v2)) => ExprRep::Int(v1 - v2),
                _ => ExprRep::Null,
            },
            (Expr::Var(v), Expr::Int(expr)) => match read_from_var(&v) {
                ExprRep::Int(v1) => ExprRep::Int(v1 - expr),
                _ => ExprRep::Null,
            },
            _ => panic!("Var Sub fail!"),
        },
        AriOp::Div => match (var, expr) {
            (Expr::Var(v), Expr::Var(expr)) => match (read_from_var(&v), read_from_var(&expr)) {
                (ExprRep::Int(v1), ExprRep::Int(v2)) => ExprRep::Int(v1 / v2),
                _ => ExprRep::Null,
            },
            (Expr::Var(v), Expr::Int(expr)) => match read_from_var(&v) {
                ExprRep::Int(v1) => ExprRep::Int(v1 / expr),
                _ => ExprRep::Null,
            },
            _ => panic!("Var Div fail!"),
        },
        AriOp::Mul => match (var, expr) {
            (Expr::Var(v), Expr::Var(expr)) => match (read_from_var(&v), read_from_var(&expr)) {
                (ExprRep::Int(v1), ExprRep::Int(v2)) => ExprRep::Int(v1 * v2),
                _ => ExprRep::Null,
            },
            (Expr::Var(v), Expr::Int(expr)) => match read_from_var(&v) {
                ExprRep::Int(v1) => ExprRep::Int(v1 * expr),
                _ => ExprRep::Null,
            },
            _ => panic!("Var Mul fail!"),
        },
    }
}

fn var_ass_op(var: Expr, ass_op: AssOp, expr: Expr) -> ExprRep {
    match ass_op {
        AssOp::Eq => match (eval_expr(var), eval_expr(expr)) {
            (ExprRep::Var(v), ExprRep::Int(val)) => insert_var(ExprRep::Var(v), ExprRep::Int(val)),
            (ExprRep::Var(v), ExprRep::Bool(val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Bool(val))
            }
            _ => panic!("Var insert fail!"),
        },
        AssOp::AddEq => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(v.clone()), ExprRep::Int(old_val + new_val))
            }
            _ => panic!("Var Add update fail!"),
        },
        AssOp::SubEq => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Int(old_val - new_val))
            }
            _ => panic!("Var Sub update fail!"),
        },
        AssOp::DivEq => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Int(old_val - new_val))
            }
            _ => panic!("Var Sub update fail!"),
        },
        AssOp::MulEq => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Int(old_val - new_val))
            }
            _ => panic!("Var Sub update fail!"),
        },
    }
}

fn var_log_op(var: Expr, log_op: LogOp, expr: Expr) -> ExprRep {
    match log_op {
        LogOp::And => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Bool(old_val), ExprRep::Bool(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Bool(old_val && new_val))
            }
            _ => panic!("Var And update fail!"),
        },
        LogOp::Or => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Bool(old_val), ExprRep::Bool(new_val)) => {
                insert_var(ExprRep::Var(v), ExprRep::Bool(old_val || new_val))
            }
            _ => panic!("Var Or update fail!"),
        },
    }
}

fn var_rel_op(var: Expr, rel_op: RelOp, expr: Expr) -> ExprRep {
    match rel_op {
        RelOp::Eq => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Bool(b1), ExprRep::Bool(b2)) => {
                if b1 == b2 {
                    insert_var(ExprRep::Var(v.clone()), ExprRep::Bool(true))
                } else {
                    insert_var(ExprRep::Var(v), ExprRep::Bool(false))
                }
            }
            _ => ExprRep::Null,
        },
        RelOp::Neq => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(v), ExprRep::Bool(b1), ExprRep::Bool(b2)) => {
                if b1 != b2 {
                    insert_var(ExprRep::Var(v.clone()), ExprRep::Bool(true))
                } else {
                    insert_var(ExprRep::Var(v), ExprRep::Bool(false))
                }
            }
            _ => ExprRep::Null,
        },
        // should not be used for booleans!
        // RelOp::Leq => ExprRep::Bool(l <= r),
        // RelOp::Geq => ExprRep::Bool(l >= r),
        // RelOp::Les => ExprRep::Bool(l < r),
        // RelOp::Gre => ExprRep::Bool(l > r),
        _ => panic!("Invalid Var Rel op!"),
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
    fn test_eval_ari_op() {
        assert_eq!(eval_ari_op(1, Op::AriOp(AriOp::Add), 2), ExprRep::Int(3));
        assert_eq!(eval_ari_op(3, Op::AriOp(AriOp::Sub), 2), ExprRep::Int(1));
        assert_eq!(eval_ari_op(10, Op::AriOp(AriOp::Div), 2), ExprRep::Int(5));
        assert_eq!(eval_ari_op(2, Op::AriOp(AriOp::Mul), 5), ExprRep::Int(10));
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
        assert_eq!(read_from_var("b1"), ExprRep::Int(4));
        interpreter(vec![
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
        assert_eq!(read_from_var("b2"), ExprRep::Bool(true));
        interpreter(vec![
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
        assert_eq!(read_from_var("b3"), ExprRep::Bool(true));
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
        assert_eq!(read_from_var("c1"), ExprRep::Int(1));
        interpreter(vec![Expr::Let(
            Box::new(Expr::Var("c2".to_string())),
            Type::Bool,
            Box::new(Expr::BinExpr(
                Box::new(Expr::Var("".to_string())),
                Op::AssOp(AssOp::Eq),
                Box::new(Expr::Bool(true)),
            )),
        )]);
        assert_eq!(read_from_var("c2"), ExprRep::Bool(true));
        interpreter(vec![Expr::Let(
            Box::new(Expr::Var("c3".to_string())),
            Type::Bool,
            Box::new(Expr::BinExpr(
                Box::new(Expr::Bool(false)),
                Op::LogOp(LogOp::And),
                Box::new(Expr::Bool(true)),
            )),
        )]);
        assert_eq!(read_from_var("c3"), ExprRep::Bool(false));
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
                Expr::Return(Box::new(Expr::VarExpr(
                    Box::new(Expr::Var("d4".to_string())),
                    Op::AriOp(AriOp::Add),
                    Box::new(Expr::Int(1)),
                )))
            ]),
            ExprRep::Int(3)
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
        // assert_eq!(
        //     interpreter(vec![
        //         Expr::VarExpr(
        //             Box::new(Expr::Var("e1".to_string())),
        //             Op::AssOp(AssOp::Eq),
        //             Box::new(Expr::Int(2)),
        //         ),
        //         Expr::Return(Box::new(Expr::BinExpr(
        //             Box::new(Expr::Var("e1".to_string())),
        //             Op::AriOp(AriOp::Add),
        //             Box::new(Expr::Int(1)),
        //         ))),
        //     ]),
        //     ExprRep::Int(3)
        // );
    }

    // #[test]
    // fn test_eval_if() {
    //     assert_eq!(
    //         interpreter(vec![Expr::If(
    //             Box::new(Expr::Bool(true)),
    //             vec![Expr::Return(Box::new(Expr::Int(1)))]
    //         )]),
    //         ExprRep::Int(1)
    //     );

    //     assert_eq!(
    //         interpreter(vec![
    //             Expr::VarExpr(
    //                 Box::new(Expr::Var("f1".to_string())),
    //                 Op::AssOp(AssOp::Eq),
    //                 Box::new(Expr::Bool(true)),
    //             ),
    //             Expr::VarExpr(
    //                 Box::new(Expr::Var("f2".to_string())),
    //                 Op::AssOp(AssOp::Eq),
    //                 Box::new(Expr::Bool(true)),
    //             ),
    //             Expr::If(
    //                 Box::new(Expr::VarExpr(
    //                     Box::new(Expr::Var("f1".to_string())),
    //                     Op::RelOp(RelOp::Eq),
    //                     Box::new(Expr::Var("f2".to_string()))
    //                 )),
    //                 vec![Expr::Return(Box::new(Expr::Int(1)))]
    //             )
    //         ]),
    //         ExprRep::Int(1)
    //     );
    // }
    // #[test]
    // fn test_eval_if_else() {
    //     assert_eq!(
    //         interpreter(vec![Expr::IfElse(
    //             Box::new(Expr::Bool(false)),
    //             vec![Expr::Return(Box::new(Expr::Int(1)))],
    //             vec![Expr::Return(Box::new(Expr::Int(2)))],
    //         )]),
    //         ExprRep::Int(2)
    //     );
    //     assert_eq!(
    //         interpreter(vec![
    //             Expr::VarExpr(
    //                 Box::new(Expr::Var("g1".to_string())),
    //                 Op::AssOp(AssOp::Eq),
    //                 Box::new(Expr::Bool(true)),
    //             ),
    //             Expr::IfElse(
    //                 Box::new(Expr::Var("g1".to_string())),
    //                 vec![Expr::Return(Box::new(Expr::Int(2)))],
    //                 vec![Expr::Return(Box::new(Expr::Int(1)))],
    //             )
    //         ]),
    //         ExprRep::Int(2)
    //     );
    // }

    // #[test]
    // fn test_eval_while() {
    //     assert_eq!(
    //         interpreter(vec![Expr::While(
    //             Box::new(Expr::Bool(true)),
    //             vec![Expr::Return(Box::new(Expr::Bool(false)))]
    //         )]),
    //         ExprRep::Bool(false),
    //     );
    //     assert_eq!(
    //         interpreter(vec![
    //             Expr::VarExpr(
    //                 Box::new(Expr::Var("h1".to_string())),
    //                 Op::AssOp(AssOp::Eq),
    //                 Box::new(Expr::Bool(true)),
    //             ),
    //             Expr::VarExpr(
    //                 Box::new(Expr::Var("h2".to_string())),
    //                 Op::AssOp(AssOp::Eq),
    //                 Box::new(Expr::Bool(true)),
    //             ),
    //             Expr::While(
    //                 Box::new(Expr::VarExpr(
    //                     Box::new(Expr::Var("h1".to_string())),
    //                     Op::RelOp(RelOp::Eq),
    //                     Box::new(Expr::Var("h2".to_string()))
    //                 )),
    //                 vec![Expr::Return(Box::new(Expr::Int(2)))]
    //             )
    //         ]),
    //         ExprRep::Int(2),
    //     );
    // }
}
