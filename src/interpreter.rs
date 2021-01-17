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

        _ => panic!("Invalid expr!"),
    }
}

fn eval_bin_expr(l: Expr, op: Op, r: Expr) -> ExprRep {
    match (l, r) {
        (Expr::Int(left), Expr::Int(right)) => eval_op(left, op, right),
        _ => panic!("Invalid bin expr!"),
    }
}
/// Inserts the variable into memory or updates the value if it already exists
fn eval_var_expr(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match op {
        Op::AssOp(AssOp::Equ) => match (var.clone(), eval_expr(expr)) {
            (Expr::Var(n), ExprRep::Int(val)) => insert_var(ExprRep::Var(n), ExprRep::Int(val)),
            _ => panic!(),
        },
        Op::AssOp(AssOp::PluEqu) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(n), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(n), ExprRep::Int(old_val + new_val))
            }
            _ => panic!(),
        },
        Op::AssOp(AssOp::SubEqu) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(n), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(n), ExprRep::Int(old_val - new_val))
            }
            _ => panic!(),
        },
        Op::AssOp(AssOp::DivEqu) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(n), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(n), ExprRep::Int(old_val / new_val))
            }
            _ => panic!(),
        },
        Op::AssOp(AssOp::MulEqu) => match (var.clone(), eval_expr(var), eval_expr(expr)) {
            (Expr::Var(n), ExprRep::Int(old_val), ExprRep::Int(new_val)) => {
                insert_var(ExprRep::Var(n), ExprRep::Int(old_val * new_val))
            }
            _ => panic!(),
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
        _ => panic!("Invalid bin expr!"),
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

// fn eval_ass_op(l: i32, ass_op: AssOp, r: i32) -> ExprRep {
//     match ass_op {
//         AssOp::Equ => ExprRep::Int(l + r),
//         AssOp::PluEqu => ExprRep::Int(l + r),
//         AssOp::SubEqu => ExprRep::Int(l + r),
//         AssOp::DivEqu => ExprRep::Int(l + r),
//         AssOp::MulEqu => ExprRep::Int(l + r),
//     }
// }

// fn eval_log_op(l: bool, ari_op: LogOp, r: bool) -> ExprRep {
//     match ari_op {
//         LogOp::And => ExprRep::Bool(l && r),
//         LogOp::Or => ExprRep::Bool(l || r),
//     }
// }

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
                Op::AssOp(AssOp::Equ),
                Box::new(Expr::Int(2)),
            ),
            Expr::VarExpr(
                Box::new(Expr::Var("e".to_string())),
                Op::AssOp(AssOp::PluEqu),
                Box::new(Expr::Int(2)),
            ),
        ]);
        assert_eq!(read_from_var("e"), ExprRep::Int(4));
    }
}
