use crate::ast::*;
use crate::memory::*;

pub fn type_checker(ast: Vec<Expr>) -> bool {
    let mut res = Vec::new();
    for expr in ast.iter() {
        res.push(type_expr(expr.clone()));

        match res {
            _ => continue,
        }
    }

    if res.is_empty() {
        return false;
    }

    return true;
}

fn type_interp(ast: Vec<Expr>) -> ExprRep {
    let mut res = ExprRep::Null;
    for expr in ast.iter() {
        res = type_expr(expr.clone());
        match res {
            _ => continue,
        }
    }
    return res;
}

fn type_expr(expr: Expr) -> ExprRep {
    match expr {
        Expr::Int(_) => ExprRep::Int(0),
        Expr::Bool(_) => ExprRep::Bool(false),
        Expr::Var(n) => read_var(&n),

        Expr::BinExpr(l, op, r) => type_bin_expr(*l, op, *r),
        Expr::VarExpr(var, op, expr) => type_var_expr(*var, op, *expr),

        Expr::Let(var, var_type, expr) => type_let(*var, var_type, *expr),

        Expr::If(cond, block) => type_if(*cond, block),
        Expr::IfElse(cond, block1, block2) => type_if_else(*cond, block1, block2),
        Expr::While(cond, block) => type_while(*cond, block),

        Expr::Fn(fn_var, params, ret_type, block) => type_fn(*fn_var, params, ret_type, block),
        Expr::FnCall(fn_var, args) => type_fn_call(*fn_var, args),
        Expr::Return(expr) => type_return(*expr),
    }
}

fn type_fn(fn_var: Expr, params: Vec<(Expr, Type)>, ret_type: Type, block: Vec<Expr>) -> ExprRep {
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

fn type_fn_call(fn_var: Expr, args: Vec<Expr>) -> ExprRep {
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
                                let type_arg = type_expr(args[i].clone());
                                match (t, type_arg.clone()) {
                                    (Type::Int, ExprRep::Int(_)) => {
                                        insert_var(ExprRep::Var(v.to_string()), type_arg)
                                    }
                                    (Type::Bool, ExprRep::Bool(_)) => {
                                        insert_var(ExprRep::Var(v.to_string()), type_arg)
                                    }
                                    _ => panic!("Return type does not match!"),
                                };
                            }
                            _ => panic!("Invalid param var!"),
                        }
                    }
                }

                let res = type_interp(block);
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

fn type_if(cond: Expr, block: Vec<Expr>) -> ExprRep {
    match type_expr(cond) {
        ExprRep::Bool(c) => {
            if c {
                return type_interp(block);
            }
            return ExprRep::Null;
        }
        _ => panic!("If stmt fail!"),
    }
}

fn type_if_else(cond: Expr, block1: Vec<Expr>, block2: Vec<Expr>) -> ExprRep {
    match type_expr(cond) {
        ExprRep::Bool(c) => {
            if c {
                return type_interp(block1);
            } else {
                return type_interp(block2);
            }
        }
        _ => panic!("IfElse stmt fail!"),
    }
}

fn type_while(cond: Expr, block: Vec<Expr>) -> ExprRep {
    match type_expr(cond) {
        ExprRep::Bool(c) => {
            if c {
                return type_interp(block);
            }
            return ExprRep::Null;
        }
        _ => panic!("While stmt fail!"),
    }
}

fn type_let(var: Expr, _var_type: Type, expr: Expr) -> ExprRep {
    match (var, type_expr(expr)) {
        (Expr::Var(v), ExprRep::Int(val)) => insert_var(ExprRep::Var(v), ExprRep::Int(val)),
        (Expr::Var(v), ExprRep::Bool(val)) => insert_var(ExprRep::Var(v), ExprRep::Bool(val)),
        _ => panic!("Invalid let expr!"),
    }
}

fn type_return(expr: Expr) -> ExprRep {
    return type_expr(expr);
}

fn type_bin_expr(l: Expr, op: Op, r: Expr) -> ExprRep {
    match (type_expr(l), type_expr(r.clone())) {
        (ExprRep::Int(left), ExprRep::Int(right)) => type_int_expr(left, op, right),
        (ExprRep::Var(v), ExprRep::Int(right)) => match read_var(&v) {
            ExprRep::Int(val) => type_int_expr(val, op, right),
            _ => ExprRep::Int(right),
        },
        (ExprRep::Bool(left), ExprRep::Bool(right)) => type_bool_expr(left, op, right),
        (ExprRep::Var(v), ExprRep::Bool(right)) => match read_var(&v) {
            ExprRep::Bool(val) => type_bool_expr(val, op, right),
            _ => ExprRep::Bool(right),
        },
        (ExprRep::Null, _) => type_expr(r),
        _ => panic!("Invalid bin expr!"),
    }
}

/// Updates existing value in memory
fn type_var_expr(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match op {
        Op::AriOp(_) => var_ari_op(var, op, expr),
        Op::AssOp(_) => var_ass_op(var, op, expr),
        Op::LogOp(_) => var_log_op(var, op, expr),
        Op::RelOp(_) => var_rel_op(var, op, expr),
    }
}

fn type_int_expr(l: i32, op: Op, r: i32) -> ExprRep {
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
fn type_bool_expr(l: bool, op: Op, r: bool) -> ExprRep {
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
            (ExprRep::Int(v1), ExprRep::Int(v2)) => type_int_expr(v1, op, v2),
            _ => panic!("Var(Int) Var(Int) op fail!"),
        },
        (Expr::Var(v), op, Expr::Int(expr)) => match read_var(&v) {
            ExprRep::Int(v1) => type_int_expr(v1, op, expr),
            _ => panic!("Var(Int) Int op fail!"),
        },
        _ => panic!("Invalid Var Log op!"),
    }
}

fn var_ass_op(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match (var.clone(), op, type_expr(expr)) {
        (Expr::Var(v), Op::AssOp(AssOp::Eq), ExprRep::Int(val)) => {
            insert_var(ExprRep::Var(v), ExprRep::Int(val))
        }
        (Expr::Var(v), Op::AssOp(AssOp::Eq), ExprRep::Bool(val)) => {
            insert_var(ExprRep::Var(v), ExprRep::Bool(val))
        }
        (Expr::Var(v), Op::AssOp(AssOp::AddEq), ExprRep::Int(new_val)) => match type_expr(var) {
            ExprRep::Int(old_val) => insert_var(ExprRep::Var(v), ExprRep::Int(old_val + new_val)),
            _ => panic!("Var Add update fail!"),
        },
        (Expr::Var(v), Op::AssOp(AssOp::SubEq), ExprRep::Int(new_val)) => match type_expr(var) {
            ExprRep::Int(old_val) => insert_var(ExprRep::Var(v), ExprRep::Int(old_val - new_val)),
            _ => panic!("Var Sub update fail!"),
        },
        (Expr::Var(v), Op::AssOp(AssOp::DivEq), ExprRep::Int(new_val)) => match type_expr(var) {
            ExprRep::Int(old_val) => insert_var(ExprRep::Var(v), ExprRep::Int(old_val / new_val)),
            _ => panic!("Var Div update fail!"),
        },
        (Expr::Var(v), Op::AssOp(AssOp::MulEq), ExprRep::Int(new_val)) => match type_expr(var) {
            ExprRep::Int(old_val) => insert_var(ExprRep::Var(v), ExprRep::Int(old_val * new_val)),
            _ => panic!("Var Mul update fail!"),
        },
        _ => panic!("Var update fail!"),
    }
}

fn var_log_op(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match (type_expr(var), op, type_expr(expr)) {
        (ExprRep::Bool(b1), op, ExprRep::Bool(b2)) => type_bool_expr(b1, op, b2),
        _ => panic!("Invalid Var Log op!"),
    }
}

fn var_rel_op(var: Expr, op: Op, expr: Expr) -> ExprRep {
    match (type_expr(var), op, type_expr(expr)) {
        (ExprRep::Bool(b1), op, ExprRep::Bool(b2)) => type_bool_expr(b1, op, b2),
        (ExprRep::Int(b1), op, ExprRep::Int(b2)) => type_int_expr(b1, op, b2),
        _ => panic!("Invalid Var Log op!"),
    }
}
