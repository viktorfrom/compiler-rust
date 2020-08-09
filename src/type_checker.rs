use crate::ast::content_tree::*;
use crate::ast::expr_tree::*;

use crate::memory::*;

pub fn type_scope(scope: Vec<Expr>) -> bool {
    let mut res = Vec::new();
    for expr in scope.iter() {
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

fn type_expr(input: Expr) -> Content {
    match input {
        Expr::Num(_) => Content::Num(0),
        Expr::Bool(_) => Content::Bool(false),
        Expr::Str(_) => Content::Str("string".to_string()),

        Expr::ArithOp(op) => match op {
            ArithOp::Add => Content::ContentOp(ContentOp::Add),
            ArithOp::Sub => Content::ContentOp(ContentOp::Sub),
            ArithOp::Mult => Content::ContentOp(ContentOp::Mult),
            ArithOp::Div => Content::ContentOp(ContentOp::Div),
        },

        Expr::LogicOp(op) => match op {
            LogicOp::And => Content::ContentOp(ContentOp::And),
            LogicOp::Or => Content::ContentOp(ContentOp::Or),
            LogicOp::Not => Content::ContentOp(ContentOp::Not),
        },

        Expr::RelOp(op) => match op {
            RelOp::EquEqu => Content::ContentOp(ContentOp::EquEqu),
            RelOp::NotEqu => Content::ContentOp(ContentOp::NotEqu),
            RelOp::LesEqu => Content::ContentOp(ContentOp::LesEqu),
            RelOp::GreEqu => Content::ContentOp(ContentOp::GreEqu),
            RelOp::Les => Content::ContentOp(ContentOp::Les),
            RelOp::Gre => Content::ContentOp(ContentOp::Gre),
        },

        Expr::Type(op) => match op {
            Type::Integer => Content::ContentOp(ContentOp::Integer),
            Type::Bool => Content::ContentOp(ContentOp::Bool),
            Type::Str => Content::ContentOp(ContentOp::Str),
            Type::Void => Content::ContentOp(ContentOp::Void),
        },

        Expr::Return(return_param, var) => match *var {
            Expr::Str(var) => type_return(&return_param, &var.to_string()),
            Expr::Num(var) => Content::Return(return_param, Box::new(Content::Num(var))),
            Expr::Bool(var) => Content::Return(return_param, Box::new(Content::Bool(var))),
            _ => panic!("Invalid Input!"),
        },

        Expr::Func(func_name, params, block) => type_func(type_expr(*func_name), params, block),

        Expr::While(_while_param, var, block) => type_if_while(type_expr(*var), block),
        Expr::If(if_param, block) => type_if_while(type_expr(*if_param), block),

        Expr::Param(param, _param_type) => match *param {
            Expr::Str(param) => {
                insert_var(Content::Str(param.clone()), Content::Str(param.clone()))
            }
            _ => panic!("Invalid Input!"),
        },

        Expr::FuncInput(var, func_name, block) => match *func_name {
            Expr::Str(func_name) => type_func_input(type_expr(*var), &func_name.to_string(), block),
            _ => panic!("Invalid Input!"),
        },

        Expr::Let(left, operator, right) => match *left {
            Expr::Num(left) => type_i32(
                type_expr(Expr::Num(left)),
                type_expr(*operator),
                type_expr(*right),
            ),
            Expr::Bool(left) => type_bool(
                type_expr(Expr::Bool(left)),
                type_expr(*operator),
                type_expr(*right),
            ),
            Expr::Str(left) => type_let(
                type_expr(Expr::Str(left)),
                type_expr(*operator),
                type_expr(*right),
            ),
            _ => (panic!("Invalid input!")),
        },

        _ => (panic!("Invalid input!")),
    }
}

fn type_if_while(if_param: Content, block: Vec<Expr>) -> Content {
    match if_param {
        Content::Bool(true) => type_block(block),
        Content::Bool(false) => Content::Null,
        _ => (panic!("Invalid input!")),
    }
}

fn type_params(params: Vec<Expr>, args: Vec<Expr>) {
    if params.len() != args.len() {
        panic!(
            "ERROR: Wrong amount of arguments. Expected {} found {}",
            params.len(),
            args.len()
        );
    }

    for i in 0..params.len() {
        let name;

        match &params[i] {
            Expr::Param(n, _t) => {
                match &**n {
                    Expr::Str(na) => name = Content::Str(na.to_string()),
                    _ => panic!("ERROR: Value is not a variable"),
                };
            }
            _ => panic!("ERROR: Value is not a parameter"),
        }

        insert_var(name, type_expr(args[i].clone()));
    }
}

fn type_func(func_name: Content, params: Vec<Expr>, block: Vec<Expr>) -> Content {
    let v = vec![params, block];
    insert_function(func_name, v);

    return Content::Null;
}

fn type_func_input(var: Content, func_name: &str, args: Vec<Expr>) -> Content {
    let func_content = read_from_func(func_name).1;
    let params = func_content[0].clone();
    let block = func_content[1].clone();

    type_params(params, args);
    let result = type_block(block);
    let var_name = match var {
        Content::Str(var) => var,
        _ => panic!("err1"),
    };

    let value = match result {
        Content::Return(_, var) => var,
        _ => panic!("err1"),
    };

    return Content::Return(var_name.to_string(), value);
}

fn type_return(return_param: &str, var: &str) -> Content {
    insert_var(
        Content::Str(return_param.to_string()),
        Content::Str(var.to_string()),
    );

    let value = read_from_var(var);
    println!("test = {:#?}", return_param);
    return Content::Return(var.to_string(), Box::new(value));
}

fn type_block(block: Vec<Expr>) -> Content {
    let mut res: Content = Content::Null;
    for expr in block.iter() {
        res = type_expr(expr.clone());
        match res {
            Content::Return(_, _) => break,
            _ => continue,
        }
    }
    return res;
}

fn type_i32(left: Content, operator: Content, right: Content) -> Content {
    match (left, operator, right) {
        (Content::Num(left), Content::ContentOp(ContentOp::Add), Content::Num(right)) => {
            Content::Num(left + right)
        }
        (Content::Num(left), Content::ContentOp(ContentOp::Sub), Content::Num(right)) => {
            Content::Num(left - right)
        }
        (Content::Num(left), Content::ContentOp(ContentOp::Div), Content::Num(right)) => {
            Content::Num(left / right)
        }
        (Content::Num(left), Content::ContentOp(ContentOp::Mult), Content::Num(right)) => {
            Content::Num(left * right)
        }
        _ => (panic!("Invalid input!")),
    }
}

fn type_bool(left: Content, operator: Content, right: Content) -> Content {
    match (left, operator, right) {
        (Content::Bool(left), Content::ContentOp(ContentOp::And), Content::Bool(right)) => {
            Content::Bool(left && right)
        }
        (Content::Bool(left), Content::ContentOp(ContentOp::Or), Content::Bool(right)) => {
            Content::Bool(left || right)
        }
        _ => (panic!("Invalid input!")),
    }
}

fn type_let(left: Content, operator: Content, right: Content) -> Content {
    match (left, operator, right) {
        (Content::Str(left), Content::ContentOp(ContentOp::Integer), Content::Num(right)) => {
            insert_var(Content::Str(left), Content::Num(right))
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Integer), Content::Str(right)) => {
            insert_var(Content::Str(left), read_from_var(&right.to_string()))
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Bool), right) => {
            insert_var(Content::Str(left), right)
        }
        (Content::Str(left), Content::ContentOp(ContentOp::Str), right) => {
            insert_var(Content::Str(left), right)
        }
        _ => (panic!("Invalid input!")),
    }
}

#[cfg(test)]
mod interp_tests {
    use super::*;

    #[test]
    fn test_interp() {
        assert_eq!(type_expr(Expr::Num(1)), Content::Num(0));
        assert_eq!(type_expr(Expr::Bool(true)), Content::Bool(false));
    }

    #[test]
    fn test_interp_node() {
        assert_eq!(
            type_expr(Expr::Let(
                Box::new(Expr::Num(2)),
                Box::new(Expr::ArithOp(ArithOp::Mult)),
                Box::new(Expr::Num(3))
            )),
            Content::Num(0)
        );
        assert_eq!(
            type_expr(Expr::Let(
                Box::new(Expr::Bool(true)),
                Box::new(Expr::LogicOp(LogicOp::And)),
                Box::new(Expr::Bool(true))
            )),
            Content::Bool(false)
        );
    }
}
