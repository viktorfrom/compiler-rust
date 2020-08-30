use crate::ast::content_tree::*;
use crate::ast::expr_tree::*;
use crate::memory::*;

pub fn eval_scope(scope: Vec<Expr>) -> Content {
    let mut res: Content = Content::Null;
    for expr in scope.iter() {
        res = eval_expr(expr.clone());
        match res {
            _ => continue,
        }
    }
    return res;
}

fn eval_expr(input: Expr) -> Content {
    match input {
        Expr::Num(i) => Content::Num(i),
        Expr::Bool(b) => Content::Bool(b),
        Expr::Str(s) => Content::Str(s),

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

        Expr::AssignOp(op) => match op {
            AssignOp::Equ => Content::ContentOp(ContentOp::Equ),
            AssignOp::PluEqu => Content::ContentOp(ContentOp::PluEqu),
            AssignOp::SubEqu => Content::ContentOp(ContentOp::SubEqu),
            AssignOp::DivEqu => Content::ContentOp(ContentOp::DivEqu),
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
            Expr::Str(var) => eval_return(&return_param, &var.to_string()),
            Expr::Num(var) => Content::Return(return_param, Box::new(Content::Num(var))),
            Expr::Bool(var) => Content::Return(return_param, Box::new(Content::Bool(var))),
            _ => panic!("Invalid Input!"),
        },

        Expr::Func(func_name, params, block) => eval_func(eval_expr(*func_name), params, block),

        Expr::While(_while_param, var, block) => eval_if_while(eval_expr(*var), block),
        Expr::If(if_param, block) => eval_if_while(eval_expr(*if_param), block),

        Expr::Param(param, _param_type) => match *param {
            Expr::Str(param) => {
                insert_var(Content::Str(param.clone()), Content::Str(param.clone()))
            }
            _ => panic!("Invalid Input!"),
        },

        Expr::FuncInput(var, func_name, block) => match *func_name {
            Expr::Str(func_name) => eval_func_input(eval_expr(*var), &func_name.to_string(), block),
            _ => panic!("Invalid Input!"),
        },

        Expr::Let(left, operator, right) => match *left {
            Expr::Num(left) => eval_i32(
                eval_expr(Expr::Num(left)),
                eval_expr(*operator),
                eval_expr(*right),
            ),
            Expr::Bool(left) => eval_bool(
                eval_expr(Expr::Bool(left)),
                eval_expr(*operator),
                eval_expr(*right),
            ),
            Expr::Str(left) => eval_let(
                eval_expr(Expr::Str(left)),
                eval_expr(*operator),
                eval_expr(*right),
            ),
            _ => (panic!("Invalid input!")),
        },
        // _ => (panic!("Invalid input!")),
    }
}

fn eval_if_while(if_param: Content, block: Vec<Expr>) -> Content {
    match if_param {
        Content::Bool(true) => eval_block(block),
        Content::Bool(false) => Content::Null,
        Content::Str(s) => match read_from_var(&s.to_string()) {
            Content::Bool(true) => eval_block(block),
            _ => Content::Null,
        },
        _ => (panic!("Invalid input!")),
    }
}

fn eval_params(params: Vec<Expr>, args: Vec<Expr>) {
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

        insert_var(name, eval_expr(args[i].clone()));
    }
}

fn eval_func(func_name: Content, params: Vec<Expr>, block: Vec<Expr>) -> Content {
    let v = vec![params, block];
    insert_function(func_name, v);

    return Content::Null;
}

fn eval_func_input(var: Content, func_name: &str, args: Vec<Expr>) -> Content {
    let func_content = read_from_func(func_name).1;
    let params = func_content[0].clone();
    let block = func_content[1].clone();

    eval_params(params, args);
    let result = eval_block(block);
    let var_name = match var {
        Content::Str(var) => var,
        _ => panic!("Error: Invalid block input!"),
    };

    let value = match result {
        Content::Return(_, var) => var,
        _ => panic!("Error: Block result not found!"),
    };

    return Content::Return(var_name.to_string(), value);
}

fn eval_return(return_param: &str, var: &str) -> Content {
    insert_var(
        Content::Str(return_param.to_string()),
        Content::Str(var.to_string()),
    );

    let value = read_from_var(var);
    return Content::Return(var.to_string(), Box::new(value));
}

fn eval_block(block: Vec<Expr>) -> Content {
    let mut res: Content = Content::Null;
    for expr in block.iter() {
        res = eval_expr(expr.clone());
        match res {
            Content::Return(_, _) => break,
            _ => continue,
        }
    }
    return res;
}

fn eval_i32(left: Content, operator: Content, right: Content) -> Content {
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
        _ => panic!("Invalid input!"),
    }
}

fn eval_bool(left: Content, operator: Content, right: Content) -> Content {
    match (left, operator, right) {
        (Content::Bool(left), Content::ContentOp(ContentOp::And), Content::Bool(right)) => {
            Content::Bool(left && right)
        }
        (Content::Bool(left), Content::ContentOp(ContentOp::Or), Content::Bool(right)) => {
            Content::Bool(left || right)
        }
        _ => panic!("Invalid input!"),
    }
}

fn eval_let(left: Content, operator: Content, right: Content) -> Content {
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
        _ => panic!("Invalid input!"),
    }
}

#[cfg(test)]
mod interp_tests {
    use super::*;

    #[test]
    fn test_interp() {
        assert_eq!(eval_expr(Expr::Num(1)), Content::Num(1));
        assert_eq!(eval_expr(Expr::Bool(true)), Content::Bool(true));
    }

    #[test]
    fn test_interp_node() {
        assert_eq!(
            eval_expr(Expr::Let(
                Box::new(Expr::Num(2)),
                Box::new(Expr::ArithOp(ArithOp::Mult)),
                Box::new(Expr::Num(3))
            )),
            Content::Num(6)
        );
        assert_eq!(
            eval_expr(Expr::Let(
                Box::new(Expr::Bool(true)),
                Box::new(Expr::LogicOp(LogicOp::And)),
                Box::new(Expr::Bool(false))
            )),
            Content::Bool(false)
        );
    }
}
