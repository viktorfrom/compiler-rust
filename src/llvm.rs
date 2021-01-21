use crate::ast::*;
use crate::memory::*;

extern crate inkwell;

use self::inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    passes::PassManager,
    types::BasicTypeEnum,
    values::{
        BasicValue, BasicValueEnum, FloatValue, FunctionValue, InstructionValue, IntValue,
        PointerValue,
    },
    FloatPredicate, IntPredicate, OptimizationLevel,
};

use core::panic;
use std::{
    borrow::Borrow,
    collections::HashMap,
    error::Error,
    io::{self, Write},
    iter::Peekable,
    ops::DerefMut,
    str::Chars,
};

type ExprFunc = unsafe extern "C" fn() -> i32;

// // ======================================================================================
// // COMPILER =============================================================================
// // ======================================================================================

// /// Defines the `Expr` compiler.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub execution_engine: &'a ExecutionEngine<'ctx>,
    pub fn_value_opt: Option<FunctionValue<'ctx>>,

    variables: HashMap<String, PointerValue<'ctx>>,
    statement: (InstructionValue<'ctx>, bool),
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    #[inline]
    fn get_variable(&self, name: &str) -> &PointerValue<'ctx> {
        match self.variables.get(name) {
            Some(var) => var,
            None => panic!("ERROR: Can't find matching variable"),
        }
    }

    fn create_entry_block_alloca(&mut self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let alloca = builder.build_alloca(self.context.i32_type(), name);
        self.variables.insert(name.to_string(), alloca);
        alloca
    }

    fn compile_expr(&mut self, expr: &Expr) -> (InstructionValue<'ctx>, bool) {
        match expr.clone() {
            Expr::Let(left, _, right) => match *left {
                Expr::Var(left) => {
                    let alloca = self.create_entry_block_alloca(&left);
                    let expr = self.compile_stmt(*right);
                    let store = self.builder.build_store(alloca, expr);

                    (store, false)
                }
                _ => panic!("Invalid Expr!"),
            },
            Expr::VarExpr(var, op, expr) => (self.compile_var_op(*var, op, *expr), false),
            // Expr::VarOp(var, op, expr) => (self.compile_var_op(*var, op, *expr), false),
            // Expr::If(cond, block) => (self.compile_if(*cond, block), false),
            // Expr::While(cond, block) => (self.compile_while(*cond, block), false),
            // Expr::Return(expr) => {
            //     let val = self.compile_expr(*expr);
            //     (self.builder.build_return(Some(&val)), true)
            // }
            // Expr::FuncCall(_) => (self.compile_expr(keyword).as_instruction().unwrap(), false),
            Expr::Return(expr) => {
                let var = self.compile_stmt(*expr);
                (self.builder.build_return(Some(&var)), true)
            }

            _ => panic!("Invalid compile expr"),
        }
    }

    fn compile_stmt(&mut self, expr: Expr) -> IntValue<'ctx> {
        match expr.clone() {
            Expr::Var(var) => {
                if var != "" {
                    let val = self.get_variable(&var);
                    self.builder.build_load(*val, &var).into_int_value()
                } else {
                    let alloca = self.create_entry_block_alloca("empty");
                    let val = self.compile_stmt(Expr::Int(0));
                    self.builder.build_store(alloca, val);

                    let ptr_val = self.get_variable("empty");
                    self.builder.build_load(*ptr_val, &var).into_int_value()
                }
            }
            Expr::Int(i) => self.compile_int(i),
            Expr::Bool(b) => self.compile_bool(b),

            Expr::BinExpr(l, op, r) => self.compile_bin_expr(*l, op, *r),

            _ => panic!("invalid compile stmt!"),
        }
    }

    fn compile_var_op(&mut self, var: Expr, op: Op, expr: Expr) -> InstructionValue<'ctx> {
        let val = self.compile_stmt(expr.clone());

        match (var, op, expr) {
            (Expr::Var(v), Op::AssOp(AssOp::Eq), Expr::Int(_)) => {
                let var_ptr = self.get_variable(&v);

                self.builder.build_store(*var_ptr, val)
            }

            _ => panic!("Invalid Var op!"),
        }
    }

    fn compile_bin_expr(&mut self, l: Expr, op: Op, r: Expr) -> IntValue<'ctx> {
        match (l.clone(), r.clone()) {
            (Expr::Int(_), Expr::Int(_)) => {
                let left = self.compile_stmt(l);
                let right = self.compile_stmt(r);
                self.compile_int_expr(left, op, right)
            }
            (Expr::Var(_), Expr::Int(_)) => {
                return self.compile_stmt(r);
            }
            (Expr::Bool(_), Expr::Bool(_)) => {
                let left = self.compile_stmt(l);
                let right = self.compile_stmt(r);
                self.compile_bool_expr(left, op, right)
            }
            (Expr::Var(_), Expr::Bool(_)) => {
                return self.compile_stmt(r);
            }
            (_, Expr::BinExpr(_, _, _)) => self.compile_stmt(r),
            _ => panic!("Invalid Bin expr!"),
        }
    }

    fn compile_bool_expr(
        &mut self,
        l: IntValue<'ctx>,
        op: Op,
        r: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        match op {
            Op::LogOp(LogOp::And) => self.builder.build_and(l, r, "and"),
            // Op::LogOp(LogOp::Or) => ExprRep::Bool(l || r),
            // Op::RelOp(RelOp::Eq) => ExprRep::Bool(l == r),
            // Op::RelOp(RelOp::Neq) => ExprRep::Bool(l != r),
            // Op::RelOp(RelOp::Leq) => ExprRep::Bool(l <= r),
            // Op::RelOp(RelOp::Geq) => ExprRep::Bool(l >= r),
            // Op::RelOp(RelOp::Les) => ExprRep::Bool(l < r),
            // Op::RelOp(RelOp::Gre) => ExprRep::Bool(l > r),
            _ => panic!("Invalid Bool expr!"),
            // BoolToken::And => self.builder.build_and(l, r, "and"),
            // BoolToken::Or => self.builder.build_or(l, r, "or"),
        }
    }

    fn compile_int_expr(&self, l: IntValue<'ctx>, op: Op, r: IntValue<'ctx>) -> IntValue<'ctx> {
        match op {
            Op::AriOp(AriOp::Add) => self.builder.build_int_add(l, r, "add"),
            // Op::AriOp(AriOp::Sub) => self.builder.build_int_add(l, r, "add"),
            // Op::AriOp(AriOp::Div) => self.builder.build_int_add(l, r, "add"),
            // Op::AriOp(AriOp::Mul) => self.builder.build_int_add(l, r, "add"),
            // Op::RelOp(RelOp::Eq) =>  self.builder.build_int_add(l, r, "add"),
            // Op::RelOp(RelOp::Neq) => self.builder.build_int_add(l, r, "add"),
            // Op::RelOp(RelOp::Leq) => self.builder.build_int_add(l, r, "add"),
            // Op::RelOp(RelOp::Geq) => self.builder.build_int_add(l, r, "add"),
            // Op::RelOp(RelOp::Les) => self.builder.build_int_add(l, r, "add"),
            // Op::RelOp(RelOp::Gre) => self.builder.build_int_add(l, r, "add"),
            _ => panic!("Invalid Int expr!"),
        }
    }

    //     fn compile_bin_op(&mut self, l: Expr, op: Box<Expr>, r: Expr) -> IntValue<'ctx> {
    //         let l_val = self.compile_stmt(l);
    //         let r_val = self.compile_stmt(r);

    //         match *op {
    //             Expr::LogicOp(op) => self.compile_logic_op(l_val, op, r_val),
    //             Expr::ArithOp(op) => self.compile_arith_op(l_val, op, r_val),
    //             Expr::RelOp(op) => self.compile_rel_op(l_val, op, r_val),
    //             _ => panic!("Not a valid expression"),
    //         }
    //     }

    //     fn compile_if(&mut self, condition: Box<Expr>, block: Vec<Expr>) -> InstructionValue<'ctx> {
    //         let cond = self.compile_stmt(*condition);
    //         let then_block = self.context.append_basic_block(self.fn_value(), "then");
    //         let cont_block = self.context.append_basic_block(self.fn_value(), "cont");

    //         self.builder
    //             .build_conditional_branch(cond, then_block, cont_block);
    //         self.builder.position_at_end(then_block);
    //         self.compile_block(block);

    //         self.builder.build_unconditional_branch(cont_block);
    //         self.builder.position_at_end(cont_block);

    //         let phi = self.builder.build_phi(self.context.i32_type(), "iftmp");
    //         phi.add_incoming(&[
    //             (&self.compile_num(0), then_block),
    //             (&self.compile_num(0), cont_block),
    //         ]);
    //         phi.as_instruction()
    //     }

    //     fn compile_while(&mut self, condition: Box<Expr>, block: Vec<Expr>) -> InstructionValue<'ctx> {
    //         let do_block = self.context.append_basic_block(self.fn_value(), "do");
    //         let cont_block = self.context.append_basic_block(self.fn_value(), "cont");

    //         self.builder.build_conditional_branch(
    //             self.compile_stmt(*condition.clone()),
    //             do_block,
    //             cont_block,
    //         );
    //         self.builder.position_at_end(do_block);
    //         self.compile_block(block);

    //         self.builder.build_conditional_branch(
    //             self.compile_stmt(*condition.clone()),
    //             do_block,
    //             cont_block,
    //         );
    //         self.builder.position_at_end(cont_block);

    //         // This phi node does nothing, used to return an InstructionValue
    //         let phi = self.builder.build_phi(self.context.i32_type(), "whiletmp");
    //         phi.add_incoming(&[
    //             (&self.compile_num(0), do_block),
    //             (&self.compile_num(0), do_block),
    //         ]);
    //         phi.as_instruction()
    //     }

    fn compile_int(&self, int: i32) -> IntValue<'ctx> {
        self.context.i32_type().const_int(int as u64, false)
    }

    fn compile_bool(&self, b: bool) -> IntValue<'ctx> {
        match b {
            true => self.context.bool_type().const_int(1, false),
            false => self.context.bool_type().const_int(0, false),
        }
    }

    //     fn compile_rel_op(&self, l: IntValue<'ctx>, op: RelOp, r: IntValue<'ctx>) -> IntValue<'ctx> {
    //         match op {
    //             RelOp::EqEq => self
    //                 .builder
    //                 .build_int_compare(IntPredicate::EQ, l, r, "tmpEq"),
    //             RelOp::NotEq => self
    //                 .builder
    //                 .build_int_compare(IntPredicate::NE, l, r, "tmpneq"),
    //             RelOp::LesEq => self
    //                 .builder
    //                 .build_int_compare(IntPredicate::SLE, l, r, "tmpleq"),
    //             RelOp::GreEq => self
    //                 .builder
    //                 .build_int_compare(IntPredicate::SGE, l, r, "tmpgeq"),
    //             RelOp::Gre => self
    //                 .builder
    //                 .build_int_compare(IntPredicate::SGT, l, r, "tmpgre"),
    //             RelOp::Les => self
    //                 .builder
    //                 .build_int_compare(IntPredicate::SLT, l, r, "tmples"),
    //         }
    //     }

    //     fn compile_arith_op(
    //         &self,
    //         l: IntValue<'ctx>,
    //         op: ArithOp,
    //         r: IntValue<'ctx>,
    //     ) -> IntValue<'ctx> {
    //         match op {
    //             ArithOp::Add => self.builder.build_int_add(l, r, "tmpadd"),
    //             ArithOp::Sub => self.builder.build_int_sub(l, r, "tmpsub"),
    //             ArithOp::Mult => self.builder.build_int_mul(l, r, "tmpmul"),
    //             ArithOp::Div => self.builder.build_int_signed_div(l, r, "tmpdiv"),
    //         }
    //     }

    //     fn compile_logic_op(
    //         &self,
    //         l: IntValue<'ctx>,
    //         op: LogicOp,
    //         r: IntValue<'ctx>,
    //     ) -> IntValue<'ctx> {
    //         match op {
    //             LogicOp::And => self.builder.build_and(l, r, "and"),
    //             LogicOp::Or => self.builder.build_or(l, r, "or"),
    //             LogicOp::Not => self.builder.build_not(r, "not"), // TODO: Not sure about this one!
    //         }
    //     }

    //     // fn compile_assign_op(&self, op: AssignOp, l: IntValue<'ctx>, r: IntValue<'ctx>) -> IntValue {
    //     // // TODO: Implement assign operators
    //     // }

    fn compile_block(&mut self, block: Vec<Expr>) -> InstructionValue {
        let mut last_cmd: Option<InstructionValue> = None;

        for expr in block.iter() {
            self.statement = self.compile_expr(expr);
            println!("llvm-expr = {:#?}", expr);

            if self.statement.1 {
                return self.statement.0;
            }
            last_cmd = Some(self.statement.0);
        }

        match last_cmd {
            Some(instruction) => instruction,
            None => panic!(),
        }
    }
}

pub fn llvm(ast: Vec<Expr>) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("llvm-program");
    let builder = context.create_builder();
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let mut compiler = Compiler {
        context: &context,
        builder: &builder,
        module: &module,
        execution_engine: &execution_engine,
        fn_value_opt: None,
        variables: HashMap::new(),

        statement: (builder.build_return(None), false),
    };

    for expr in ast {
        let fn_var: String;
        let params: Vec<(Expr, Type)>;
        let ret_type: Type;
        let block: Vec<Expr>;

        match expr {
            Expr::Fn(n, p, t, b) => match *n {
                Expr::Var(func_name) => {
                    fn_var = func_name.to_string();
                    params = p;
                    ret_type = t;
                    block = b;
                }
                _ => continue,
            },
            _ => continue,
        }

        let u32_type = context.i32_type();
        let fn_type = u32_type.fn_type(&[], false);
        let function = compiler.module.add_function(&*fn_var, fn_type, None);
        let basic_block = context.append_basic_block(function, "entry");

        compiler.fn_value_opt = Some(function);
        compiler.builder.position_at_end(basic_block);
        compiler.compile_block(block);
    }

    compiler.module.print_to_stderr();
    let compiled_program: JitFunction<ExprFunc> =
        unsafe { compiler.execution_engine.get_function("main").ok().unwrap() };

    unsafe {
        println!("llvm-result: {} ", compiled_program.call());
    }

    Ok(())
}

#[cfg(test)]
mod parse_tests {
    use super::*;
    use crate::parser::*;
    use crate::type_checker::*;

    #[test]
    fn test_llvm_return() {
        let p = parser("fn main() -> i32 { return 1 }").unwrap().1;
        let t = type_checker(p.clone());

        if t {
            assert!(llvm(p).is_ok());
        }

        let p = parser("fn main() -> bool { return true }").unwrap().1;
        let t = type_checker(p.clone());

        if t {
            assert!(llvm(p).is_ok());
        }
    }

    #[test]
    fn test_llvm_let() {
        let p = parser("fn main() -> i32 { let a: i32 = 1; return a }")
            .unwrap()
            .1;
        let t = type_checker(p.clone());

        if t {
            assert!(llvm(p).is_ok());
        }

        let p = parser("fn main() -> bool { let a: bool = true; return a }")
            .unwrap()
            .1;
        let t = type_checker(p.clone());

        if t {
            assert!(llvm(p).is_ok());
        }

        let p = parser("fn main() -> i32 { let a: i32 = 1 + 1; return a }")
            .unwrap()
            .1;
        let t = type_checker(p.clone());

        if t {
            assert!(llvm(p).is_ok());
        }

        let p = parser("fn main() -> bool { let a: bool = true && true; return a }")
            .unwrap()
            .1;
        let t = type_checker(p.clone());

        if t {
            assert!(llvm(p).is_ok());
        }
    }
}
