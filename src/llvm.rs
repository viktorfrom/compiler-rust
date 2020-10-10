use crate::ast::expr_tree::*;
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

// ======================================================================================
// COMPILER =============================================================================
// ======================================================================================

/// Defines the `Expr` compiler.
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

    /// Returns the `FunctionValue` representing the function being compiled.
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

    /// Creates a new stack allocation instruction in the entry block of the function.
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
                Expr::Str(left) => {
                    let alloca = self.create_entry_block_alloca(&left);
                    let expr = self.compile_stmt(*right);
                    let store = self.builder.build_store(alloca, expr);

                    (store, false)
                }
                _ => panic!("Invalid Expr!"),
            },
            Expr::Return(_, expr) => {
                let var = self.compile_stmt(*expr);
                (self.builder.build_return(Some(&var)), true)
            }
            Expr::If(cond, b) => {
                    (self.compile_if(cond, b), false)
                },
            Expr::While(_while_param, var, block) => {
                (self.compile_while(var, block), false)
            },
            _ => panic!("Invalid Expr!"),
        }
    }

    fn compile_stmt(&self, expr: Expr) -> IntValue<'ctx> {
        match expr.clone() {
            Expr::Str(var) => {
                let val = self.get_variable(&var);
                self.builder.build_load(*val, &var).into_int_value()
            }
            Expr::Num(i) => self.compile_num(i),
            Expr::Bool(b) => self.compile_bool(b),

            Expr::Let(l, op, r) => {
                self.compile_bin_op(*l, op, *r)
            },

            _ => unimplemented!(),
        }
    }

    fn compile_bin_op(&self, l: Expr, op: Box<Expr>, r: Expr) -> IntValue<'ctx> {
        let l_val = self.compile_stmt(l);
        let r_val = self.compile_stmt(r);

        match *op {
            Expr::LogicOp(op) => self.compile_logic_op(l_val, op, r_val),
            Expr::ArithOp(op) => self.compile_arith_op(l_val, op, r_val),
            Expr::RelOp(op) => self.compile_rel_op(l_val, op, r_val),
            _ => panic!("Not a valid expression"),
        }
    }


    fn compile_if(&mut self, condition: Box<Expr>, block: Vec<Expr>) -> InstructionValue<'ctx> {
        let cond = self.compile_stmt(*condition);
        let then_block = self.context.append_basic_block(self.fn_value(), "then");
        let cont_block = self.context.append_basic_block(self.fn_value(), "cont");

        self.builder
            .build_conditional_branch(cond, then_block, cont_block);
        self.builder.position_at_end(then_block);
        self.compile_block(block);

        self.builder.build_unconditional_branch(cont_block);
        self.builder.position_at_end(cont_block);

        let phi = self.builder.build_phi(self.context.i32_type(), "iftmp");
        phi.add_incoming(&[
            (&self.compile_num(0), then_block),
            (&self.compile_num(0), cont_block),
        ]);
        phi.as_instruction()
    }

    fn compile_while(&mut self, condition: Box<Expr>, block: Vec<Expr>) -> InstructionValue<'ctx> {
        let do_block = self.context.append_basic_block(self.fn_value(), "do");
        let cont_block = self.context.append_basic_block(self.fn_value(), "cont");

        self.builder.build_conditional_branch(
            self.compile_stmt(*condition.clone()),
            do_block,
            cont_block,
        );
        self.builder.position_at_end(do_block);
        self.compile_block(block);

        self.builder.build_conditional_branch(
            self.compile_stmt(*condition.clone()),
            do_block,
            cont_block,
        );
        self.builder.position_at_end(cont_block);

        // This phi node does nothing, used to return an InstructionValue
        let phi = self.builder.build_phi(self.context.i32_type(), "whiletmp");
        phi.add_incoming(&[
            (&self.compile_num(0), do_block),
            (&self.compile_num(0), do_block),
        ]);
        phi.as_instruction()
    }

    fn compile_bool(&self, b: bool) -> IntValue<'ctx> {
        match b {
            true => self.context.bool_type().const_int(1, false),
            false => self.context.bool_type().const_int(0, false),
        }
    }

    fn compile_num(&self, num: i32) -> IntValue<'ctx> {
        self.context.i32_type().const_int(num as u64, false)
    }

    fn compile_rel_op(&self, l: IntValue<'ctx>, op: RelOp, r: IntValue<'ctx>) -> IntValue<'ctx> {
        match op {
            RelOp::EquEqu => self
                .builder
                .build_int_compare(IntPredicate::EQ, l, r, "tmpequ"),
            RelOp::NotEqu => self
                .builder
                .build_int_compare(IntPredicate::NE, l, r, "tmpneq"),
            RelOp::LesEqu => self
                .builder
                .build_int_compare(IntPredicate::SLE, l, r, "tmpleq"),
            RelOp::GreEqu => self
                .builder
                .build_int_compare(IntPredicate::SGE, l, r, "tmpgeq"),
            RelOp::Gre => self
                .builder
                .build_int_compare(IntPredicate::SGT, l, r, "tmpgre"),
            RelOp::Les => self
                .builder
                .build_int_compare(IntPredicate::SLT, l, r, "tmples"),
        }
    }

    fn compile_arith_op(&self, l: IntValue<'ctx>, op: ArithOp, r: IntValue<'ctx>) -> IntValue<'ctx> {
        match op {
            ArithOp::Add => self.builder.build_int_add(l, r, "tmpadd"),
            ArithOp::Sub => self.builder.build_int_sub(l, r, "tmpsub"),
            ArithOp::Mult => self.builder.build_int_mul(l, r, "tmpmul"),
            ArithOp::Div => self.builder.build_int_signed_div(l, r, "tmpdiv"),
        }
    }

    fn compile_logic_op(&self, l: IntValue<'ctx>, op: LogicOp, r: IntValue<'ctx>) -> IntValue<'ctx> {
        match op {
            LogicOp::And => self.builder.build_and(l, r, "and"),
            LogicOp::Or => self.builder.build_or(l, r, "or"),
            LogicOp::Not => self.builder.build_not(r, "not"), // TODO: Not sure about this one!
        }
    }

    // fn compile_assign_op(&self, op: AssignOp, l: IntValue<'ctx>, r: IntValue<'ctx>) -> IntValue {
    // // TODO: Implement assign operators
    // }

    fn compile_block(&mut self, block: Vec<Expr>) -> InstructionValue {
        let mut last_cmd: Option<InstructionValue> = None;

        for expr in block.iter() {
            self.statement = self.compile_expr(expr);

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

pub fn compiler(tree: Vec<Expr>) -> Result<(), Box<dyn Error>> {
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

    for scope in tree {
        let fn_name: String;
        let _fn_params: Vec<Expr>;
        let fn_block: Vec<Expr>;

        match scope {
            Expr::Func(func_name, _, _) => match *func_name {
                Expr::Str(func_name) => {
                    fn_name = func_name.to_string();

                    let fn_content = read_from_func(&fn_name).1;
                    _fn_params = fn_content[0].clone();
                    fn_block = fn_content[1].clone();
                }
                _ => continue,
            },
            _ => continue,
        }
        // println!("name {:#?}, block {:#?}", fn_name, fn_block);

        let u32_type = context.i32_type();
        let fn_type = u32_type.fn_type(&[], false);
        let function = compiler.module.add_function(&*fn_name, fn_type, None);
        let basic_block = context.append_basic_block(function, "entry");

        compiler.fn_value_opt = Some(function);
        compiler.builder.position_at_end(basic_block);
        compiler.compile_block(fn_block);
    }

    compiler.module.print_to_stderr();
    let compiled_program: JitFunction<ExprFunc> =
        unsafe {compiler.execution_engine.get_function("testfn").ok().unwrap()};

    unsafe {
        println!("test: {} ", compiled_program.call());
    }

    Ok(())
}
