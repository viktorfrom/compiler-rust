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
        BasicValue, BasicValueEnum, FloatValue, FunctionValue, InstructionValue, PointerValue, IntValue,
    },
    FloatPredicate, OptimizationLevel,
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

// const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";
// type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;
type ExprFunc = unsafe extern "C" fn() -> i32;

// ======================================================================================
// COMPILER =============================================================================
// ======================================================================================

/// Defines the `Expr` compiler.
pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    #[inline]
    fn get_variable(&self, name: &str) -> &PointerValue{
        match self.variables.get(name) {
            Some(var) => var,
            None => panic!("ERROR: Can't find matching variable")
        }
    }

    #[inline]
    fn fn_value(&self) -> FunctionValue {
        self.fn_value_opt.unwrap()
    }

    fn create_entry_block_alloca(&mut self, name: &str) -> PointerValue {
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

    pub fn compile_keyword(&self, expr: &Expr) -> (InstructionValue, bool) {
        println!("test  = {:#?}", expr);
        match expr.clone() {
            // Expr::Let(left, operator, right) => match *left {
            //     Expr::Str(left) => {

            //             let alloca = self.create_entry_block_alloca(&left);
            //             let expr = self.match_node(*right);
            //             let store = self.builder.build_store(alloca, expr);
                        

            //         (store, false)
            //     }
            //         // eval_expr(Expr::Str(left)),
            //         // eval_expr(*operator),
            //         // eval_expr(*right),
            //     _ => panic!("wops"),
            // },
            _ => panic!("wops"),
        } 
    }

    fn compile_expr(&self, expr: Expr) -> IntValue {
        match expr.clone() {
            Expr::Str(var) => {
                let val = self.get_variable(&var);
                self.builder.build_load(*val, &var).into_int_value()
            }
            Expr::Num(i) => self.compile_num(i),
            Expr::Bool(b) => {
                if b {
                    self.context.bool_type().const_int(1, false)
                } else {
                    self.context.bool_type().const_int(0, false)
                }
            }
            // Expr::BinOp(l, op, r) => self.compile_bin_op(*l, op, *r),
            // Expr::FuncCall(fn_call) => self.compile_function_call(fn_call),
            _ => unimplemented!(),
        }
    }

    fn match_node(&self, expr: Expr) -> IntValue{
        match expr{
            Expr::Str(name) => {
                let var = self.get_variable(&name);
                self.builder.build_load(*var, &name).into_int_value()
            }
            Expr::Num(num) => self.compile_num(num),
            
            Expr::Bool(b) => self.compile_bool(b),


            
            _ => panic!("ERROR: Can't match node")
        }
    }

    
    fn compile_bool(&self, b: bool) -> IntValue{
        match b {
            true => self.context.bool_type().const_int(1, false),
            false => self.context.bool_type().const_int(0, false)
        }
    }

    fn compile_num(&self, num: i32) -> IntValue{
        self.context.i32_type().const_int(num as u64, false)
    }

    pub fn compile_block(&self, block: Vec<Expr>) -> InstructionValue {
        // let mut res: Option<InstructionValue> = None;
        let mut last_cmd: Option<InstructionValue> = None;
        for expr in block.iter() {
            let (cmd, ret) = self.compile_keyword(expr);
            if ret {
                return cmd;
            }
            last_cmd = Some(cmd);
        }

        match last_cmd {
            Some(instruction) => instruction,
            None => panic!(),
        }
    }
}

pub fn compiler(tree: Vec<Expr>) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let builder = context.create_builder();

    let mut codegen = Compiler {
        context: &context,
        module: module,
        builder: builder,
        execution_engine: execution_engine,
        fn_value_opt: None,
        variables: HashMap::new(),
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
        let function = codegen.module.add_function(&*fn_name, fn_type, None);
        let basic_block = context.append_basic_block(function, "entry");

        codegen.builder.position_at_end(basic_block);
        codegen.compile_block(fn_block);
    }

    // codegen.module.print_to_stderr();
    // let compiled_program: JitFunction<ExprFunc> =
    //     unsafe {codegen.execution_engine.get_function("main").ok().unwrap()};

    // unsafe {
    //     println!("test: {} ", compiled_program.call());
    // }

    Ok(())
}
