use crate::ast::{content_tree::*, expr_tree::*};

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    targets::{InitializationConfig, Target},
    values::{BasicValueEnum, FloatValue, FunctionValue, InstructionValue, IntValue, PointerValue},
    OptimizationLevel,
};
use std::{collections::HashMap, error::Error};

/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type ExprFunc = unsafe extern "C" fn() -> i32;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    fn codegen_num(&mut self, num: i32) -> IntValue {
        self.context.i32_type().const_int(num as u64, false)
    }

    fn codegen_bool(&mut self, b: bool) -> IntValue {
        match b {
            true => self.context.bool_type().const_int(1, false),
            false => self.context.bool_type().const_int(0, false),
        }
    }

    fn codegen_scope(&mut self, scope: Expr) -> Result<(), Box<Error>> {
        println!("scope = {:#?}", scope);

        Ok(())
    }
}

pub fn compiler(tree: Vec<Expr>) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let mut codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
        fn_value_opt: None,
        variables: HashMap::new(),
    };

    for scope in tree {
        codegen.codegen_scope(scope);
    }

    // let sum = codegen.jit_compile_sum().ok_or("Unable to JIT compile `sum`")?;

    // let x = 1u64;
    // let y = 2u64;
    // let z = 3u64;

    // unsafe {
    //     println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
    //     assert_eq!(sum.call(x, y, z), x + y + z);
    // }

    Ok(())
}
