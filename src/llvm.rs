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
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue> {
        self.module.get_function(name)
    }

    #[inline]
    fn get_variable(&self, name: &str) -> &PointerValue {
        match self.variables.get(name) {
            Some(var) => var,
            None => panic!(
                "Could not find a matching variable, {} in {:?}",
                name, self.variables
            ),
        }
    }

    #[inline]
    fn fn_value(&self) -> FunctionValue {
        self.fn_value_opt.unwrap()
    }

    fn codegen_num(&mut self, num: i32) -> IntValue {
        self.context.i32_type().const_int(num as u64, false)
    }

    fn codegen_bool(&mut self, b: bool) -> IntValue {
        match b {
            true => self.context.bool_type().const_int(1, false),
            false => self.context.bool_type().const_int(0, false),
        }
    }

    fn codegen_block(&mut self, block: Vec<Expr>) {}

    fn codegen_scope(&mut self, scope: Expr) -> Result<(), Box<Error>> {
        println!("sc = {:#?}", scope);

        let fn_name: String;
        let fn_params: Vec<Expr>;
        let fn_block: Vec<Expr>;

        match scope {
            Expr::Func(func_name, params, block) => match *func_name {
                Expr::Str(func_name) => {
                    fn_name = func_name.to_string();
                    fn_params = params;
                    fn_block = block;
                }
                _ => panic!("Invalid Input!"),
            },
            _ => panic!("ERROR: Can't find function head"),
        }

        // let u32_type = codegen.context.i32_type();
        // let fn_type = u32_type.fn_type(&[], false);
        // let function = codegen.module.add_function(&*fn_name, fn_type, None);
        // let basic_block = codegen.context.append_basic_block(function, "entry");
        // codegen.builder.position_at_end(basic_block);

        // codegen.codegen_block(fn_block);

        Ok(())
    }
}

pub fn compiler(tree: Vec<Expr>) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("llvm-program");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let mut codegen = CodeGen {
        context: &context,
        module: module,
        builder: context.create_builder(),
        execution_engine: execution_engine,
        fn_value_opt: None,
        variables: HashMap::new(),
    };

    for scope in tree {
        codegen.codegen_scope(scope)?;
    }

    codegen.module.print_to_stderr();
    let fun_expr: JitFunction<ExprFunc> = unsafe {
        codegen
            .execution_engine
            .get_function("testfn")
            .ok()
            .unwrap()
    };

    unsafe {
        println!("{}", fun_expr.call());
    }

    Ok(())
}
