use crate::ast::expr_tree::*;


extern crate inkwell;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::io::{self, Write};
use std::iter::Peekable;
use std::str::Chars;
use std::ops::DerefMut;

use self::inkwell::builder::Builder;
use self::inkwell::context::Context;
use self::inkwell::module::Module;
use self::inkwell::passes::PassManager;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use self::inkwell::types::BasicTypeEnum;
use self::inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use self::inkwell::{OptimizationLevel, FloatPredicate};

use std::error::Error;

const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;


// ======================================================================================
// COMPILER =============================================================================
// ======================================================================================

/// Defines the `Expr` compiler.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    // pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    // pub function: &'a Function,
        execution_engine: ExecutionEngine<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
        fn jit_compile_sum(&self) -> Option<JitFunction<SumFunc>> {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0)?.into_int_value();
        let y = function.get_nth_param(1)?.into_int_value();
        let z = function.get_nth_param(2)?.into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum");
        let sum = self.builder.build_int_add(sum, z, "sum");

        self.builder.build_return(Some(&sum));

        unsafe { self.execution_engine.get_function("sum").ok() }
    }
}


pub fn llvmmain() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;

    let codegen = Compiler {
        context: &context,
        module: &module,
        builder: &context.create_builder(),
        execution_engine,
        fn_value_opt: None,
        variables: HashMap::new(),
    };

    let sum = codegen.jit_compile_sum().ok_or("Unable to JIT compile `sum`")?;

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
        assert_eq!(sum.call(x, y, z), x + y + z);
    }

    Ok(())
}



















































// use inkwell::OptimizationLevel;
// use inkwell::builder::Builder;
// use inkwell::context::Context;
// use inkwell::execution_engine::{ExecutionEngine, JitFunction};
// use inkwell::module::Module;
// use inkwell::targets::{InitializationConfig, Target};
// use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, InstructionValue, IntValue, PointerValue};
// use std::error::Error;
// use std::collections::HashMap;


// /// Convenience type alias for the `sum` function.
// ///
// /// Calling this is innately `unsafe` because there's no guarantee it doesn't
// /// do `unsafe` operations internally.
// type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

// struct CodeGen<'ctx> {
//     context: &'ctx Context,
//     module: Module<'ctx>,
//     builder: Builder<'ctx>,
//     execution_engine: ExecutionEngine<'ctx>,
//     fn_value_opt: Option<FunctionValue<'ctx>>,
//     variables: HashMap<String, PointerValue<'ctx>>,
// }

// impl<'ctx> CodeGen<'ctx> {
//     fn jit_compile_sum(&self) -> Option<JitFunction<SumFunc>> {
//         let i64_type = self.context.i64_type();
//         let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
//         let function = self.module.add_function("sum", fn_type, None);
//         let basic_block = self.context.append_basic_block(function, "entry");

//         self.builder.position_at_end(basic_block);

//         let x = function.get_nth_param(0)?.into_int_value();
//         let y = function.get_nth_param(1)?.into_int_value();
//         let z = function.get_nth_param(2)?.into_int_value();

//         let sum = self.builder.build_int_add(x, y, "sum");
//         let sum = self.builder.build_int_add(sum, z, "sum");

//         self.builder.build_return(Some(&sum));

//         unsafe { self.execution_engine.get_function("sum").ok() }
//     }

//     fn compile_function(context: &Context, module: &Module, 
//                         builder: &Builder,
//                         execution_engine: &ExecutionEngine,
//                         scope: Expr) -> Result<(), Box<Error>> {

//         let fn_name: String; 
//         let fn_params: Vec<Expr>;
//         let fn_block: Vec<Expr>;

//         match scope {
//             Expr::Func(func_name, params, block) => match *func_name {
//                 Expr::Str(func_name) => {
//                     fn_name = func_name.to_string();
//                     fn_params = params;
//                     fn_block = block;
//                 }
//                 _ => panic!("Invalid Input!"),
//             },
//             _ => panic!("ERROR: Can't find function head"),
//         }


//         let u32_type = context.i32_type();
//         let fn_type = u32_type.fn_type(&[], false);
//         // let function = module.add_function(&*fn_name, fn_type, None);
//         // let basic_block = context.append_basic_block(&function, "entry");
//         // builder.position_at_end(&basic_block);



//         Ok(())
//     }
// }


// pub fn test() -> Result<(), Box<dyn Error>> {
//     let context = Context::create();
//     let module = context.create_module("sum");
//     let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
//     let codegen = CodeGen {
//         context: &context,
//         module,
//         builder: context.create_builder(),
//         execution_engine,
//         fn_value_opt: None,
//         variables: HashMap::new(),
//     };

//     let sum = codegen.jit_compile_sum().ok_or("Unable to JIT compile `sum`")?;

//     let x = 1u64;
//     let y = 2u64;
//     let z = 3u64;

//     unsafe {
//         println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
//         assert_eq!(sum.call(x, y, z), x + y + z);
//     }

//     Ok(())
// }