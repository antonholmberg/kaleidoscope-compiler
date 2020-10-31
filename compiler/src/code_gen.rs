use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, InstructionValue};
use inkwell::OptimizationLevel;
use parser::Expr;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::path::Path;

pub struct GeneratedCode<'a>(Module<'a>);

type KaleidoscopeMain = unsafe extern "C" fn() -> f64;

pub trait IntoGeneratedCode {
    fn generate_code<'a>(self, ctx: &'a Context) -> GeneratedCode<'a>;
}

impl<'a> GeneratedCode<'a> {
    pub fn write_output(&self, path: &Path) {
        self.0.write_bitcode_to_path(path);
    }

    pub fn execute_jit_main(&self) {
        let execution_engine = self
            .0
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let function: JitFunction<KaleidoscopeMain> =
            unsafe { execution_engine.get_function("kaleidoscope_main").ok() }.unwrap();
        unsafe {
            print!("Program evaluated to: {}", function.call());
        }
    }
}

impl<'b, I> IntoGeneratedCode for I
where
    I: IntoIterator<Item = Expr<'b>>,
{
    fn generate_code<'a>(self, ctx: &'a Context) -> GeneratedCode<'a> {
        let iterator = self.into_iter();
        let generator = CodeGenerator::new(ctx);

        for expr in iterator {
            generator.generate(expr);
        }

        GeneratedCode(generator.module)
    }
}

struct CodeGenerator<'a> {
    ctx: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    main_block: BasicBlock<'a>,
}

impl<'a> CodeGenerator<'a> {
    fn new(ctx: &'a Context) -> CodeGenerator {
        let module = ctx.create_module("kaleidoscope_module");
        let f64_type = ctx.f64_type();
        let main_type = f64_type.fn_type(&[], false);
        let main = module.add_function("kaleidoscope_main", main_type, None);
        let main_block = ctx.append_basic_block(main, "entry");
        CodeGenerator {
            ctx,
            module,
            builder: ctx.create_builder(),
            main_block,
        }
    }

    fn generate<'b>(&self, expr: Expr<'b>) {
        match expr {
            Expr::FunctionDefinition(proto, body) => {
                let f64_type = self.ctx.f64_type();
                let argument_types = std::iter::repeat(f64_type.into())
                    .take(proto.argument_names.len())
                    .collect::<Vec<BasicTypeEnum>>();
                let fn_type = f64_type.fn_type(&argument_types, false);
                let function = self.module.add_function(proto.name, fn_type, None);

                let mut bindings = HashMap::new();
                for (index, &name) in proto.argument_names.iter().enumerate() {
                    bindings.insert(name, function.get_nth_param(index as u32).unwrap());
                }

                let function_block = self.ctx.append_basic_block(function, "entry");

                self.generate_for_function(&function_block, &bindings, body);
            }
            expr => {
                self.generate_for_main(expr);
            }
        }
    }

    fn generate_for_main<'b>(&self, expr: Expr<'b>) -> InstructionValue<'a> {
        self.builder.position_at_end(self.main_block);
        let ret = self.generate_expression(expr, &mut HashMap::new());
        return self.builder.build_return(Some(&ret.as_basic_value_enum()));
    }

    fn generate_expression<'b>(
        &self,
        expr: Expr<'b>,
        bindings: &HashMap<&'b str, BasicValueEnum<'a>>,
    ) -> Box<dyn BasicValue<'a> + 'a> {
        match expr {
            Expr::Number(num) => Box::new(self.ctx.f64_type().const_float(num)),
            Expr::Variable(ident) => Box::new(bindings[ident]),
            Expr::Binary {
                rhs,
                lhs,
                operator: _,
            } => {
                let lhs = (*self.generate_expression(*lhs, bindings)).as_basic_value_enum();
                let rhs = (*self.generate_expression(*rhs, bindings)).as_basic_value_enum();

                Box::new(self.builder.build_float_add(
                    FloatValue::try_from(lhs).unwrap(),
                    FloatValue::try_from(rhs).unwrap(),
                    "sum",
                ))
            }
            Expr::FunctionCall { callee, arguments } => {
                let function = self.module.get_function(callee).unwrap();
                let mut arguments_code = Vec::<BasicValueEnum>::new();
                for argument in arguments {
                    arguments_code.push(
                        self.generate_expression(argument, &bindings)
                            .as_basic_value_enum(),
                    );
                }

                let call_result = self
                    .builder
                    .build_call(function, &arguments_code, "calltmp")
                    .try_as_basic_value()
                    .unwrap_left();
                Box::new(call_result)
            }
            _ => todo!("Implement"),
        }
    }

    fn generate_for_function<'b>(
        &self,
        function_block: &BasicBlock<'a>,
        bindings: &HashMap<&'b str, BasicValueEnum<'a>>,
        body: Box<Expr<'b>>,
    ) {
        self.builder.position_at_end(*function_block);
        let body = self.generate_expression(*body, &bindings);
        self.builder.build_return(Some(&body.as_basic_value_enum()));
    }
}
