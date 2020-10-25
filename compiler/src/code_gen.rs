extern crate inkwell;

use std::collections::HashMap;
use std::path::Path;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{BasicValue, FloatValue, FunctionValue};
use inkwell::types::BasicTypeEnum;
use inkwell::basic_block::BasicBlock;
use parser::Expr;

pub struct GeneratedCode<'a>(Module<'a>);

pub trait IntoGeneratedCode {
    fn generate_code<'a>(self, ctx: &'a Context) -> GeneratedCode<'a>;
}

impl <'a> GeneratedCode<'a> {
    pub fn write_output(&self, path: &Path) {
        self.0.write_bitcode_to_path(path);
    }
}

impl <'b, I> IntoGeneratedCode for I where I: IntoIterator<Item=Expr<'b>> {
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
    main_block: BasicBlock<'a>
}

impl <'a> CodeGenerator<'a> {

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
                let argument_types = std::iter::repeat(f64_type.into()).take(proto.argument_names.len()).collect::<Vec<BasicTypeEnum>>();
                let fn_type = f64_type.fn_type(&argument_types, false);
                let function = self.module.add_function(proto.name, fn_type, None);

                let mut bindings = HashMap::new();
                for (index, &name) in proto.argument_names.iter().enumerate() {
                    bindings.insert(name, function.get_nth_param(index as u32).unwrap().into_float_value());
                }

                let function_block = self.ctx.append_basic_block(function, "entry");

                self.generate_for_function(&function_block, bindings, body);
            }
            expr => {
                self.generate_for_main(expr);
            }
        }
    }

    fn generate_for_main<'b>(&self, expr: Expr<'b>) {
        self.builder.position_at_end(self.main_block)
    }

    fn generate_expression<'b>(&self, expr: Expr<'b>, bindings: &mut HashMap<&'b str, FloatValue<'a>>) -> FloatValue<'a> {
        match expr {
            Expr::Number(num) => {
                self.ctx.f64_type().const_float(num)
            },
            Expr::Variable(ident) => bindings[ident],
            Expr::Binary { rhs, lhs, operator } => {
                let lhs = self.generate_expression(*lhs, bindings);
                let rhs = self.generate_expression(*rhs, bindings);

                self.builder.build_float_add(lhs, rhs, "sum")
            }
            _ => todo!("Implement"),
        }
    }

    fn generate_for_function<'b>(&self, function_block: &BasicBlock<'a>, bindings: HashMap<&'b str, FloatValue<'a>>, body: Box<Expr<'b>>) {
    }
}
