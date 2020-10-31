extern crate anyhow;
extern crate inkwell;

pub mod code_gen;

use inkwell::context::Context;

use code_gen::IntoGeneratedCode;
use lexer::Lexer;
use parser::{Expr, IntoParsingIterator};

fn main() -> anyhow::Result<()> {
    let context = Context::create();
    let code: &'static str = "def add(x, y)\n x + y\nadd(1, 2)";
    Lexer::new(code)
        .tokens()
        .parse_ast()
        .collect::<Result<Vec<Expr>, _>>()?
        .generate_code(&context)
        .execute_jit_main();

    Ok(())
}
