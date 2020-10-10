mod variable_expr;

use lexer::Token;
use std::fmt::Debug;
use std::iter::FromIterator;

pub trait ExprAST: Debug {}

#[derive(Debug)]
struct RootAST {
    children: Vec<Box<dyn ExprAST>>
}

impl ExprAST for RootAST {}

impl RootAST {
    fn new() -> Self {
        RootAST { children: Vec::new() }
    }

    fn add(&mut self, ast: Box<dyn ExprAST>) {
        self.children.push(ast)
    }
}

pub struct Parser {
    source: Vec<Token>,
}

impl FromIterator<Token> for Parser {
    fn from_iter<I: IntoIterator<Item=Token>>(iter: I) -> Self {
        Parser { source: iter.into_iter().collect() }
    }
}

impl Parser {
    fn parse(self) -> impl ExprAST {
        let mut root = RootAST::new();

        for token in self.source {
            if let Ok(ast) = variable_expr::VariableExprAST::new(&[token]) {
                root.add(Box::new(ast));
            }
        }

        root
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use super::Token;
    
    use std::any::type_name;

    fn type_of<T>(_: T) -> &'static str {
            type_name::<T>()
    }

    #[test]
    fn it_works() {
        let input = vec![Token::Identifier("hello".to_string())].into_iter();
        let parser = input.collect::<Parser>();

        assert_eq!("parser::RootAST", type_of(parser.parse()))
    }
}
