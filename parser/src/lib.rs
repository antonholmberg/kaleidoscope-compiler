mod variable_expr;

use lexer::Token;
use std::fmt::Debug;
use std::rc::Rc;

pub trait ExprAST<'a>: Debug {}

pub struct Parser<'a, 'b> {
    source: &'b mut dyn Iterator<Item = Token<'a>>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(iter: &'b mut dyn Iterator<Item = Token<'a>>) -> Self {
        Parser { source: iter }
    }
}

impl<'a, 'b> Iterator for Parser<'a, 'b> {
    type Item = Result<Rc<dyn ExprAST<'a> + 'a>, &'a str>;
    fn next(&mut self) -> Option<Self::Item> {
        let tokens = self.source.take(1).collect::<Vec<Token<'a>>>();
        let ast = variable_expr::VariableExprAST::new(&tokens);
        match ast {
            Ok(ast) => Some(Ok(Rc::new(ast))),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use super::Token;
    #[test]
    fn it_works() {
        let mut input = vec![Token::Identifier("hello")].into_iter();
        let mut parser = Parser::new(&mut input);

        assert!(parser.next().map(|r| r.is_ok()).unwrap_or(false))
    }
}
