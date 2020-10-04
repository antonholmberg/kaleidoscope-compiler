mod variable_expr;

use std::fmt::Debug;
use lexer::Token;

pub trait ExprAST<'a>: Sized + Eq + PartialEq + Debug { 
    fn from_tokens(tokens: &[Token<'a>]) -> Result<Self, &'static str>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
