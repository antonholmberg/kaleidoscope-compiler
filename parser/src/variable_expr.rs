use super::ExprAST;

use lexer::Token;

#[derive(Eq, PartialEq, Debug)]
pub struct VariableExprAST<'a> {
  name: &'a str,
}

impl<'a> VariableExprAST<'a> {
  pub fn new(tokens: &[Token<'a>]) -> Result<Self, &'static str> {
    if tokens.len() != 1 {
      return Err("Too many tokens passed to from_tokens");
    }

    if let Some(Token::Identifier(ident)) = tokens.first() {
      return Ok(VariableExprAST { name: ident });
    } else {
      return Err("Expected identifier in variable expression");
    }
  }
}

impl<'a> ExprAST<'a> for VariableExprAST<'a> {}

#[cfg(test)]
mod tests {
  use super::Token;
  use super::VariableExprAST;

  #[test]
  fn parses_from_token() {
    let input = vec![Token::Identifier("hello")];
    let output = VariableExprAST::new(&input);

    assert_eq!(Ok(VariableExprAST { name: "hello" }), output);
  }

  #[test]
  fn fails_on_too_many_tokens() {
    let input = vec![Token::Identifier("hello"), Token::Identifier("hello")];
    let output = VariableExprAST::new(&input);

    assert!(output.is_err())
  }

  #[test]
  fn fails_on_non_identifier() {
    let input = vec![Token::Plus];
    let output = VariableExprAST::new(&input);

    assert!(output.is_err())
  }
}
