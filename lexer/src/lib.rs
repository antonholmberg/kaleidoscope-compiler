use std::str::Chars;
use std::iter::Peekable;

#[derive(PartialEq, Debug)]
pub enum Token {
    Def,
    Extern,
    Identifier(String),
    Number(f64),
    GroupL,
    GroupR,
    Separator,
    Plus,
}

pub struct ParseTokenError();

pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
}

impl <'a> Lexer<'a> {
    pub fn new(source: Chars<'a>) -> Self {
        Lexer { source: source.peekable() }
    }
}

impl <'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        let mut aggregate = String::new();
        let token = loop {
            if let (Some(current), next) = (self.source.next(), self.source.peek()) {
                match current {
                    '(' => break Some(Token::GroupL),
                    ')' => break Some(Token::GroupR),
                    ',' => break Some(Token::Separator),
                    '+' => break Some(Token::Plus),
                    ' ' | '\n' => continue,
                    _ => {
                        aggregate.push(current);
                        match next {
                            Some(' ') | Some(')') | Some('(') | Some(',') | Some('\n') | Some('+') | None => {
                                if aggregate == "extern" {
                                    break Some(Token::Extern);
                                }
                                if aggregate == "def" {
                                    break Some(Token::Def);
                                }

                                if let Ok(num) = aggregate.parse::<f64>() {
                                    break Some(Token::Number(num));
                                }

                                break Some(Token::Identifier(aggregate));
                            }
                            _ => continue,
                        }
                    }
                };
            } else {
                break None;
            }
        };

        return token;
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};
    #[test]
    fn basic_lexing() {
        let input = "def hello";
        let lexer = Lexer::new(input.chars());

        let output = lexer.collect::<Vec<Token>>();

        assert_eq!(vec![Token::Def, Token::Identifier("hello".to_string())], output);
    }

    #[test]
    fn argument_lexing() {
        let input = "def hello(x, y)";
        let lexer = Lexer::new(input.chars());

        let output = lexer.collect::<Vec<Token>>();

        assert_eq!(
            vec![
                Token::Def,
                Token::Identifier("hello".to_string()),
                Token::GroupL,
                Token::Identifier("x".to_string()),
                Token::Separator,
                Token::Identifier("y".to_string()),
                Token::GroupR
            ],
            output
        )
    }

    #[test]
    fn junk_code() {
        let input = "(def hello(x, y)";
        let lexer = Lexer::new(input.chars());

        let output = lexer.collect::<Vec<Token>>();

        assert_eq!(
            vec![
                Token::GroupL,
                Token::Def,
                Token::Identifier("hello".to_string()),
                Token::GroupL,
                Token::Identifier("x".to_string()),
                Token::Separator,
                Token::Identifier("y".to_string()),
                Token::GroupR
            ],
            output
        )
    }

    #[test]
    fn numbers() {
        let input = "1 2 3 4";
        let lexer = Lexer::new(input.chars());

        let output = lexer.collect::<Vec<Token>>();

        assert_eq!(
            vec![
                Token::Number(1.0),
                Token::Number(2.0),
                Token::Number(3.0),
                Token::Number(4.0)
            ],
            output
        )
    }

    #[test]
    fn add_operator() {
        let input = "1 + 1";
        let lexer = Lexer::new(input.chars());

        let output = lexer.collect::<Vec<Token>>();

        assert_eq!(
            vec![Token::Number(1.0), Token::Plus, Token::Number(1.0)],
            output
        )
    }

    #[test]
    fn add_operator_no_spaces() {
        let input = "1+1";
        let lexer = Lexer::new(input.chars());

        let output = lexer.collect::<Vec<Token>>();

        assert_eq!(
            vec![Token::Number(1.0), Token::Plus, Token::Number(1.0)],
            output
        )
    }
}
