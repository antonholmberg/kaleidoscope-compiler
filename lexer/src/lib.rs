#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {
    Def,
    Extern,
    Identifier(&'a str),
    Number(f64),
    GroupL,
    GroupR,
    Separator,
    Plus,
}

pub struct ParseTokenError();

pub struct Lexer<'a> {
    source: &'a str,
}

pub struct TokenIterator<'a> {
    source: &'a str,
    from: usize,
    to: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer { source }
    }

    pub fn tokens(&self) -> TokenIterator<'a> {
        TokenIterator {
            source: self.source,
            from: 0,
            to: 0,
        }
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Token<'a>> {
        let token = loop {
            self.to += 1;
            if let (Some(current), next) = (
                self.source.get(self.from..self.to),
                self.source.get(self.to..self.to + 1),
            ) {
                match current {
                    "(" => break Some(Token::GroupL),
                    ")" => break Some(Token::GroupR),
                    "," => break Some(Token::Separator),
                    "+" => break Some(Token::Plus),
                    " " | "\n" => {
                        self.from += 1;
                        continue;
                    }
                    _ => match next {
                        Some(" ") | Some(")") | Some("(") | Some(",") | Some("\n") | Some("+")
                        | None => {
                            if current == "extern" {
                                break Some(Token::Extern);
                            }
                            if current == "def" {
                                break Some(Token::Def);
                            }

                            if let Ok(num) = current.parse::<f64>() {
                                break Some(Token::Number(num));
                            }

                            break Some(Token::Identifier(current));
                        }
                        _ => continue,
                    },
                }
            } else {
                break None;
            }
        };

        self.from = self.to;
        return token;
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};
    #[test]
    fn basic_lexing() {
        let input = "def hello";
        let lexer = Lexer::new(input);

        let output = lexer.tokens().collect::<Vec<Token>>();

        assert_eq!(vec![Token::Def, Token::Identifier("hello")], output);
    }

    #[test]
    fn argument_lexing() {
        let input = "def hello(x, y)";
        let lexer = Lexer::new(input);

        let output = lexer.tokens().collect::<Vec<Token>>();

        assert_eq!(
            vec![
                Token::Def,
                Token::Identifier("hello"),
                Token::GroupL,
                Token::Identifier("x"),
                Token::Separator,
                Token::Identifier("y"),
                Token::GroupR
            ],
            output
        )
    }

    #[test]
    fn junk_code() {
        let input = "(def hello(x, y)";
        let lexer = Lexer::new(input);

        let output = lexer.tokens().collect::<Vec<Token>>();

        assert_eq!(
            vec![
                Token::GroupL,
                Token::Def,
                Token::Identifier("hello"),
                Token::GroupL,
                Token::Identifier("x"),
                Token::Separator,
                Token::Identifier("y"),
                Token::GroupR
            ],
            output
        )
    }

    #[test]
    fn numbers() {
        let input = "1 2 3 4";
        let lexer = Lexer::new(input);

        let output = lexer.tokens().collect::<Vec<Token>>();

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
        let lexer = Lexer::new(input);

        let output = lexer.tokens().collect::<Vec<Token>>();

        assert_eq!(
            vec![Token::Number(1.0), Token::Plus, Token::Number(1.0)],
            output
        )
    }

    #[test]
    fn add_operator_no_spaces() {
        let input = "1+1";
        let lexer = Lexer::new(input);

        let output = lexer.tokens().collect::<Vec<Token>>();

        assert_eq!(
            vec![Token::Number(1.0), Token::Plus, Token::Number(1.0)],
            output
        )
    }
}
