#[derive(PartialEq, Debug)]
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
    pub source: &'a str,
}

pub struct LexerIterator<'a> {
    source: &'a str,
    start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer { source: source }
    }

    pub fn iter(&self) -> LexerIterator<'a> {
        LexerIterator {
            source: self.source,
            start: 0,
        }
    }
}

impl<'a> Iterator for LexerIterator<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Token<'a>> {
        let mut offset = 0;
        let token = loop {
            offset += 1;

            if self.start + offset > self.source.len() {
                break None;
            }

            let current = &self.source[self.start..(self.start + offset)];
            let next = self.source.chars().nth(self.start + offset);

            match current {
                "(" => break Some(Token::GroupL),
                ")" => break Some(Token::GroupR),
                "," => break Some(Token::Separator),
                "+" => break Some(Token::Plus),
                " " | "\n" => {
                    self.start += 1;
                    offset -= 1;
                    continue;
                }
                _ => match next {
                    Some(' ') | Some(')') | Some('(') | Some(',') | Some('\n') | Some('+') | None => {
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
            };
        };

        self.start += offset;

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

        let output = lexer.iter().collect::<Vec<Token>>();

        assert_eq!(vec![Token::Def, Token::Identifier("hello")], output);
    }

    #[test]
    fn argument_lexing() {
        let input = "def hello(x, y)";
        let lexer = Lexer::new(&input);

        let output = lexer.iter().collect::<Vec<Token>>();

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

        let output = lexer.iter().collect::<Vec<Token>>();

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

        let output = lexer.iter().collect::<Vec<Token>>();

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

        let output = lexer.iter().collect::<Vec<Token>>();

        assert_eq!(
            vec![Token::Number(1.0), Token::Plus, Token::Number(1.0)],
            output
        )
    }

    #[test]
    fn add_operator_no_spaces() {
        let input = "1+1";
        let lexer = Lexer::new(input);

        let output = lexer.iter().collect::<Vec<Token>>();

        assert_eq!(
            vec![Token::Number(1.0), Token::Plus, Token::Number(1.0)],
            output
        )
    }
}
