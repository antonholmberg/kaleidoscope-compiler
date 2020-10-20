use lexer::Token;
use std::boxed::Box;
use std::fmt::Debug;
use std::iter::FromIterator;
use std::iter::Peekable;

#[derive(Eq, PartialEq, Debug)]
pub enum Operator {
    Plus,
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Variable(&'a str),
    Number(f64),
    Binary {
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
        operator: Operator,
    },
    FunctionCall {
        callee: &'a str,
        arguments: Vec<Expr<'a>>,
    },
}

fn parse_function_call<'a, I: Iterator<Item = Token<'a>>>(
    callee: &'a str,
    peekable: &mut Peekable<I>,
) -> Expr<'a> {
    let mut arguments: Vec<Expr<'a>> = Vec::new();
    peekable.next();

    loop {
        let peeked = peekable.peek();
        if peeked == Some(&Token::GroupR) || peeked == None {
            return Expr::FunctionCall { callee, arguments };
        }
        let mut argument_part: Vec<Token<'a>> = Vec::new();
        loop {
            match peekable.peek() {
                Some(Token::GroupR) | Some(Token::Separator) => break,
                None => panic!("Unclosed parenthesis"),
                Some(token) => {
                    argument_part.push(token.clone());
                }
            }
            peekable.next();
        }

        arguments.push(argument_part.into_iter().collect::<Expr<'a>>())
    }
}

fn parse_binary_expr<'a, I: Iterator<Item = Token<'a>>>(
    lhs: Expr<'a>,
    peekable: &mut Peekable<I>,
) -> Expr<'a> {
    let operator = peekable.next();

    if let Some(Token::Plus) = operator {
        let rhs = match peekable.peek() {
            Some(Token::Identifier(_)) => parse_identifier_expr(peekable),
            Some(Token::Number(_)) => parse_number_expr(peekable),
            _ => panic!("Error"),
        };

        return Expr::Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: Operator::Plus,
        };
    } else {
        panic!("Error")
    }
}

fn parse_identifier_expr<'a, I: Iterator<Item = Token<'a>>>(
    peekable: &mut Peekable<I>,
) -> Expr<'a> {
    if let Some(Token::Identifier(ident)) = peekable.next() {
        match peekable.peek() {
            Some(Token::GroupL) => parse_function_call(ident, peekable),
            Some(Token::Plus) => parse_binary_expr(Expr::Variable(ident), peekable),
            None => Expr::Variable(ident),
            _ => panic!("Error parsing identifier"),
        }
    } else {
        panic!("Error parsing identifier");
    }
}

fn parse_number_expr<'a, I: Iterator<Item = Token<'a>>>(peekable: &mut Peekable<I>) -> Expr<'a> {
    if let Some(Token::Number(number)) = peekable.next() {
        match peekable.peek() {
            Some(Token::Plus) => parse_binary_expr(Expr::Number(number), peekable),
            None => Expr::Number(number),
            _ => panic!("Error parsing identifier"),
        }
    } else {
        panic!("Error parsing identifier");
    }
}

impl<'a> FromIterator<Token<'a>> for Expr<'a> {
    fn from_iter<I: IntoIterator<Item = Token<'a>>>(iter: I) -> Self {
        let mut peekable = iter.into_iter().peekable();
        match peekable.peek() {
            Some(Token::Number(_)) => parse_number_expr(&mut peekable),
            Some(Token::Identifier(_)) => parse_identifier_expr(&mut peekable),
            _ => panic!("Error parsing"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Expr, Operator, Token};

    #[test]
    fn test_parse_number_expr() {
        let input = vec![Token::Number(1.0)].into_iter();
        let ast = input.collect::<Expr>();

        assert_eq!(Expr::Number(1.0), ast);
    }

    #[test]
    fn test_parse_binary_mixed_variable_and_number() {
        let input = vec![Token::Identifier("x"), Token::Plus, Token::Number(0.1)].into_iter();
        let ast = input.collect::<Expr>();

        assert_eq!(
            Expr::Binary {
                lhs: Box::new(Expr::Variable("x")),
                rhs: Box::new(Expr::Number(0.1)),
                operator: Operator::Plus
            },
            ast
        );
    }

    #[test]
    fn test_parse_binary_variable() {
        let input = vec![Token::Identifier("x"), Token::Plus, Token::Identifier("y")].into_iter();
        let ast = input.collect::<Expr>();

        assert_eq!(
            Expr::Binary {
                lhs: Box::new(Expr::Variable("x")),
                rhs: Box::new(Expr::Variable("y")),
                operator: Operator::Plus
            },
            ast
        );
    }

    #[test]
    fn test_nested_binary() {
        let input = vec![
            Token::Identifier("x"),
            Token::Plus,
            Token::Identifier("y"),
            Token::Plus,
            Token::Identifier("z"),
        ]
        .into_iter();
        let ast = input.collect::<Expr>();

        assert_eq!(
            Expr::Binary {
                lhs: Box::new(Expr::Variable("x")),
                rhs: Box::new(Expr::Binary {
                    lhs: Box::new(Expr::Variable("y")),
                    rhs: Box::new(Expr::Variable("z")),
                    operator: Operator::Plus
                }),
                operator: Operator::Plus
            },
            ast
        );
    }

    #[test]
    fn test_function_call() {
        let input = vec![
            Token::Identifier("print"),
            Token::GroupL,
            Token::Identifier("x"),
            Token::GroupR,
        ]
        .into_iter();

        let ast = input.collect::<Expr>();

        assert_eq!(
            Expr::FunctionCall {
                callee: "print",
                arguments: vec![Expr::Variable("x")]
            },
            ast
        )
    }

    #[test]
    fn test_function_call_with_expressions_in_arguments() {
        let input = vec![
            Token::Identifier("print"),
            Token::GroupL,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::GroupR,
        ]
        .into_iter();

        let ast = input.collect::<Expr>();

        assert_eq!(
            Expr::FunctionCall {
                callee: "print",
                arguments: vec![Expr::Binary {
                    lhs: Box::new(Expr::Number(1.0)),
                    rhs: Box::new(Expr::Number(2.0)),
                    operator: Operator::Plus
                }]
            },
            ast
        )
    }
}
