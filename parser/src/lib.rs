extern crate thiserror;

use lexer::Token;
use std::boxed::Box;
use std::fmt::Debug;
use std::iter::Peekable;
use thiserror::Error;

#[derive(Eq, PartialEq, Debug)]
pub enum Operator {
    Plus,
}

#[derive(Debug, PartialEq)]
pub struct FunctionPrototype<'a> {
    pub name: &'a str,
    pub argument_names: Vec<&'a str>,
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
    ExternalDefinition(FunctionPrototype<'a>),
    FunctionDefinition(FunctionPrototype<'a>, Box<Expr<'a>>),
}

#[derive(Error, Debug, PartialEq)]
pub enum ParsingError<'a> {
    #[error("Unexpected Token")]
    UnexpectedToken(Token<'a>),
    #[error("Missing closing parenthesis")]
    UnmatchedParenthesis,
    #[error("Missing RHS expression")]
    MissingRhsExpression,
    #[error("Internal parsing error")]
    InternalParsingError,
    #[error("Missing argument separator")]
    MissingArgumentSeparator,
    #[error("Unexpected EOF")]
    UnexpectedEof,
}

pub trait IntoParsingIterator<'a, I: Iterator<Item = Token<'a>>> {
    fn parse_ast(self) -> ParsingIterator<'a, I>;
}

impl<'a, In: IntoIterator<Item = Token<'a>>> IntoParsingIterator<'a, In::IntoIter> for In {
    fn parse_ast(self) -> ParsingIterator<'a, In::IntoIter> {
        ParsingIterator {
            source_iterator: self.into_iter().peekable(),
        }
    }
}

pub struct ParsingIterator<'a, I: Iterator<Item = Token<'a>>> {
    source_iterator: Peekable<I>,
}

impl<'a, I> Iterator for ParsingIterator<'a, I>
where
    I: Iterator<Item = Token<'a>>,
{
    type Item = Result<Expr<'a>, ParsingError<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.source_iterator.peek() {
            Some(Token::Extern) => Some(self.next_extern_definition()),
            Some(Token::Def) => Some(self.next_function_definition()),
            Some(_) => Some(self.next_expression()),
            None => None,
        }
    }
}

impl<'a, I> ParsingIterator<'a, I>
where
    I: Iterator<Item = Token<'a>>,
{
    fn next_expression(&mut self) -> Result<Expr<'a>, ParsingError<'a>> {
        return loop {
            match self.source_iterator.peek() {
                Some(Token::Number(_)) => break Ok(self.next_numerical_expression()?),
                Some(Token::Identifier(_)) => break Ok(self.next_identifier_expression()?),
                Some(Token::Endl) => {
                    // Swallow new lines
                    self.source_iterator.next();
                    continue;
                }
                Some(token) => break Err(ParsingError::UnexpectedToken(token.clone())),
                None => break Err(ParsingError::UnexpectedEof),
            }
        };
    }

    fn next_numerical_expression(&mut self) -> Result<Expr<'a>, ParsingError<'a>> {
        if let Some(Token::Number(number)) = self.source_iterator.next() {
            match self.source_iterator.peek() {
                Some(Token::Plus) => Ok(self.next_binary_expression(Expr::Number(number))?),
                Some(Token::Endl) | None => Ok(Expr::Number(number)),
                Some(token) => Err(ParsingError::UnexpectedToken(token.clone())),
            }
        } else {
            panic!("Error parsing identifier");
        }
    }

    fn next_identifier_expression(&mut self) -> Result<Expr<'a>, ParsingError<'a>> {
        if let Some(Token::Identifier(ident)) = self.source_iterator.next() {
            match self.source_iterator.peek() {
                Some(Token::GroupL) => Ok(self.next_function_call_expression(ident)?),
                Some(Token::Plus) => Ok(self.next_binary_expression(Expr::Variable(ident))?),
                Some(Token::Endl) | None => Ok(Expr::Variable(ident)),
                Some(token) => Err(ParsingError::UnexpectedToken(token.clone())),
            }
        } else {
            Err(ParsingError::InternalParsingError)
        }
    }

    fn next_function_call_expression(
        &mut self,
        callee: &'a str,
    ) -> Result<Expr<'a>, ParsingError<'a>> {
        let mut arguments: Vec<Expr<'a>> = Vec::new();
        self.source_iterator.next();

        loop {
            let peeked = self.source_iterator.peek();
            if peeked == Some(&Token::GroupR) || peeked == None {
                return Ok(Expr::FunctionCall { callee, arguments });
            }
            let mut argument_part: Vec<Token<'a>> = Vec::new();
            loop {
                match self.source_iterator.next() {
                    Some(Token::GroupR) | Some(Token::Separator) => break,
                    None => return Err(ParsingError::UnmatchedParenthesis),
                    Some(token) => {
                        if token != Token::Endl {
                            argument_part.push(token);
                        }
                    }
                }
            }

            arguments.push(argument_part.into_iter().parse_ast().next_expression()?);
        }
    }

    fn next_binary_expression(&mut self, lhs: Expr<'a>) -> Result<Expr<'a>, ParsingError<'a>> {
        let operator = self.source_iterator.next();

        if let Some(Token::Plus) = operator {
            let rhs = match self.source_iterator.peek() {
                Some(Token::Identifier(_)) => self.next_identifier_expression(),
                Some(Token::Number(_)) => self.next_numerical_expression(),
                None => return Err(ParsingError::MissingRhsExpression),
                Some(token) => return Err(ParsingError::UnexpectedToken(token.clone())),
            };

            return Ok(Expr::Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs?),
                operator: Operator::Plus,
            });
        } else if let Some(token) = operator {
            return Err(ParsingError::UnexpectedToken(token));
        } else {
            return Err(ParsingError::InternalParsingError);
        }
    }

    fn next_extern_definition(&mut self) -> Result<Expr<'a>, ParsingError<'a>> {
        self.source_iterator.next();
        Ok(Expr::ExternalDefinition(self.next_prototype_definition()?))
    }

    fn next_function_definition(&mut self) -> Result<Expr<'a>, ParsingError<'a>> {
        self.source_iterator.next();
        let prototype = self.next_prototype_definition()?;
        let expression = self.next_expression()?;

        Ok(Expr::FunctionDefinition(prototype, Box::new(expression)))
    }

    fn next_prototype_definition(&mut self) -> Result<FunctionPrototype<'a>, ParsingError<'a>> {
        let token = self.source_iterator.next();
        let parenthesis = self.source_iterator.next();
        if let Some(Token::Identifier(ident)) = token {
            if let Some(Token::GroupL) = parenthesis {
                let mut arguments: Vec<&'a str> = Vec::new();
                loop {
                    let current = self.source_iterator.peek();
                    match current {
                        Some(Token::GroupR) => {
                            self.source_iterator.next();
                            break;
                        }
                        Some(Token::Identifier(ident)) => {
                            arguments.push(ident);
                            self.source_iterator.next();
                            let next_token = self.source_iterator.next();
                            match next_token {
                                Some(Token::Separator) => continue,
                                Some(Token::GroupR) => break,
                                _ => return Err(ParsingError::MissingArgumentSeparator),
                            }
                        }
                        Some(token) => {
                            return Err(ParsingError::UnexpectedToken(token.clone()));
                        }
                        None => {
                            return Err(ParsingError::UnmatchedParenthesis);
                        }
                    }
                }

                Ok(FunctionPrototype {
                    name: ident,
                    argument_names: arguments,
                })
            } else if let Some(tok) = parenthesis {
                return Err(ParsingError::UnexpectedToken(tok));
            } else {
                return Err(ParsingError::InternalParsingError);
            }
        } else if let Some(tok) = token {
            return Err(ParsingError::UnexpectedToken(tok));
        } else {
            return Err(ParsingError::InternalParsingError);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Expr, FunctionPrototype, IntoParsingIterator, Operator, ParsingError, Token};
    use std::boxed::Box;

    #[test]
    fn parse_number_expr() {
        let input = vec![Token::Number(1.0)];
        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(Ok(vec![Expr::Number(1.0)]), ast);
    }

    #[test]
    fn parse_binary_mixed_variable_and_number() {
        let input = vec![Token::Identifier("x"), Token::Plus, Token::Number(0.1)];
        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::Binary {
                lhs: Box::new(Expr::Variable("x")),
                rhs: Box::new(Expr::Number(0.1)),
                operator: Operator::Plus
            }]),
            ast
        );
    }

    #[test]
    fn parse_binary_variable() {
        let input = vec![Token::Identifier("x"), Token::Plus, Token::Identifier("y")];
        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::Binary {
                lhs: Box::new(Expr::Variable("x")),
                rhs: Box::new(Expr::Variable("y")),
                operator: Operator::Plus
            }]),
            ast
        );
    }

    #[test]
    fn nested_binary() {
        let input = vec![
            Token::Identifier("x"),
            Token::Plus,
            Token::Identifier("y"),
            Token::Plus,
            Token::Identifier("z"),
        ];
        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::Binary {
                lhs: Box::new(Expr::Variable("x")),
                rhs: Box::new(Expr::Binary {
                    lhs: Box::new(Expr::Variable("y")),
                    rhs: Box::new(Expr::Variable("z")),
                    operator: Operator::Plus
                }),
                operator: Operator::Plus
            }]),
            ast
        );
    }

    #[test]
    fn function_call() {
        let input = vec![
            Token::Identifier("print"),
            Token::GroupL,
            Token::Identifier("x"),
            Token::GroupR,
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::FunctionCall {
                callee: "print",
                arguments: vec![Expr::Variable("x")]
            }]),
            ast
        )
    }

    #[test]
    fn function_call_with_expressions_in_arguments() {
        let input = vec![
            Token::Identifier("print"),
            Token::GroupL,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::GroupR,
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::FunctionCall {
                callee: "print",
                arguments: vec![Expr::Binary {
                    lhs: Box::new(Expr::Number(1.0)),
                    rhs: Box::new(Expr::Number(2.0)),
                    operator: Operator::Plus
                }]
            }]),
            ast
        )
    }

    #[test]
    fn parse_multiple_top_levels() {
        let input = vec![Token::Number(1.0), Token::Endl, Token::Number(1.0)];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(Ok(vec![Expr::Number(1.0), Expr::Number(1.0),]), ast)
    }

    #[test]
    fn parse_mutliple_parameters() {
        let input = vec![
            Token::Identifier("print"),
            Token::GroupL,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Separator,
            Token::Identifier("x"),
            Token::GroupR,
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::FunctionCall {
                callee: "print",
                arguments: vec![
                    Expr::Binary {
                        lhs: Box::new(Expr::Number(1.0)),
                        rhs: Box::new(Expr::Number(2.0)),
                        operator: Operator::Plus
                    },
                    Expr::Variable("x")
                ]
            }]),
            ast
        )
    }

    #[test]
    fn parse_function_call_with_newline_parameters() {
        let input = vec![
            Token::Identifier("print"),
            Token::GroupL,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Separator,
            Token::Endl,
            Token::Identifier("x"),
            Token::GroupR,
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::FunctionCall {
                callee: "print",
                arguments: vec![
                    Expr::Binary {
                        lhs: Box::new(Expr::Number(1.0)),
                        rhs: Box::new(Expr::Number(2.0)),
                        operator: Operator::Plus
                    },
                    Expr::Variable("x")
                ]
            }]),
            ast
        )
    }

    #[test]
    fn parse_external_definition() {
        let input = vec![
            Token::Extern,
            Token::Identifier("exit"),
            Token::GroupL,
            Token::GroupR,
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::ExternalDefinition(FunctionPrototype {
                name: "exit",
                argument_names: vec![]
            })]),
            ast
        )
    }

    #[test]
    fn parse_external_definition_with_arguments() {
        let input = vec![
            Token::Extern,
            Token::Identifier("add"),
            Token::GroupL,
            Token::Identifier("x"),
            Token::Separator,
            Token::Identifier("y"),
            Token::GroupR,
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::ExternalDefinition(FunctionPrototype {
                name: "add",
                argument_names: vec!["x", "y"]
            })]),
            ast
        )
    }

    #[test]
    fn parse_external_definition_with_arguments_requires_argument_separators() {
        let input = vec![
            Token::Extern,
            Token::Identifier("add"),
            Token::GroupL,
            Token::Identifier("x"),
            Token::Identifier("y"),
            Token::GroupR,
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(Err(ParsingError::MissingArgumentSeparator), ast)
    }

    #[test]
    fn parse_function_definition() {
        let input = vec![
            Token::Def,
            Token::Identifier("foo"),
            Token::GroupL,
            Token::GroupR,
            Token::Number(1.0),
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::FunctionDefinition(
                FunctionPrototype {
                    name: "foo",
                    argument_names: vec![]
                },
                Box::new(Expr::Number(1.0))
            )]),
            ast
        )
    }

    #[test]
    fn parse_function_definition_with_arguments() {
        let input = vec![
            Token::Def,
            Token::Identifier("add"),
            Token::GroupL,
            Token::Identifier("x"),
            Token::Separator,
            Token::Identifier("y"),
            Token::GroupR,
            Token::Identifier("x"),
            Token::Plus,
            Token::Identifier("y"),
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(
            Ok(vec![Expr::FunctionDefinition(
                FunctionPrototype {
                    name: "add",
                    argument_names: vec!["x", "y"]
                },
                Box::new(Expr::Binary {
                    lhs: Box::new(Expr::Variable("x")),
                    rhs: Box::new(Expr::Variable("y")),
                    operator: Operator::Plus
                })
            )]),
            ast
        )
    }

    #[test]
    fn parse_function_definition_with_arguments_requires_argument_separators() {
        let input = vec![
            Token::Def,
            Token::Identifier("add"),
            Token::GroupL,
            Token::Identifier("x"),
            Token::Identifier("y"),
            Token::GroupR,
            Token::Number(1.0),
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(Err(ParsingError::MissingArgumentSeparator), ast)
    }
    #[test]
    fn parse_function_definition_requires_body() {
        let input = vec![
            Token::Def,
            Token::Identifier("add"),
            Token::GroupL,
            Token::Identifier("x"),
            Token::Separator,
            Token::Identifier("y"),
            Token::GroupR,
        ];

        let ast = input
            .parse_ast()
            .collect::<Result<Vec<Expr>, ParsingError>>();

        assert_eq!(Err(ParsingError::UnexpectedEof), ast)
    }
}
