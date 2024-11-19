use std::fmt::Display;

use crate::{lexer::{Token, TokenType}, Lexer};

#[derive(Debug, PartialEq)]
pub enum InsertValue {
    StringLiteral(String),
    NumberLiteral(i64),
}

#[derive(Debug)]
pub struct InsertNode {
    pub table: String,
    pub columns: Vec<String>,
    pub values: Vec<InsertValue>,
}

#[derive(Debug)]
pub struct Parser<'a> {
    current_token: Option<Token>,
    lexer: Lexer<'a>,
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    NoToken,
    TokenParseError,
    UnexpectedToken {
        expected: TokenType,
        actual: TokenType,
    },
    UnexpectedTokenValue {
        expected: String,
        actual: String,
    },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NoToken => write!(f, "Expected token, found none"),
            ParseError::TokenParseError => write!(f, "Could not parse token value"),
            ParseError::UnexpectedToken { expected, actual } => {
                write!(f, "Expected token {:?}, found {:?}", expected, actual)
            }
            ParseError::UnexpectedTokenValue { expected, actual } => {
                write!(f, "Expected token {:?}, found {:?}", expected, actual)
            }
        }
    }
}

impl <'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            current_token: None,
            lexer,
        }
    }

    pub fn parse(&mut self) -> Result<InsertNode, ParseError> {
        self.advance();

        match &self.current_token {
            Some(token) => match token.value.as_str() {
                "INSERT" => self.parse_insert(),
                "SELECT" => Err(ParseError::TokenParseError),
                x => Err(ParseError::UnexpectedTokenValue { expected: "INSERT | SELECT".into(), actual: x.into() }),
            },
            _ => Err(ParseError::TokenParseError),
        }
    }

    fn parse_insert(&mut self) -> Result<InsertNode, ParseError> {
        self.expect("INSERT")?;
        self.expect("INTO")?;

        let table_name = self.expect_type(TokenType::Literal)?;

        self.expect_type(TokenType::LParen)?;

        let mut columns = Vec::new();

        loop {
            let column = self.expect_type(TokenType::Literal)?;
            columns.push(column);

            match &self.current_token {
                Some(token) if token.token_type != TokenType::Comma => break,
                Some(_) => {},
                None => {},
            }

            self.advance();
        }

        self.expect_type(TokenType::RParen)?;
        self.expect("VALUES")?;
        self.expect_type(TokenType::LParen)?;

        let mut values = Vec::new();

        loop {
            let value = match &self.current_token {
                Some(token) => match token.token_type {
                    TokenType::StringLiteral => InsertValue::StringLiteral(token.value.clone()),
                    TokenType::NumberLiteral => match token.value.parse() {
                        Err(_) => return Err(ParseError::TokenParseError),
                        Ok(number) => InsertValue::NumberLiteral(number)
                    }
                    _ => return Err(ParseError::UnexpectedToken {
                        expected: TokenType::Unknown,
                        actual: token.token_type.clone(),
                    }),
                }
                None => return Err(ParseError::NoToken),
            };

            self.advance();

            values.push(value);

            match &self.current_token {
                Some(token) if token.token_type != TokenType::Comma => break,
                Some(_) => {},
                None => {},
            }

            self.advance();
        }

        self.expect_type(TokenType::RParen)?;

        Ok(InsertNode {
            table: table_name,
            columns,
            values,
        })
    }

    fn advance(&mut self) {
        while let Some(token) = self.lexer.next() {
            self.current_token = Some(token.clone());
            if !matches!(token.token_type, TokenType::Space) {
                return;
            }
        }
    }

    fn expect(&mut self, expected: &str) -> Result<(), ParseError> {
        let token = self.current_token
            .take()
            .ok_or(ParseError::NoToken)?;

        if token.value == expected {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedTokenValue {
                expected: expected.to_string(),
                actual: token.value.clone(),
            })
        }
    }

    fn expect_type(&mut self, expected_type: TokenType) -> Result<String, ParseError> {
        let token = self.current_token
            .take()
            .ok_or(ParseError::NoToken)?;

        if token.token_type == expected_type {
            self.advance();
            Ok(token.value)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected_type,
                actual: token.token_type,
            })
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_ast_should_return_insert_node() {
        // given
        let query = "INSERT INTO mytable (c1, c2) VALUES (1, 2)";
        let lexer = Lexer::new(query);

        // when
        let mut parser = Parser::new(lexer);
        let node = parser.parse().unwrap();

        // then
        assert_eq!("mytable", node.table);
        assert_eq!(vec!["c1", "c2"], node.columns);
        assert_eq!(vec![InsertValue::NumberLiteral(1), InsertValue::NumberLiteral(2)], node.values);
    }

    #[test]
    fn test_ast_should_return_token_parse_error() {
        // given
        let query = "INSERT INTO mytable (c1, c2) VALUES (1a, 2)";
        let lexer = Lexer::new(query);

        // when
        let mut parser = Parser::new(lexer);
        let node = parser.parse();

        // then
        assert_eq!(ParseError::TokenParseError, node.err().unwrap());
    }

    #[test]
    fn test_ast_should_return_unexpected_token_error_when_column_name_is_not_literal() {
        // given
        let query = "INSERT INTO mytable (12334, c2) VALUES (1, 2)";
        let lexer = Lexer::new(query);

        // when
        let mut parser = Parser::new(lexer);
        let node = parser.parse();

        // then
        assert_eq!(ParseError::UnexpectedToken {
            expected: TokenType::Literal,
            actual: TokenType::NumberLiteral,
        }, node.err().unwrap());
    }

    #[test]
    fn test_ast_should_return_unexpected_token_error_when_table_name_is_not_literal() {
        // given
        let query = "INSERT INTO 1234 (c1, c2) VALUES (1, 2)";
        let lexer = Lexer::new(query);

        // when
        let mut parser = Parser::new(lexer);
        let node = parser.parse();

        // then
        assert_eq!(ParseError::UnexpectedToken {
            expected: TokenType::Literal,
            actual: TokenType::NumberLiteral,
        }, node.err().unwrap());
    }

    #[test]
    fn test_ast_should_return_error_when_syntax_is_wrong() {
        // given
        let query = "IN INTO mytable (c1, c2) VALUES (1, 2)";
        let lexer = Lexer::new(query);

        // when
        let mut parser = Parser::new(lexer);
        let node = parser.parse();

        // then
        assert_eq!(ParseError::UnexpectedTokenValue {
            expected: "INSERT | SELECT".into(),
            actual: "IN".into(),
        }, node.err().unwrap());
    }
}
