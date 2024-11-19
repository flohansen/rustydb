use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Literal,
    NumberLiteral,
    StringLiteral,
    Space,
    LParen,
    RParen,
    Comma,
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        if let Some(c) = self.chars.next() {
            let (value , token_type) = match c {
                'a'..='z' | 'A'..='Z' => (self.parse_literal(c), TokenType::Literal),
                '0'..='9' => (self.parse_number_literal(c), TokenType::NumberLiteral),
                '\'' => (self.parse_string_literal(), TokenType::StringLiteral),
                ' ' => (String::from(c), TokenType::Space),
                '(' => (String::from(c), TokenType::LParen),
                ')' => (String::from(c), TokenType::RParen),
                ',' => (String::from(c), TokenType::Comma),
                _ => (String::from(c), TokenType::Unknown),
            };

            return Some(Token { value, token_type })
        }

        None
    }

    fn parse_literal(&mut self, c: char) -> String {
        let mut literal = String::from(c);

        loop {
            let c = match self.chars.peek() {
                Some(peeked) => peeked,
                _ => break,
            };

            if !c.is_alphanumeric() {
                break;
            }

            literal.push(self.chars.next().unwrap());
        }

        literal
    }

    fn parse_number_literal(&mut self, c: char) -> String {
        let mut literal = String::from(c);

        loop {
            let c = match self.chars.peek() {
                Some(peeked) => peeked,
                _ => break,
            };

            if !c.is_alphanumeric() {
                break;
            }

            literal.push(self.chars.next().unwrap());
        }

        literal
    }

    fn parse_string_literal(&mut self) -> String {
        let mut literal = String::new();

        loop {
            let c = self.chars
                .next()
                .unwrap();

            if c == '\'' {
                break;
            }

            literal.push(c);
        }

        literal
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_should_return_literal() {
        // given
        let query = "SELECT";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(Some(Token { value: String::from("SELECT"), token_type: TokenType::Literal }), lexer.next());
    }
    
    #[test]
    fn test_lexer_should_return_number_literal() {
        // given
        let query = "123456abc";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(Some(Token { value: String::from("123456abc"), token_type: TokenType::NumberLiteral }), lexer.next());
    }

    #[test]
    fn test_lexer_should_return_string_literal() {
        // given
        let query = "'Hello'";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(Some(Token { value: String::from("Hello"), token_type: TokenType::StringLiteral }), lexer.next());
    }

    #[test]
    fn test_lexer_should_return_comma() {
        // given
        let query = ",";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(Some(Token { value: String::from(","), token_type: TokenType::Comma }), lexer.next());
    }

    #[test]
    fn test_lexer_should_return_space() {
        // given
        let query = " ";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(Some(Token { value: String::from(" "), token_type: TokenType::Space }), lexer.next());
    }

    #[test]
    fn test_lexer_should_return_left_parantheses() {
        // given
        let query = "(";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(Some(Token { value: String::from("("), token_type: TokenType::LParen }), lexer.next());
    }

    #[test]
    fn test_lexer_should_return_right_parantheses() {
        // given
        let query = ")";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(Some(Token { value: String::from(")"), token_type: TokenType::RParen }), lexer.next());
    }

    #[test]
    fn test_lexer_should_return_unknown() {
        // given
        let query = "!";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(Some(Token { value: String::from("!"), token_type: TokenType::Unknown }), lexer.next());
    }

    #[test]
    fn test_lexer_should_return_none() {
        // given
        let query = "";

        // when
        let mut lexer = Lexer::new(query);

        // then
        assert_eq!(None, lexer.next());
    }
}
