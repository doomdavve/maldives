use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(buffer: &'a str) -> Lexer<'a> {
        Lexer {
            iter: buffer.chars().peekable(),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cr) = self.iter.peek() {
            let c = *cr;
            if c >= '0' && c <= '9' {
                self.iter.next();
                Some(Token::integer((c as i32) - '0' as i32))
            } else {
                None // FIXME: error
            }
        } else {
            None
        }
    }
}

enum TokenType {
    Integer,
}

struct Token {
    token_type: TokenType,
    integer_value: i32,
}

impl Token {
    fn integer(integer: i32) -> Token {
        Token {
            token_type: TokenType::Integer,
            integer_value: integer,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.token_type {
            TokenType::Integer => write!(f, "{}", self.integer_value),
        }
    }
}

fn main() {
    let test: &str = "22";
    let mut lexer = Lexer::new(test);
    while let Some(token) = lexer.next() {
        println!("TOKEN: {}", token);
    }
}
