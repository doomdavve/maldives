use std::str::from_utf8_unchecked;

struct Lexer<'a> {
    cur: &'a [u8]
}

fn eat_digits<'a>(s: &'a [u8]) -> (&[u8], &'a [u8]) {
    let mut i = 0;
    while i < s.len() && b'0' <= s[i] && s[i] <= b'9' {
        i += 1;
    }
    (&s[..i], &s[i..])
}

impl<'a> Lexer<'a> {
    fn new(buffer: &'a str) -> Lexer<'a> {
        Lexer {
            cur: buffer.as_bytes()
        }
    }

    fn tokenize_number(&self) -> (Option<Token>, &'a [u8]) {
        let (number, trailing) = eat_digits(self.cur);
        if !number.is_empty() {
            unsafe {
                let token = from_utf8_unchecked(number).parse::<i32>().map(|j| Token::integer(j)).ok();
                (token, trailing)
            }
        } else {
            (None, trailing)
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        // Attempt to parse as number first.
        let (token, _rest) = self.tokenize_number();
        self.cur = _rest;
        token
    }
}

#[derive(Debug)]
enum TokenType {
    Integer,
}

#[derive(Debug)]
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

fn main() {
    let test: &str = "223 123";
    let mut lexer = Lexer::new(test);
    while let Some(token) = lexer.next() {
        println!("{:?}", token);
    }
}
