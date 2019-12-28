use crate::token::{Keyword, Token};

use std::str::from_utf8_unchecked;

pub struct Lexer<'a> {
    buffer: &'a [u8],
    start: usize,
    end: usize,
}

fn eat_digits<'a>(s: &'a [u8], mut i: usize) -> usize {
    while i < s.len() && b'0' <= s[i] && s[i] <= b'9' {
        i += 1;
    }
    i
}

fn eat_whitespace<'a>(s: &'a [u8], mut i: usize) -> usize {
    while i < s.len() && (b' ' == s[i] || b'\t' == s[i] || b'\n' == s[i] || b'\r' == s[i]) {
        i += 1;
    }
    i
}

fn is_alphabetic(c: u8) -> bool {
    (b'a' <= c && c <= b'z') || (b'A' <= c && c <= b'Z') || b'_' == c || b'-' == c
}

fn is_numeric(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn eat_symbol<'a>(s: &'a [u8], mut i: usize) -> (&[u8], usize) {
    let start = i;
    if i < s.len() && is_alphabetic(s[i]) {
        i += 1;
    }
    while i < s.len() && (is_alphabetic(s[i]) || is_numeric(s[i])) {
        i += 1;
    }
    (&s[start..i], i)
}

fn eat_character<'a>(s: &'a [u8], mut i: usize, c: u8) -> usize {
    if i < s.len() && c == s[i] {
        i += 1;
    }
    i
}

macro_rules! return_if_character {
    ($self:ident, $char:expr, $token:expr) => {{
        if eat_character($self.buffer, $self.start, $char) > $self.start {
            return (Some($token), $self.start + 1);
        }
    }};
}

macro_rules! return_if_2characters {
    ($self:ident, $char1:expr, $char2:expr, $token:expr) => {{
        if eat_character($self.buffer, $self.start, $char1) > $self.start
            && eat_character($self.buffer, $self.start + 1, $char2) > $self.start + 1
        {
            return (Some($token), $self.start + 2);
        }
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(buffer: &'a str) -> Lexer<'a> {
        Lexer {
            buffer: buffer.as_bytes(),
            start: 0,
            end: 0,
        }
    }

//    fn peek(&mut self) -> Option<Token<'a>> {
//        let start = self.start;
//        let end = self.end;
//        let res = self.next();
//        self.start = start;
//        self.end = end;
//        res
//    }

    fn tokenize_number(&self) -> (Option<Token<'a>>, usize) {
        let n = eat_digits(self.buffer, self.start);
        if n > 0 {
            unsafe {
                let token = from_utf8_unchecked(&self.buffer[self.start..n])
                    .parse::<i32>()
                    .map(|j| Token::Integer(j))
                    .ok();
                return (token, n);
            }
        } else {
            return (None, n);
        };
    }

    fn tokenize_symbol(&self) -> (Option<Token<'a>>, usize) {
        let (symbol, n) = eat_symbol(self.buffer, self.start);
        if !symbol.is_empty() {
            (Some(Token::Symbol(symbol)), n)
        } else {
            (None, n)
        }
    }

    fn tokenize_keyword(&self) -> (Option<Token<'a>>, usize) {
        return_if_character!(self, b'{', Token::Keyword(Keyword::BraceLeft));
        return_if_character!(self, b'}', Token::Keyword(Keyword::BraceRight));
        return_if_character!(self, b'(', Token::Keyword(Keyword::ParenLeft));
        return_if_character!(self, b')', Token::Keyword(Keyword::ParenRight));
        return_if_character!(self, b';', Token::Keyword(Keyword::SemiColon));
        return_if_character!(self, b',', Token::Keyword(Keyword::Comma));
        return_if_2characters!(self, b'f', b'n', Token::Keyword(Keyword::Function));

        (None, self.start)
    }
}

macro_rules! return_if_match {
    ($self:ident, $func:expr) => {{
        let (token, end) = $func;
        if token.is_some() {
            $self.end = end;
            println!("token length: {}", end - $self.start);
            return token;
        }
    }};
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.buffer.len() {
            return None
        }

        self.end = eat_whitespace(self.buffer, self.end);
        self.start = self.end;

        println!("left: {:?}", unsafe {
            from_utf8_unchecked(&self.buffer[self.start..])
        });

        return_if_match!(self, self.tokenize_number());
        return_if_match!(self, self.tokenize_keyword());
        return_if_match!(self, self.tokenize_symbol());

        None
    }
}
