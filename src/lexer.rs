use crate::token::Token;

use std::str::from_utf8_unchecked;

pub struct Lexer<'a> {
    buffer: &'a [u8],
    start: usize,
}

fn eat_digits(s: &[u8], mut i: usize) -> usize {
    while i < s.len() && b'0' <= s[i] && s[i] <= b'9' {
        i += 1;
    }
    i
}

fn eat_whitespace(s: &[u8], mut i: usize) -> usize {
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

fn eat_symbol(s: &[u8], mut i: usize) -> (&[u8], usize) {
    let start = i;
    if i < s.len() && is_alphabetic(s[i]) {
        i += 1;
    }
    while i < s.len() && (is_alphabetic(s[i]) || is_numeric(s[i])) {
        i += 1;
    }
    (&s[start..i], i)
}

fn eat_character(s: &[u8], mut i: usize, c: u8) -> usize {
    if i < s.len() && c == s[i] {
        i += 1;
    }
    i
}

macro_rules! return_if_characters {
    ($self:ident, $token:expr, $($char:expr),+) => {{
        let mut i = $self.start;
        $(
            if eat_character($self.buffer, i, $char) > i {
                i+=1;
            }
        )+
        if i > $self.start {
            return (Some($token), i);
        }
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(buffer: &'a str) -> Lexer<'a> {
        Lexer {
            buffer: buffer.as_bytes(),
            start: 0,
        }
    }

    #[cfg(test)]
    pub fn rewind(&mut self) {
        self.start = 0;
    }

    pub fn peek(&mut self) -> Option<Token<'a>> {
        let start = self.start;
        let res = self.next();
        self.start = start;
        res
    }

    fn tokenize_number(&self) -> (Option<Token<'a>>, usize) {
        let n = eat_digits(self.buffer, self.start);
        if n > 0 {
            let token = unsafe {
                from_utf8_unchecked(&self.buffer[self.start..n])
                    .parse::<i32>()
                    .map(Token::Integer)
                    .ok()
            };
            (token, n)
        } else {
            (None, n)
        }
    }

    fn tokenize_symbol(&self) -> (Option<Token<'a>>, usize) {
        let (symbol, n) = eat_symbol(self.buffer, self.start);
        if !symbol.is_empty() {
            (Some(Token::Symbol(symbol)), n)
        } else {
            (None, n)
        }
    }

    fn tokenize_structure(&self) -> (Option<Token<'a>>, usize) {
        return_if_characters!(self, Token::BraceLeft, b'{');
        return_if_characters!(self, Token::BraceRight, b'}');
        return_if_characters!(self, Token::ParenLeft, b'(');
        return_if_characters!(self, Token::ParenRight, b')');
        return_if_characters!(self, Token::BracketLeft, b'[');
        return_if_characters!(self, Token::BracketRight, b']');
        return_if_characters!(self, Token::SemiColon, b';');
        return_if_characters!(self, Token::Comma, b',');
        return_if_characters!(self, Token::Equal, b'=');
        return_if_characters!(self, Token::Function, b'f', b'n');
        return_if_characters!(self, Token::Let, b'l', b'e', b't');
        (None, self.start)
    }
}

macro_rules! return_if_match {
    ($self:ident, $func:expr) => {{
        let (token, end) = $func;
        if token.is_some() {
            $self.start = end;
            return token;
        }
    }};
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.buffer.len() {
            return None;
        }

        self.start = eat_whitespace(self.buffer, self.start);

        return_if_match!(self, self.tokenize_number());
        return_if_match!(self, self.tokenize_structure());
        return_if_match!(self, self.tokenize_symbol());

        None
    }
}
