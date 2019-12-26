use crate::token::{Keyword, Token};

use std::str::from_utf8_unchecked;

pub struct Lexer<'a> {
    cur: &'a [u8],
}

fn eat_digits<'a>(s: &'a [u8]) -> (&[u8], &'a [u8]) {
    let mut i = 0;
    while i < s.len() && b'0' <= s[i] && s[i] <= b'9' {
        i += 1;
    }
    (&s[..i], &s[i..])
}

fn eat_whitespace<'a>(s: &'a [u8]) -> (&[u8], &'a [u8]) {
    let mut i = 0;
    while i < s.len() && (b' ' == s[i] || b'\t' == s[i] || b'\n' == s[i] || b'\r' == s[i]) {
        i += 1;
    }
    (&s[..i], &s[i..])
}

fn is_alphabetic(c: u8) -> bool {
    (b'a' <= c && c <= b'z') || (b'A' <= c && c <= b'Z') || b'_' == c || b'-' == c
}

fn is_numeric(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn eat_symbol<'a>(s: &'a [u8]) -> (&[u8], &'a [u8]) {
    let mut i = 0;
    if i < s.len() && is_alphabetic(s[i]) {
        i += 1;
    }
    while i < s.len() && (is_alphabetic(s[i]) || is_numeric(s[i])) {
        i += 1;
    }
    (&s[..i], &s[i..])
}

impl<'a> Lexer<'a> {
    pub fn new(buffer: &'a str) -> Lexer<'a> {
        Lexer {
            cur: buffer.as_bytes(),
        }
    }

    fn tokenize_number(&self) -> (Option<Token>, &'a [u8]) {
        let (number, trailing) = eat_digits(self.cur);
        if !number.is_empty() {
            unsafe {
                let token = from_utf8_unchecked(number)
                    .parse::<i32>()
                    .map(|j| Token::integer(j))
                    .ok();
                (token, trailing)
            }
        } else {
            (None, trailing)
        }
    }

    fn tokenize_whitespace(&self) -> (Option<Token>, &'a [u8]) {
        let (whitespace, trailing) = eat_whitespace(self.cur);
        if !whitespace.is_empty() {
            (Some(Token::whitespace()), trailing)
        } else {
            (None, trailing)
        }
    }

    fn tokenize_symbol(&self) -> (Option<Token>, &'a [u8]) {
        let (symbol, trailing) = eat_symbol(self.cur);
        if !symbol.is_empty() {
            unsafe {
                let symbol2 = from_utf8_unchecked(symbol);
                (Some(Token::symbol(symbol2)), trailing)
            }
        } else {
            (None, trailing)
        }
    }

    fn tokenize_keyword(&self) -> (Option<Token>, &'a [u8]) {
        if self.cur.len() >= 1 && self.cur[0] == b'{' {
            (Some(Token::keyword(Keyword::BraceLeft)), &self.cur[1..])
        } else if self.cur.len() >= 1 && self.cur[0] == b'}' {
            (Some(Token::keyword(Keyword::BraceRight)), &self.cur[1..])
        } else if self.cur.len() >= 1 && self.cur[0] == b'(' {
            (Some(Token::keyword(Keyword::ParanLeft)), &self.cur[1..])
        } else if self.cur.len() >= 1 && self.cur[0] == b')' {
            (Some(Token::keyword(Keyword::ParanRight)), &self.cur[1..])
        } else if self.cur.len() >= 1 && self.cur[0] == b',' {
            (Some(Token::keyword(Keyword::Comma)), &self.cur[1..])
        } else if self.cur.len() >= 2 && self.cur[0] == b'f' && self.cur[1] == b'n' {
            (Some(Token::keyword(Keyword::Function)), &self.cur[2..])
        } else {
            (None, &self.cur)
        }
    }
}

macro_rules! return_if_match {
    ($self:ident, $func:expr) => {{
        let (token, _rest) = $func;
        $self.cur = _rest;
        if token.is_some() {
            return token;
        }
    }};
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        return_if_match!(self, self.tokenize_number());
        return_if_match!(self, self.tokenize_whitespace());
        return_if_match!(self, self.tokenize_keyword());
        return_if_match!(self, self.tokenize_symbol());

        match self.cur.is_empty() {
            true => None,
            false => Some(Token::error()),
        }
    }
}
