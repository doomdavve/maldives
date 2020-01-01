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

fn match_character(s: &[u8], i: usize, c: u8) -> bool {
    i < s.len() && c == s[i]
}

macro_rules! return_if_character {
    ($self:ident, $token:expr, $char:expr) => {{
        if match_character($self.buffer, $self.start, $char) {
            return (Some($token), $self.start + 1);
        }
    }};
}

macro_rules! return_if_2characters {
    ($self:ident, $token:expr, $char1:expr, $char2:expr) => {{
        if match_character($self.buffer, $self.start, $char1)
            && match_character($self.buffer, $self.start + 1, $char2)
        {
            return (Some($token), $self.start + 2);
        }
    }};
}

macro_rules! return_if_3characters {
    ($self:ident, $token:expr, $char1:expr, $char2:expr, $char3:expr) => {{
        if match_character($self.buffer, $self.start, $char1)
            && match_character($self.buffer, $self.start + 1, $char2)
            && match_character($self.buffer, $self.start + 2, $char3)
        {
            return (Some($token), $self.start + 3);
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
        return_if_character!(self, Token::BraceLeft, b'{');
        return_if_character!(self, Token::BraceRight, b'}');
        return_if_character!(self, Token::ParenLeft, b'(');
        return_if_character!(self, Token::ParenRight, b')');
        return_if_character!(self, Token::BracketLeft, b'[');
        return_if_character!(self, Token::BracketRight, b']');
        return_if_character!(self, Token::SemiColon, b';');
        return_if_character!(self, Token::Comma, b',');
        return_if_character!(self, Token::Equal, b'=');
        return_if_2characters!(self, Token::Function, b'f', b'n');
        return_if_3characters!(self, Token::Let, b'l', b'e', b't');
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
