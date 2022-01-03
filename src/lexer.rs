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

fn eat_comment(s: &[u8], i: usize) -> usize {
    let at_line_comment = match_character(s, i, b'/') && match_character(s, i + 1, b'/');
    if !at_line_comment {
        return i;
    }
    eat_line(s, i + 2)
}

fn eat_whitespace_and_comments(s: &[u8], mut i: usize) -> usize {
    loop {
        let iteration_start = i;
        i = eat_whitespace(s, i);
        i = eat_comment(s, i);
        if i == iteration_start {
            break i;
        }
    }
}

fn eat_hashbang(s: &[u8], i: usize) -> usize {
    let at_hashbang = match_character(s, i, b'#') && match_character(s, i + 1, b'!');
    if !at_hashbang {
        return i;
    }
    eat_line(s, i + 2)
}

fn is_alphabetic(c: u8) -> bool {
    (b'a'..=b'z').contains(&c) || (b'A'..=b'Z').contains(&c) || b'_' == c || b'-' == c
}

fn is_numeric(c: u8) -> bool {
    (b'0'..=b'9').contains(&c)
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

fn eat_line(s: &[u8], mut i: usize) -> usize {
    while i < s.len() && s[i] != b'\n' {
        i += 1;
    }
    if i < s.len() && s[i] == b'\n' {
        i += 1;
    }
    i
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

macro_rules! return_if_4characters {
    ($self:ident, $token:expr, $char1:expr, $char2:expr, $char3:expr, $char4:expr) => {{
        if match_character($self.buffer, $self.start, $char1)
            && match_character($self.buffer, $self.start + 1, $char2)
            && match_character($self.buffer, $self.start + 2, $char3)
            && match_character($self.buffer, $self.start + 3, $char4)
        {
            return (Some($token), $self.start + 4);
        }
    }};
}

macro_rules! return_if_5characters {
    ($self:ident, $token:expr, $char1:expr, $char2:expr, $char3:expr, $char4:expr, $char5:expr) => {{
        if match_character($self.buffer, $self.start, $char1)
            && match_character($self.buffer, $self.start + 1, $char2)
            && match_character($self.buffer, $self.start + 2, $char3)
            && match_character($self.buffer, $self.start + 3, $char4)
            && match_character($self.buffer, $self.start + 4, $char5)
        {
            return (Some($token), $self.start + 5);
        }
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(buffer: &'a str) -> Lexer<'a> {
        let bytes = buffer.as_bytes();
        Lexer {
            buffer: bytes,
            start: eat_hashbang(bytes, 0),
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

    fn tokenize_string(&self) -> (Option<Token<'a>>, usize) {
        let at_string = match_character(self.buffer, self.start, b'"');
        if at_string {
            let string_start = self.start + 1;
            let mut n = string_start;
            let mut escape_next = false;
            loop {
                if n >= self.buffer.len() {
                    break;
                }

                // Because this is a zero-coping tokenizer, we can't
                // modify the string. The escaping is thus kept.
                if !escape_next {
                    escape_next = match_character(self.buffer, n, b'\\');
                } else {
                    escape_next = false;
                    n += 1;
                }

                if match_character(self.buffer, n, b'"') {
                    break;
                } else {
                    n += 1;
                }
            }
            // TODO: Error handling missing here.
            let string_end = n;
            let str = Token::String(&self.buffer[string_start..string_end]);
            if match_character(self.buffer, n, b'"') {
                n += 1;
            }
            (Some(str), n)
        } else {
            (None, self.start)
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
        // TODO: Rewrite with pattern matching.
        return_if_5characters!(self, Token::False, b'f', b'a', b'l', b's', b'e');
        return_if_5characters!(self, Token::Break, b'b', b'r', b'e', b'a', b'k');
        return_if_4characters!(self, Token::Loop, b'l', b'o', b'o', b'p');
        return_if_4characters!(self, Token::True, b't', b'r', b'u', b'e');
        return_if_4characters!(self, Token::Else, b'e', b'l', b's', b'e');
        return_if_3characters!(self, Token::Let, b'l', b'e', b't');
        return_if_2characters!(self, Token::If, b'i', b'f');
        return_if_2characters!(self, Token::Function, b'f', b'n');
        return_if_2characters!(self, Token::GreaterEqual, b'>', b'=');
        return_if_2characters!(self, Token::LessEqual, b'<', b'=');
        return_if_2characters!(self, Token::RightArrow, b'-', b'>');
        return_if_2characters!(self, Token::EqualEqual, b'=', b'=');
        return_if_2characters!(self, Token::StarStar, b'*', b'*');
        return_if_character!(self, Token::BraceLeft, b'{');
        return_if_character!(self, Token::BraceRight, b'}');
        return_if_character!(self, Token::ParenLeft, b'(');
        return_if_character!(self, Token::ParenRight, b')');
        return_if_character!(self, Token::BracketLeft, b'[');
        return_if_character!(self, Token::BracketRight, b']');
        return_if_character!(self, Token::Dot, b'.');
        return_if_character!(self, Token::Colon, b':');
        return_if_character!(self, Token::SemiColon, b';');
        return_if_character!(self, Token::Comma, b',');
        return_if_character!(self, Token::Equal, b'=');
        return_if_character!(self, Token::Less, b'<');
        return_if_character!(self, Token::Greater, b'>');
        return_if_character!(self, Token::Plus, b'+');
        return_if_character!(self, Token::Minus, b'-');
        return_if_character!(self, Token::Star, b'*');
        return_if_character!(self, Token::Slash, b'/');
        return_if_character!(self, Token::Minus, b'-');
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

        self.start = eat_whitespace_and_comments(self.buffer, self.start);

        return_if_match!(self, self.tokenize_number());
        return_if_match!(self, self.tokenize_string());
        return_if_match!(self, self.tokenize_structure());
        return_if_match!(self, self.tokenize_symbol());

        None
    }
}

#[test]
fn tokenize_with_comments() {
    let tokens: Vec<Token> = Lexer::new(
        "

// This is a line comment
    
// And one more with some trailing whitespace before it. After comes code:


if 2>1 1 // end of line should be ignored too
else 0

// more comments to be ignored",
    )
    .collect();
    assert_eq!(
        tokens,
        vec![
            Token::If,
            Token::Integer(2),
            Token::Greater,
            Token::Integer(1),
            Token::Integer(1),
            Token::Else,
            Token::Integer(0)
        ]
    );
}

#[test]
fn tokenize_if() {
    let tokens: Vec<Token> = Lexer::new("if 2>1 1 else 0").collect();
    assert_eq!(
        tokens,
        vec![
            Token::If,
            Token::Integer(2),
            Token::Greater,
            Token::Integer(1),
            Token::Integer(1),
            Token::Else,
            Token::Integer(0)
        ]
    );
}

#[test]
fn tokenize_string() {
    let tokens: Vec<Token> = Lexer::new("\"this is a string\"").collect();
    assert_eq!(tokens, vec![Token::String("this is a string".as_bytes()),]);
}

#[test]
fn tokenize_string_with_escaping() {
    let tokens: Vec<Token> = Lexer::new("\"this is a \\\"string\\\"\"").collect();
    assert_eq!(
        tokens,
        vec![Token::String("this is a \\\"string\\\"".as_bytes()),]
    );
}

#[test]
fn tokenize_if_with_bool_literals() {
    let tokens: Vec<Token> = Lexer::new("if false 1 else 0").collect();
    assert_eq!(
        tokens,
        vec![
            Token::If,
            Token::False,
            Token::Integer(1),
            Token::Else,
            Token::Integer(0)
        ]
    );
}
