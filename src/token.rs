use std::fmt;
use std::str::from_utf8_unchecked;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Integer(i32),
    Symbol(&'a [u8]),
    String(&'a [u8]),
    True,
    False,
    If,
    Else,
    Let,
    Equal,
    EqualEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Function,
    BraceLeft,
    BraceRight,
    ParenLeft,
    ParenRight,
    BracketLeft,
    BracketRight,
    Colon,
    SemiColon,
    Comma,
    RightArrow,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Token::Integer(i) => write!(f, "{}", i),
            Token::Symbol(s) => write!(f, "{}", unsafe { from_utf8_unchecked(s) }),
            Token::String(s) => write!(f, "{}", unsafe { from_utf8_unchecked(s) }),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Let => write!(f, "let"),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Greater => write!(f, ">"),
            Token::Less => write!(f, "<"),
            Token::GreaterEqual => write!(f, ">="),
            Token::LessEqual => write!(f, "<="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Function => write!(f, "fn"),
            Token::BraceLeft => write!(f, "{{"),
            Token::BraceRight => write!(f, "}}"),
            Token::ParenLeft => write!(f, "("),
            Token::ParenRight => write!(f, ")"),
            Token::BracketLeft => write!(f, "["),
            Token::BracketRight => write!(f, "]"),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::RightArrow => write!(f, "->"),
        }
    }
}
