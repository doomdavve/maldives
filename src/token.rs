#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Integer(i32),
    Symbol(&'a [u8]),
    Let,
    Equal,
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
    SemiColon,
    Comma
}
