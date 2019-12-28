#[derive(Debug, PartialEq)]
pub enum Keyword {
    Function,
    BraceLeft,
    BraceRight,
    ParenLeft,
    ParenRight,
    SemiColon,
    Comma
}

#[derive(Debug)]
pub enum Token<'a> {
    Integer(i32),
    Symbol(&'a [u8]),
    Keyword(Keyword),
//    Error
}
