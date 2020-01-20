use std::fmt;

struct InterpretString<'a> {
    s: std::str::Chars<'a>,
}

impl<'a> InterpretString<'a> {
    pub fn new(s: &'a str) -> Self {
        InterpretString { s: s.chars() }
    }
}

impl Iterator for InterpretString<'_> {
    type Item = Result<char, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.s.next().map(|c| match c {
            '\\' => match self.s.next() {
                Some('n') => Ok('\n'),
                Some('\\') => Ok('\\'),
                Some('\"') => Ok('"'),
                Some('t') => Ok('\t'),
                Some('r') => Ok('\r'),
                Some(c) => Err(Error::UnrecognizedEscapedChar(c)),
                None => Err(Error::EscapeAtEndOfString),
            },
            c => Ok(c),
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    EscapeAtEndOfString,
    UnrecognizedEscapedChar(char),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::EscapeAtEndOfString => write!(f, "Escape character at the end of the string"),
            Error::UnrecognizedEscapedChar(c) => write!(f, "Unrecognized escaped char: '{}'", c),
        }
    }
}

impl std::error::Error for Error {}

pub fn interpret_escaped_string(s: &str) -> Result<String, Error> {
    InterpretString::new(s).collect()
}
