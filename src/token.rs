#[derive(Debug)]
pub enum Keyword {
    Function,
    BraceLeft,
    BraceRight,
    ParanLeft,
    ParanRight,
    Comma
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum TokenType {
    Integer,
    Symbol,
    Keyword,
    Error
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub integer_value: i32,
    pub symbol: Option<String>,
    pub keyword: Option<Keyword>
}

impl Token {
    pub fn integer(integer: i32) -> Token {
        Token {
            token_type: TokenType::Integer,
            integer_value: integer,
            symbol: None,
            keyword: None
        }
    }
    pub fn symbol(sym: &str) -> Token {
        Token {
            token_type: TokenType::Symbol,
            integer_value: 0,
            symbol: Some(sym.to_string()),
            keyword: None
        }
    }
    pub fn keyword(keyword: Keyword) -> Token {
        Token {
            token_type: TokenType::Keyword,
            integer_value: 0,
            symbol: None,
            keyword: Some(keyword)
        }
    }
    pub fn error() -> Token {
        Token {
            token_type: TokenType::Error,
            integer_value: 0,
            symbol: None,
            keyword: None
        }
    }
}
