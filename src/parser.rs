use std::str::from_utf8_unchecked;

use crate::lexer::Lexer;
use crate::parser_error::ParserError;
use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Integer(i32),
    FunctionCall(Box<FunctionCallExpr>),
    Block(Box<BlockExpr>),
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallExpr {
    sym: String,
    arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    list: Vec<Expression>,
}

pub struct Parser<'a> {
    sym: Option<Token<'a>>,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            sym: lexer.next(),
            lexer: lexer,
        }
    }

    #[cfg(test)]
    pub fn rewind(&mut self) {
        self.lexer.rewind();
        self.sym = self.lexer.next()
    }

    fn accept(&mut self, s: Token) -> bool {
        if self.sym == Some(s) {
            self.sym = self.lexer.next();
            return true;
        } else {
            return false;
        }
    }

    fn peek_symbol(&mut self) -> bool {
        match self.sym {
            Some(Token::Symbol(_s)) => true,
            _ => false,
        }
    }

    fn expect(&mut self, s: Token) -> Result<(), ParserError> {
        if self.accept(s) {
            return Ok(());
        } else {
            return Err(ParserError);
        }
    }

    fn symbol(&mut self) -> Result<String, ParserError> {
        match self.sym {
            Some(Token::Symbol(s)) => {
                let symbol_name = unsafe { from_utf8_unchecked(s) }.to_string();
                self.sym = self.lexer.next();
                return Ok(symbol_name);
            }
            _ => Err(ParserError),
        }
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        if self.peek_symbol() {
            let fc = self.function_call()?;
            return Ok(Expression::FunctionCall(Box::new(fc)));
        } else {
            match self.sym {
                Some(Token::BraceLeft) => {
                    let block = self.block()?;
                    return Ok(Expression::Block(Box::new(block)));
                }
                Some(Token::Integer(i)) => {
                    self.sym = self.lexer.next();
                    return Ok(Expression::Integer(i));
                }
                _ => return Err(ParserError),
            }
        }
    }

    fn function_call(&mut self) -> Result<FunctionCallExpr, ParserError> {
        let sym = self.symbol()?;
        self.expect(Token::ParenLeft)?;
        let mut args: Vec<Expression> = Vec::new();
        if self.sym != Some(Token::ParenRight) {
            let arg = self.expression()?;
            args.push(arg);
            loop {
                if self.accept(Token::Comma) {
                    let arg = self.expression()?;
                    args.push(arg);
                } else {
                    break;
                }
            }
        }
        self.expect(Token::ParenRight)?;
        let fc = FunctionCallExpr {
            sym: sym,
            arguments: args,
        };
        return Ok(fc);
    }

    fn block(&mut self) -> Result<BlockExpr, ParserError> {
        self.expect(Token::BraceLeft)?;
        let mut list: Vec<Expression> = Vec::new();
        if self.sym != Some(Token::BraceRight) {
            list.push(self.expression()?);
            loop {
                if self.accept(Token::SemiColon) {
                    list.push(self.expression()?);
                } else {
                    break;
                }
            }
        }
        self.expect(Token::BraceRight)?;
        return Ok(BlockExpr { list: list });
    }

    pub fn program(&mut self) -> Result<Expression, ParserError> {
        self.expression()
    }
}

#[test]
fn parse_symbol() {
    let contents = String::from("apa");
    let tokens = Lexer::new(&contents);
    let mut parser = Parser::new(tokens);
    let res = parser.symbol();
    assert_eq!(Ok(contents), res);
}

#[test]
fn parse_function_call_no_args() {
    let mut parser = Parser::new(Lexer::new("sideeffect()"));
    let res = parser.function_call();
    let expected = FunctionCallExpr {
        sym: String::from("sideeffect"),
        arguments: vec![],
    };
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call() {
    let mut parser = Parser::new(Lexer::new("sqrt(4)"));
    let res = parser.function_call();
    let expected = FunctionCallExpr {
        sym: String::from("sqrt"),
        arguments: vec![Expression::Integer(4)],
    };
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call_multiple_args() {
    let mut parser = Parser::new(Lexer::new("sum(4, 4)"));
    let res = parser.function_call();
    let expected = FunctionCallExpr {
        sym: String::from("sum"),
        arguments: vec![Expression::Integer(4), Expression::Integer(4)],
    };
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call_many_multiple_args() {
    let mut parser = Parser::new(Lexer::new("sum(4, 1, 2,    4, 5, 223, 23,2)"));
    let res = parser.function_call();
    let expected = FunctionCallExpr {
        sym: String::from("sum"),
        arguments: vec![4, 1, 2, 4, 5, 223, 23, 2]
            .into_iter()
            .map(|i| Expression::Integer(i))
            .collect(),
    };
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call_as_expression() {
    let mut parser = Parser::new(Lexer::new("sqrt(4)"));
    let res = parser.expression();
    let expected = Expression::FunctionCall(Box::new(FunctionCallExpr {
        sym: String::from("sqrt"),
        arguments: vec![4]
            .into_iter()
            .map(|i| Expression::Integer(i))
            .collect(),
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_nested_function_call_as_expression() {
    let mut parser = Parser::new(Lexer::new("sqrt(sqrt(81))"));
    let res = parser.expression();
    let expected = Expression::FunctionCall(Box::new(FunctionCallExpr {
        sym: String::from("sqrt"),
        arguments: vec![Expression::FunctionCall(Box::new(FunctionCallExpr {
            sym: String::from("sqrt"),
            arguments: vec![81]
                .into_iter()
                .map(|i| Expression::Integer(i))
                .collect(),
        }))],
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_number_as_expression() {
    let mut parser = Parser::new(Lexer::new("4"));
    let res = parser.expression();
    let expected = Expression::Integer(4);
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_block() {
    let lexer = Lexer::new("{ print(9); print(3) }");
    let mut parser = Parser::new(lexer);
    let res = parser.block();
    let expected = || BlockExpr {
        list: vec![
            Expression::FunctionCall(Box::new(FunctionCallExpr {
                sym: String::from("print"),
                arguments: vec![Expression::Integer(9)],
            })),
            Expression::FunctionCall(Box::new(FunctionCallExpr {
                sym: String::from("print"),
                arguments: vec![Expression::Integer(3)],
            })),
        ],
    };
    assert_eq!(res, Ok(expected()));

    parser.rewind();
    let expected_in_expression = Expression::Block(Box::new(expected()));
    let res_in_expr = parser.expression();
    assert_eq!(res_in_expr, Ok(expected_in_expression));
}
