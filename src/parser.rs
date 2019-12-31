use std::rc::Rc;
use std::str::from_utf8_unchecked;

use crate::lexer::Lexer;
use crate::parser_error::ParserError;
use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Integer(i32),
    Function(Rc<FunctionExpr>),
    FunctionCall(Rc<FunctionCallExpr>),
    Bind(Rc<BindExpr>),
    Block(Rc<BlockExpr>),
    Symbol(String),
    Void
}

#[derive(Debug, PartialEq)]
pub struct BindExpr {
    pub sym: String,
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallExpr {
    pub sym: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionExpr {
    pub sym: String,
    pub parameters: Vec<String>,
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub list: Vec<Expression>,
}

pub struct Parser<'a> {
    sym: Option<Token<'a>>,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            sym: lexer.next(),
            lexer,
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
            true
        } else {
            false
        }
    }

    fn at_symbol(&mut self) -> bool {
        match self.sym {
            Some(Token::Symbol(_s)) => true,
            _ => false,
        }
    }

    fn peek(&mut self, s: Token) -> bool {
        let peeked = self.lexer.peek();
        peeked == Some(s)
    }

    fn expect(&mut self, s: Token) -> Result<(), ParserError> {
        if self.accept(s) {
            Ok(())
        } else {
            Err(ParserError)
        }
    }

    fn symbol(&mut self) -> Result<String, ParserError> {
        match self.sym {
            Some(Token::Symbol(s)) => {
                let symbol_name = unsafe { from_utf8_unchecked(s) }.to_string();
                self.sym = self.lexer.next();
                Ok(symbol_name)
            }
            _ => Err(ParserError),
        }
    }

    fn function(&mut self) -> Result<FunctionExpr, ParserError> {
        self.expect(Token::Function)?;
        let sym = self.symbol()?;
        self.expect(Token::ParenLeft)?;
        let mut parameters: Vec<String> = Vec::new();
        if self.sym != Some(Token::ParenRight) {
            let parameter = self.symbol()?;
            parameters.push(parameter);
            loop {
                if self.accept(Token::Comma) {
                    let parameter = self.symbol()?;
                    parameters.push(parameter);
                } else {
                    break;
                }
            }
        }
        self.expect(Token::ParenRight)?;
        let expr = self.expression()?;
        let function = FunctionExpr {
            sym,
            parameters,
            expr,
        };
        Ok(function)
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        if self.at_symbol() {
            if self.peek(Token::ParenLeft) {
                let fc = self.function_call()?;
                Ok(Expression::FunctionCall(Rc::new(fc)))
            } else {
                let symbol = self.symbol()?;
                Ok(Expression::Symbol(symbol))
            }
        } else {
            match self.sym {
                Some(Token::Function) => {
                    let function = self.function()?;
                    Ok(Expression::Function(Rc::new(function)))
                }
                Some(Token::Let) => {
                    let binding = self.binding()?;
                    Ok(Expression::Bind(Rc::new(binding)))
                }
                Some(Token::BraceLeft) => {
                    let block = self.block()?;
                    Ok(Expression::Block(Rc::new(block)))
                }
                Some(Token::Integer(i)) => {
                    self.sym = self.lexer.next();
                    Ok(Expression::Integer(i))
                }
                _ => Err(ParserError),
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
            sym,
            arguments: args,
        };
        Ok(fc)
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
        Ok(BlockExpr { list })
    }

    fn binding(&mut self) -> Result<BindExpr, ParserError> {
        self.expect(Token::Let)?;
        let sym = self.symbol()?;
        self.expect(Token::Equal)?;
        Ok(BindExpr {
            sym,
            expr: self.expression()?,
        })
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
fn parse_symbol_as_expression() {
    let contents = String::from("apa");
    let tokens = Lexer::new(&contents);
    let mut parser = Parser::new(tokens);
    let res = parser.expression();
    assert_eq!(Ok(Expression::Symbol(contents)), res);
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
    let expected = Expression::FunctionCall(Rc::new(FunctionCallExpr {
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
    let expected = Expression::FunctionCall(Rc::new(FunctionCallExpr {
        sym: String::from("sqrt"),
        arguments: vec![Expression::FunctionCall(Rc::new(FunctionCallExpr {
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
fn parse_define_function() {
    let mut parser = Parser::new(Lexer::new("fn identity(x) { x }"));
    let res = parser.expression();
    let expected = Expression::Function(Rc::new(FunctionExpr {
        sym: String::from("identity"),
        parameters: vec![String::from("x")],
        expr: Expression::Block(Rc::new(BlockExpr {
            list: vec![Expression::Symbol(String::from("x"))],
        })),
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
            Expression::FunctionCall(Rc::new(FunctionCallExpr {
                sym: String::from("print"),
                arguments: vec![Expression::Integer(9)],
            })),
            Expression::FunctionCall(Rc::new(FunctionCallExpr {
                sym: String::from("print"),
                arguments: vec![Expression::Integer(3)],
            })),
        ],
    };
    assert_eq!(res, Ok(expected()));

    parser.rewind();
    let expected_in_expression = Expression::Block(Rc::new(expected()));
    let res_in_expr = parser.expression();
    assert_eq!(res_in_expr, Ok(expected_in_expression));
}

#[test]
fn parse_bind() {
    let lexer = Lexer::new("let a = 4");
    let mut parser = Parser::new(lexer);
    let res = parser.binding();
    let expected = || BindExpr {
        sym: String::from("a"),
        expr: Expression::Integer(4),
    };
    assert_eq!(res, Ok(expected()));

    parser.rewind();
    let expected_in_expression = Expression::Bind(Rc::new(expected()));
    let res_in_expr = parser.expression();
    assert_eq!(res_in_expr, Ok(expected_in_expression));
}
