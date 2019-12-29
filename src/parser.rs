// Function Symbol ParenLeft ParenRight BraceLeft Expression BraceRight
// Expression -> Symbol Some(< [_] > :: into_vec (box [$ ($ x), *]))ght

//use crate::lexer::Lexer;
//use crate::token::Keyword;
use crate::lexer::Lexer;
use std::error;
use std::fmt;
use std::str::from_utf8_unchecked;

use crate::token::Token;

//fn parse_expression(tokens: &Vec<Token>, pos: usize) -> Option<TreeNode> {
//    let (node, _next) = parse_function_call(tokens, pos);
//    node
//}
//
//fn parse_function_call(tokens: &Vec<Token>, pos: usize) -> (Option<TreeNode>, usize) {
//    let (symbol, next) = parse_symbol(tokens, pos);
//    (match symbol {
//        Some(name) => {
//            let (arguments, rest) = parse_arguments(tokens, next);
//            (Some(TreeNode {
//                name: name,
//                children: arguments,
//            }), rest)
//        },
//        _ => (None, next),
//    })
//}
//
//fn accept_token(tokens: &Vec<Token>, pos: usize, token: Token) -> Option<usize> {
//    return tokens.get(pos).and_then(|s| {
//        if *s == token {
//            Some(pos + 1)
//        } else {
//            None
//        }
//    });
//}
//
//fn parse_arguments(tokens: &Vec<Token>, pos: usize) -> (Vec<TreeNode>, usize) {
//    match accept_token(tokens, pos, Token::keyword(Keyword::ParenLeft)) {
//        Some(argument_start) => {
//            loop {
//                let next = accept_token(tokens, pos, Token::keyword(Keyword::ParenRight));
//                if (next.is_some())
//                    ;
//
//                let token = tokens.get(pos).and_then(|s| {
//                    if s.token_type == TokenType::Keyword && s.keyword == Some(Keyword::ParenRight) {
//                        Some(s)
//                    } else {
//                        None
//                    }
//                });
//                if token.is_some() {
//                    break;
//                }
//            }
//        },
//        None => (vec!(), pos)
//    }
////
////    let token = tokens.get(pos).and_then(|s| {
////        if s.token_type == TokenType::Keyword && s.keyword == Some(Keyword::ParenLeft) {
////            Some(s)
////        } else {
////            None
////        }
////    });
////
////    if token.is_none() {
////        return ;
////    }
//}

#[derive(Debug, Clone, PartialEq)]
pub struct ParserError;

// Generation of an error is completely separate from how it is displayed.
// There's no need to be concerned about cluttering complex logic with the display style.
//
// Note that we don't store any extra info about the errors. This means we can't state
// which string failed to parse without modifying our types to carry that information.
impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parser error")
    }
}

impl error::Error for ParserError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

#[derive(Debug, PartialEq)]
struct FunctionCall {
    sym: String,
    arguments: Vec<i32>,
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

    fn accept(&mut self, s: Token) -> bool {
        if self.sym == Some(s) {
            self.sym = self.lexer.next();
            return true;
        } else {
            return false;
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

    fn expression(&mut self) -> Result<i32, ParserError> {
        match self.sym {
            Some(Token::Integer(i)) => {
                self.sym = self.lexer.next();
                return Ok(i);
            }
            _ => Err(ParserError),
        }
    }

    fn function_call(&mut self) -> Result<FunctionCall, ParserError> {
        let sym = self.symbol()?;
        self.expect(Token::ParenLeft)?;
        let mut args: Vec<i32> = vec![];
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
        let fc = FunctionCall {
            sym: sym,
            arguments: args,
        };
        return Ok(fc);
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
    let expected = FunctionCall { sym: String::from("sideeffect"), arguments: vec![] };
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call() {
    let mut parser = Parser::new(Lexer::new("sqrt(4)"));
    let res = parser.function_call();
    let expected = FunctionCall { sym: String::from("sqrt"), arguments: vec![4] };
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call_multiple_args() {
    let mut parser = Parser::new(Lexer::new("sum(4, 4)"));
    let res = parser.function_call();
    let expected = FunctionCall { sym: String::from("sum"), arguments: vec![4, 4] };
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call_many_multiple_args() {
    let mut parser = Parser::new(Lexer::new("sum(4, 1, 2,    4, 5, 223, 23,2)"));
    let res = parser.function_call();
    let expected = FunctionCall { sym: String::from("sum"), arguments: vec![4, 1, 2, 4, 5, 223, 23, 2] };
    assert_eq!(res, Ok(expected));
}

//fn parse_symbol(lexer: &mut Lexer) -> Option<TreeNode> {
//    accept_token(tokens, pos, Token::symbol())
//    let s = tokens.get(pos).and_then(|s| {
//        if s.token_type == TokenType::Symbol {
//            Some(s)
//        } else {
//            None
//        }
//    });
//
//    match s {
//        Some(s) => ( Some(s), pos + 1 ),
//        None => (None, pos)
//    }
//}
