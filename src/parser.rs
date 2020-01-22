use crate::expression::FunctionDeclaration;
use crate::expression::TypeDeclaration;
use crate::lexer::Lexer;
use crate::string::interpret_escaped_string;
use crate::token::Token;

use std::error;
use std::fmt;
use std::rc::Rc;
use std::str::from_utf8_unchecked;

use crate::expression::{
    BinaryExpr, BindExpr, BlockExpr, BreakExpr, ConditionalExpr, Expression, FunctionCallExpr,
    FunctionExpr, GroupExpr, LoopExpr, Operator, TypeQualifiedExpressionExpr,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parser error: {}", self.message)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl Error {
    pub fn new(message: String) -> Error {
        Error { message }
    }
}

pub struct Parser<'a> {
    sym: Option<Token<'a>>,
    lexer: Lexer<'a>,
}

enum Assoc {
    Left,
    Right,
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

    fn sym_as_str(&self) -> String {
        self.sym
            .as_ref()
            .map(|t| t.to_string())
            .unwrap_or_else(|| String::from("<none>"))
    }

    fn accept(&mut self, s: Token) -> bool {
        if self.sym == Some(s) {
            self.sym = self.lexer.next();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, s: Token) -> Result<(), Error> {
        if self.accept(s) {
            Ok(())
        } else {
            Err(Error::new(format!(
                "unexpected token '{}'",
                self.sym_as_str()
            )))
        }
    }

    fn expect_none(&mut self) -> Result<(), Error> {
        if self.sym == None {
            Ok(())
        } else {
            Err(Error::new(format!(
                "unexpected token '{}'",
                self.sym_as_str()
            )))
        }
    }

    fn symbol(&mut self) -> Result<String, Error> {
        match self.sym {
            Some(Token::Symbol(s)) => {
                let symbol_name = unsafe { from_utf8_unchecked(s) }.to_string();
                self.sym = self.lexer.next();
                Ok(symbol_name)
            }
            _ => Err(Error::new(format!(
                "unexpected token {} found, expected symbol",
                self.sym_as_str()
            ))),
        }
    }

    fn operator(&mut self) -> Result<Operator, Error> {
        match self.sym {
            Some(Token::Plus) => {
                self.sym = self.lexer.next();
                Ok(Operator::Sum)
            }
            Some(Token::Minus) => {
                self.sym = self.lexer.next();
                Ok(Operator::Difference)
            }
            Some(Token::Slash) => {
                self.sym = self.lexer.next();
                Ok(Operator::Divide)
            }
            Some(Token::Star) => {
                self.sym = self.lexer.next();
                Ok(Operator::Multiply)
            }
            Some(Token::StarStar) => {
                self.sym = self.lexer.next();
                Ok(Operator::ToThePowerOf)
            }
            Some(Token::Less) => {
                self.sym = self.lexer.next();
                Ok(Operator::LessThan)
            }
            Some(Token::Greater) => {
                self.sym = self.lexer.next();
                Ok(Operator::GreaterThan)
            }
            Some(Token::GreaterEqual) => {
                self.sym = self.lexer.next();
                Ok(Operator::GreaterEqualThan)
            }
            Some(Token::LessEqual) => {
                self.sym = self.lexer.next();
                Ok(Operator::LessEqualThan)
            }
            Some(Token::EqualEqual) => {
                self.sym = self.lexer.next();
                Ok(Operator::Equal)
            }
            Some(Token::Equal) => {
                self.sym = self.lexer.next();
                Ok(Operator::Assign)
            }
            Some(Token::ParenLeft) => {
                self.sym = self.lexer.next();
                Ok(Operator::Call)
            }
            Some(Token::BracketLeft) => {
                self.sym = self.lexer.next();
                Ok(Operator::TypeArguments)
            }
            _ => Err(Error::new(format!(
                "unexpected token {} found, expected operator",
                self.sym_as_str()
            ))),
        }
    }

    fn function(&mut self) -> Result<FunctionExpr, Error> {
        self.expect(Token::Function)?;
        let sym = if self.sym != Some(Token::ParenLeft) {
            Some(self.symbol()?)
        } else {
            None
        };
        self.expect(Token::ParenLeft)?;
        let mut parameters: Vec<(String, TypeDeclaration)> = Vec::new();
        if self.sym != Some(Token::ParenRight) {
            let parameter = self.symbol()?;
            self.expect(Token::Colon)?;
            let parameter_type = self.type_declaration()?;
            parameters.push((parameter, parameter_type));
            loop {
                if self.accept(Token::Comma) {
                    let parameter = self.symbol()?;
                    self.expect(Token::Colon)?;
                    let parameter_type = self.type_declaration()?;
                    parameters.push((parameter, parameter_type));
                } else {
                    break;
                }
            }
        }
        self.expect(Token::ParenRight)?;

        let return_type = if self.accept(Token::RightArrow) {
            Some(self.type_declaration()?)
        } else {
            None
        };

        let expr = if self.sym == Some(Token::BraceLeft) {
            Expression::Block(Rc::new(self.block()?))
        } else {
            self.expect(Token::Equal)?;
            self.expression()?
        };

        let function = FunctionExpr {
            sym,
            return_type,
            parameters,
            expr,
        };
        Ok(function)
    }

    fn type_declaration(&mut self) -> Result<TypeDeclaration, Error> {
        if self.sym == Some(Token::ParenLeft) {
            let function_declaration = self.function_declaration()?;
            Ok(TypeDeclaration::Function(function_declaration))
        } else {
            let symbol = self.symbol()?;
            Ok(TypeDeclaration::Symbol(symbol))
        }
    }

    fn function_declaration(&mut self) -> Result<FunctionDeclaration, Error> {
        self.expect(Token::ParenLeft)?;

        let mut parameters: Vec<TypeDeclaration> = Vec::new();

        if self.sym != Some(Token::ParenRight) {
            let parameter_type = self.type_declaration()?;
            parameters.push(parameter_type);
            loop {
                if self.accept(Token::Comma) {
                    let parameter_type = self.type_declaration()?;
                    parameters.push(parameter_type);
                } else {
                    break;
                }
            }
        }

        self.expect(Token::ParenRight)?;
        self.expect(Token::RightArrow)?;
        let return_type = Rc::new(self.type_declaration()?);
        Ok(FunctionDeclaration {
            parameters,
            return_type,
        })
    }

    pub fn expression(&mut self) -> Result<Expression, Error> {
        self.expression_from_precedence(0)
    }

    fn next_min_prec(&self) -> (Option<i32>, Assoc) {
        match self.sym {
            Some(Token::Equal) => (Some(0), Assoc::Right),
            Some(Token::Less)
            | Some(Token::LessEqual)
            | Some(Token::Greater)
            | Some(Token::GreaterEqual)
            | Some(Token::EqualEqual) => (Some(1), Assoc::Left),
            Some(Token::Plus) | Some(Token::Minus) => (Some(2), Assoc::Left),
            Some(Token::Star) | Some(Token::Slash) => (Some(3), Assoc::Left),
            Some(Token::StarStar) => (Some(4), Assoc::Right),
            Some(Token::ParenLeft) | Some(Token::BracketLeft) => (Some(5), Assoc::Left),
            _ => (None, Assoc::Left),
        }
    }

    fn expression_from_precedence(&mut self, min_prec: i32) -> Result<Expression, Error> {
        let mut expr = self.expression_atom()?;
        loop {
            match self.next_min_prec() {
                (Some(prec), assoc) => {
                    if prec < min_prec {
                        break;
                    }
                    let next_min_prec = match assoc {
                        Assoc::Left => prec + 1,
                        Assoc::Right => prec,
                    };
                    let operator = self.operator()?;
                    expr = match &operator {
                        Operator::Call => {
                            let arguments = self.arguments()?;
                            Expression::FunctionCall(Rc::new(FunctionCallExpr { expr, arguments }))
                        }
                        Operator::TypeArguments => {
                            let type_arguments = self.type_arguments()?;
                            Expression::TypeQualifiedExpression(Rc::new(
                                TypeQualifiedExpressionExpr {
                                    expr,
                                    type_arguments,
                                },
                            ))
                        }
                        _ => {
                            let right = self.expression_from_precedence(next_min_prec)?;
                            Expression::Binary(Rc::new(BinaryExpr {
                                operator,
                                left: expr,
                                right,
                            }))
                        }
                    }
                }
                _ => {
                    break;
                }
            }
        }
        Ok(expr)
    }

    fn expression_atom(&mut self) -> Result<Expression, Error> {
        match self.sym {
            Some(Token::Symbol(s)) => {
                let symbol_name = unsafe { from_utf8_unchecked(s) }.to_string();
                self.sym = self.lexer.next();
                Ok(Expression::Symbol(symbol_name))
            }
            Some(Token::String(s)) => {
                let string = interpret_escaped_string(unsafe { from_utf8_unchecked(s) })
                    .map_err(|e| Error::new(e.to_string()))?;
                self.sym = self.lexer.next();
                Ok(Expression::String(string))
            }
            Some(Token::Function) => {
                let function = self.function()?;
                Ok(Expression::Function(Rc::new(function)))
            }
            Some(Token::If) => {
                let conditional = self.conditional()?;
                Ok(Expression::Conditional(Rc::new(conditional)))
            }
            Some(Token::Loop) => {
                let loop_expr = self.r#loop()?;
                Ok(Expression::Loop(Rc::new(loop_expr)))
            }
            Some(Token::ParenLeft) => {
                let group = self.group()?;
                Ok(Expression::Group(Rc::new(group)))
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
            Some(Token::True) => {
                self.sym = self.lexer.next();
                Ok(Expression::Bool(true))
            }
            Some(Token::False) => {
                self.sym = self.lexer.next();
                Ok(Expression::Bool(false))
            }
            Some(Token::BraceRight) => Ok(Expression::Void),
            Some(Token::Break) => {
                let break_expr = self.r#break()?;
                Ok(Expression::Break(Rc::new(break_expr)))
            }
            Some(Token::Else)
            | Some(Token::Equal)
            | Some(Token::EqualEqual)
            | Some(Token::Greater)
            | Some(Token::Less)
            | Some(Token::GreaterEqual)
            | Some(Token::LessEqual)
            | Some(Token::Plus)
            | Some(Token::Minus)
            | Some(Token::Star)
            | Some(Token::StarStar)
            | Some(Token::Slash)
            | Some(Token::ParenRight)
            | Some(Token::BracketLeft)
            | Some(Token::BracketRight)
            | Some(Token::Colon)
            | Some(Token::SemiColon)
            | Some(Token::Comma)
            | Some(Token::Dot)
            | Some(Token::RightArrow) => Err(Error::new(format!(
                "unexpected token: {}",
                self.sym_as_str()
            ))),
            None => Err(Error::new(format!(
                "unexpected EOF; expected part of expression"
            ))),
        }
    }

    fn r#break(&mut self) -> Result<BreakExpr, Error> {
        self.expect(Token::Break)?;
        let expr = self.expression()?;
        Ok(BreakExpr { expr })
    }

    fn r#loop(&mut self) -> Result<LoopExpr, Error> {
        self.expect(Token::Loop)?;
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
        Ok(LoopExpr { list })
    }

    fn conditional(&mut self) -> Result<ConditionalExpr, Error> {
        self.expect(Token::If)?;
        let condition = self.expression()?;
        let true_branch = self.expression()?;
        let false_branch = if self.accept(Token::Else) {
            Some(self.expression()?)
        } else {
            None
        };
        Ok(ConditionalExpr {
            condition,
            true_branch,
            false_branch,
        })
    }

    fn group(&mut self) -> Result<GroupExpr, Error> {
        self.expect(Token::ParenLeft)?;
        let expr = self.expression()?;
        self.expect(Token::ParenRight)?;
        Ok(GroupExpr { expr })
    }

    fn arguments(&mut self) -> Result<Vec<Expression>, Error> {
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
        Ok(args)
    }

    fn type_arguments(&mut self) -> Result<Vec<TypeDeclaration>, Error> {
        let mut args: Vec<TypeDeclaration> = Vec::new();
        if self.sym != Some(Token::BracketRight) {
            let arg = self.type_declaration()?;
            args.push(arg);
            loop {
                if self.accept(Token::Comma) {
                    let arg = self.type_declaration()?;
                    args.push(arg);
                } else {
                    break;
                }
            }
        }
        self.expect(Token::BracketRight)?;
        Ok(args)
    }

    fn block(&mut self) -> Result<BlockExpr, Error> {
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

    fn binding(&mut self) -> Result<BindExpr, Error> {
        self.expect(Token::Let)?;
        let sym = self.symbol()?;
        let sym_type = if self.accept(Token::Colon) {
            Some(self.type_declaration()?)
        } else {
            None
        };
        self.expect(Token::Equal)?;
        Ok(BindExpr {
            sym,
            sym_type,
            expr: self.expression()?,
        })
    }

    pub fn program(&mut self) -> Result<Expression, Error> {
        let mut list: Vec<Expression> = Vec::new();
        if self.sym != None {
            list.push(self.expression()?);
            loop {
                if self.accept(Token::SemiColon) {
                    list.push(self.expression()?);
                } else {
                    break;
                }
            }
        }

        self.expect_none()?;
        Ok(Expression::Program(Rc::new(BlockExpr { list })))
    }
}

#[test]
fn parse_type_parameter() {
    let tokens = Lexer::new("array[int]()");
    let mut parser = Parser::new(tokens);
    let res = parser.expression().unwrap();
    assert_eq!(
        Expression::FunctionCall(Rc::new(FunctionCallExpr {
            expr: Expression::TypeQualifiedExpression(Rc::new(TypeQualifiedExpressionExpr {
                expr: Expression::Symbol(String::from("array")),
                type_arguments: vec![TypeDeclaration::Symbol(String::from("int"))]
            })),
            arguments: vec![],
        })),
        res
    );
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
    let res = parser.expression();
    let expected = Expression::FunctionCall(Rc::new(FunctionCallExpr {
        expr: Expression::Symbol(String::from("sideeffect")),
        arguments: vec![],
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call() {
    let mut parser = Parser::new(Lexer::new("test_func(4, 2)"));
    let res = parser.expression();
    let expected = Expression::FunctionCall(Rc::new(FunctionCallExpr {
        expr: Expression::Symbol(String::from("test_func")),
        arguments: vec![Expression::Integer(4), Expression::Integer(2)],
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_declaration() {
    let mut parser = Parser::new(Lexer::new("fn double(x: int) -> int = x + x"));
    let res = parser.expression();
    let expected = Expression::Function(Rc::new(FunctionExpr {
        sym: Some("double".to_string()),
        return_type: Some(TypeDeclaration::Symbol("int".to_string())),
        parameters: vec![("x".to_string(), TypeDeclaration::Symbol("int".to_string()))],
        expr: Expression::Binary(Rc::new(BinaryExpr {
            operator: Operator::Sum,
            left: Expression::Symbol("x".to_string()),
            right: Expression::Symbol("x".to_string()),
        })),
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_declaration_no_equal() {
    let mut parser = Parser::new(Lexer::new("fn double(x: int) -> int x + x"));
    let res = parser.expression();
    assert!(res.is_err());
}

#[test]
fn parse_function_declaration_block() {
    let mut parser = Parser::new(Lexer::new("fn double(x: int) -> int { x + x }"));
    let res = parser.expression();
    let expected = Expression::Function(Rc::new(FunctionExpr {
        sym: Some("double".to_string()),
        return_type: Some(TypeDeclaration::Symbol("int".to_string())),
        parameters: vec![("x".to_string(), TypeDeclaration::Symbol("int".to_string()))],
        expr: Expression::Block(Rc::new(BlockExpr {
            list: vec![Expression::Binary(Rc::new(BinaryExpr {
                operator: Operator::Sum,
                left: Expression::Symbol("x".to_string()),
                right: Expression::Symbol("x".to_string()),
            }))],
        })),
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_declaration_block_2() {
    let mut parser = Parser::new(Lexer::new("fn double(x: int) -> int = { x + x }"));
    let res = parser.expression();
    let expected = Expression::Function(Rc::new(FunctionExpr {
        sym: Some("double".to_string()),
        return_type: Some(TypeDeclaration::Symbol("int".to_string())),
        parameters: vec![("x".to_string(), TypeDeclaration::Symbol("int".to_string()))],
        expr: Expression::Block(Rc::new(BlockExpr {
            list: vec![Expression::Binary(Rc::new(BinaryExpr {
                operator: Operator::Sum,
                left: Expression::Symbol("x".to_string()),
                right: Expression::Symbol("x".to_string()),
            }))],
        })),
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call_multiple_args() {
    let mut parser = Parser::new(Lexer::new("sum(4, 4)"));
    let res = parser.expression();
    let expected = Expression::FunctionCall(Rc::new(FunctionCallExpr {
        expr: Expression::Symbol(String::from("sum")),
        arguments: vec![Expression::Integer(4), Expression::Integer(4)],
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call_many_multiple_args() {
    let mut parser = Parser::new(Lexer::new("sum(4, 1, 2,    4, 5, 223, 23,2)"));
    let res = parser.expression();
    let expected = Expression::FunctionCall(Rc::new(FunctionCallExpr {
        expr: Expression::Symbol(String::from("sum")),
        arguments: vec![4, 1, 2, 4, 5, 223, 23, 2]
            .into_iter()
            .map(|i| Expression::Integer(i))
            .collect(),
    }));
    assert_eq!(res, Ok(expected));
}

#[test]
fn parse_function_call_as_expression() {
    let mut parser = Parser::new(Lexer::new("sqrt(4)"));
    let res = parser.expression();
    let expected = Expression::FunctionCall(Rc::new(FunctionCallExpr {
        expr: Expression::Symbol(String::from("sqrt")),
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
        expr: Expression::Symbol(String::from("sqrt")),
        arguments: vec![Expression::FunctionCall(Rc::new(FunctionCallExpr {
            expr: Expression::Symbol(String::from("sqrt")),
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
    let mut parser = Parser::new(Lexer::new("fn identity(x: int) -> int = { x }"));
    let res = parser.expression();
    let expected = Expression::Function(Rc::new(FunctionExpr {
        sym: Some(String::from("identity")),
        return_type: Some(TypeDeclaration::Symbol(String::from("int"))),
        parameters: vec![(
            String::from("x"),
            TypeDeclaration::Symbol(String::from("int")),
        )],
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
                expr: Expression::Symbol(String::from("print")),
                arguments: vec![Expression::Integer(9)],
            })),
            Expression::FunctionCall(Rc::new(FunctionCallExpr {
                expr: Expression::Symbol(String::from("print")),
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
    let lexer = Lexer::new("let a: int = 4");
    let mut parser = Parser::new(lexer);
    let res = parser.binding();
    let expected = || BindExpr {
        sym: String::from("a"),
        sym_type: Some(TypeDeclaration::Symbol(String::from("int"))),
        expr: Expression::Integer(4),
    };
    assert_eq!(res, Ok(expected()));

    parser.rewind();
    let expected_in_expression = Expression::Bind(Rc::new(expected()));
    let res_in_expr = parser.expression();
    assert_eq!(res_in_expr, Ok(expected_in_expression));
}

#[test]
fn parse_nested_call_expr() {
    let mut parser = Parser::new(Lexer::new("apa()()()"));
    let res_in_expr = parser.expression();
    let expected = Expression::FunctionCall(Rc::new(FunctionCallExpr {
        expr: Expression::FunctionCall(Rc::new(FunctionCallExpr {
            expr: Expression::FunctionCall(Rc::new(FunctionCallExpr {
                expr: Expression::Symbol(String::from("apa")),
                arguments: vec![],
            })),
            arguments: vec![],
        })),
        arguments: vec![],
    }));
    assert_eq!(res_in_expr, Ok(expected));
}

#[test]
fn parse_infix() {
    let mut parser = Parser::new(Lexer::new("1 + 2"));
    let expected = Expression::Binary(Rc::new(BinaryExpr {
        operator: Operator::Sum,
        left: Expression::Integer(1),
        right: Expression::Integer(2),
    }));
    let expression = parser.expression();
    assert_eq!(expression, Ok(expected));
}

#[test]
fn parse_infix_no_space() {
    let mut parser = Parser::new(Lexer::new("1+2"));
    let expected = Expression::Binary(Rc::new(BinaryExpr {
        operator: Operator::Sum,
        left: Expression::Integer(1),
        right: Expression::Integer(2),
    }));
    let expression = parser.expression();
    assert_eq!(expression, Ok(expected));
}

#[test]
fn parse_string_concatination() {
    let mut parser = Parser::new(Lexer::new("\"apa\" + \"banan\""));
    let expected = Expression::Binary(Rc::new(BinaryExpr {
        operator: Operator::Sum,
        left: Expression::String("apa".to_string()),
        right: Expression::String("banan".to_string()),
    }));
    let expression = parser.expression();
    assert_eq!(expression, Ok(expected));
}
