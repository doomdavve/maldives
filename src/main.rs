use std::fs;

mod token;
mod lexer;
mod parser_error;
mod parser;
mod interpreter;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;

fn main() {
    let contents = fs::read_to_string("main.ur")
        .expect("Something went wrong reading the file");
    println!("'{}' read as content", contents);
    let tokens = Lexer::new(&contents);
    let program = Parser::new(tokens).program();
    println!("{:?}", program);

    let mut interpreter = Interpreter::new();
    let res = interpreter.eval(program.unwrap());
    println!("{:?}", res);
}

#[test]
fn eval_simple() {
    let mut parser = Parser::new(Lexer::new("apa"));
    let expression = parser.program().unwrap();

    let mut interpreter = Interpreter::new();
    interpreter.set("apa".to_string(), 4);
    let res = interpreter.eval(expression);
    assert_eq!(Ok(4), res);
}

#[test]
fn eval_simple_assignment() {
    let mut parser = Parser::new(Lexer::new("{ let apa = 3; apa }"));
    let expression = parser.program().unwrap();

    let mut interpreter = Interpreter::new();
    let res = interpreter.eval(expression);
    assert_eq!(Ok(3), res);
}
