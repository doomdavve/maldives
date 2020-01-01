extern crate rustyline;

#[macro_use]
extern crate log;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod interpreter;
mod lexer;
mod parse_error;
mod parser;
mod token;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut interpreter = Interpreter::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let tokens = Lexer::new(line.as_str());
                match Parser::new(tokens).program() {
                    Ok(program) => {
                        debug!("Parsed program: {:?}", program);
                        let res = interpreter.eval(&program);
                        match res {
                            Ok(a) => println!("{:?}", a),
                            Err(e) => println!("{}", e),
                        }
                    }
                    Err(e) => {
                        println!("Failed to parse: {:?}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Exit.");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Exit.");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}

#[cfg(test)]
use parser::Expression;

#[test]
fn eval_simple() {
    let mut parser = Parser::new(Lexer::new("apa"));
    let expression = parser.program().unwrap();

    let mut interpreter = Interpreter::new();
    interpreter.set("apa".to_string(), Expression::Integer(4));
    let res = interpreter.eval(&expression);
    assert_eq!(Ok(Expression::Integer(4)), res);
}

#[test]
fn eval_simple_assignment() {
    let mut parser = Parser::new(Lexer::new("{ let apa = 3; apa }"));
    let expression = parser.program().unwrap();

    let mut interpreter = Interpreter::new();
    let res = interpreter.eval(&expression);
    assert_eq!(Ok(Expression::Integer(3)), res);
}

#[test]
fn eval_assignment() {
    let mut parser = Parser::new(Lexer::new("let apa = 3"));
    let expression = parser.program().unwrap();

    let mut interpreter = Interpreter::new();
    let res = interpreter.eval(&expression);
    assert_eq!(Ok(Expression::Integer(3)), res);
}

#[test]
fn eval_anon_function() {
    let mut parser = Parser::new(Lexer::new("{ let apa = fn () 3; apa() }"));
    let expression = parser.program().unwrap();

    let mut interpreter = Interpreter::new();
    let res = interpreter.eval(&expression);
    assert_eq!(Ok(Expression::Integer(3)), res);
}

#[test]
fn eval_anon_function_with_arg() {
    let mut parser = Parser::new(Lexer::new("{ let apa = fn (x) x; apa(3) }"));
    let expression = parser.program().unwrap();

    let mut interpreter = Interpreter::new();
    let res = interpreter.eval(&expression);
    assert_eq!(Ok(Expression::Integer(3)), res);
}
