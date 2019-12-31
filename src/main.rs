extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod token;
mod lexer;
mod parser_error;
mod parser;
mod interpreter;

use lexer::Lexer;
use parser::Parser;
use parser::Expression; // for test
use interpreter::Interpreter;

fn main() {
// `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut interpreter = Interpreter::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let tokens = Lexer::new(line.as_str());
                match Parser::new(tokens).program() {
                    Ok(program) => {
                        let res = interpreter.eval(&program);
                        println!("{:?}", res);
                    }
                    Err(e) => {
                        println!("Failed to parse: {:?}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}

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
