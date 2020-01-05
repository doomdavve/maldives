extern crate dirs;
extern crate rustyline;

#[macro_use]
extern crate log;
extern crate env_logger;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod expression;
mod lexer;
mod parse_error;
mod parser;
mod token;
mod symboltable;
mod typechecker;
mod interpreter;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

use std::path::Path;

fn main() {
    env_logger::init();

    let mut rl = Editor::<()>::new();
    let homedir = dirs::home_dir().unwrap();
    let history_file_path = Path::new(&homedir).join(".maldives_history");
    if rl.load_history(&history_file_path).is_err() {
        println!("No previous history.");
    }
    let mut interpreter = Interpreter::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if line != "" {
                    rl.add_history_entry(line.as_str());
                    let tokens = Lexer::new(line.as_str());
                    match Parser::new(tokens).program() {
                        Ok(program) => {
                            debug!("Parsed program: {:?}", program);
                            let res = interpreter.eval_global(&program);
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
    rl.save_history(&history_file_path).unwrap();
}

#[cfg(test)]
use expression::Expression;

#[test]
fn eval_simple() {
    let mut parser = Parser::new(Lexer::new("apa"));
    let expression = parser.program().unwrap();
    let mut interpreter = Interpreter::new();
    interpreter.set("apa".to_string(), Expression::Integer(4));
    let res = interpreter.eval_global(&expression);
    assert_eq!(Ok(Expression::Integer(4)), res);
}

#[cfg(test)]
fn eval_program(contents: &str) -> std::result::Result<Expression, interpreter::InterpreterError> {
    let mut parser = Parser::new(Lexer::new(contents));
    let expression = parser.program().unwrap();
    let mut interpreter = Interpreter::new();
    interpreter.eval_global(&expression)
}

#[test]
fn eval_simple_assignment() {
    assert_eq!(
        Ok(Expression::Integer(3)),
        eval_program("{ let apa = 3; apa }")
    );
}

#[test]
fn eval_assignment() {
    assert_eq!(Ok(Expression::Integer(3)), eval_program("let apa = 3"));
}

#[test]
fn eval_anon_function() {
    assert_eq!(
        Ok(Expression::Integer(3)),
        eval_program("{ let apa = fn () 3; apa() }")
    );
}

#[test]
fn eval_anon_function_with_arg() {
    assert_eq!(
        Ok(Expression::Integer(3)),
        eval_program("{ let apa = fn (x) x; apa(3) }")
    );
}

#[test]
fn eval_anon_function_as_arg() {
    assert_eq!(
        Ok(Expression::Integer(6)),
        eval_program(
            "{ let apply = fn (x, arg) x(arg); let doubler = fn (i) i*2; apply(doubler, 3) }"
        )
    );
}

#[test]
fn eval_infix() {
    assert_eq!(Ok(Expression::Integer(5)), eval_program("3 + 2"));
}

#[test]
fn eval_infix_in_succession() {
    assert_eq!(Ok(Expression::Integer(15)), eval_program("3 + 2 + 10"));
}

#[test]
fn eval_infix_operators_grouping() {
    assert_eq!(Ok(Expression::Integer(60)), eval_program("(3 + 3) * 10"));
}

#[test]
fn eval_infix_division() {
    assert_eq!(Ok(Expression::Integer(545)), eval_program("125895 / 231"));
}

#[test]
fn eval_closure_1() {
    assert_eq!(
        Ok(Expression::Integer(12)),
        eval_program("{ fn b() { let a = 12; fn () a }; b()() }")
    );
}

#[test]
fn eval_closure_2() {
    assert_eq!(
        Ok(Expression::Integer(12)),
        eval_program("{ let a = 1; fn b() { let a = 12; fn () a }; b()() }")
    );
}

#[test]
fn eval_closure_3() {
    assert_eq!(
        Ok(Expression::Integer(12)),
        eval_program("{ let a = 1; fn b() { let a = 12; fn () a }; b()() }")
    );
}

#[test]
fn eval_string_concatination() {
    assert_eq!(
        Ok(Expression::String("apabanan".to_string())),
        eval_program("{ let a = \"apa\"; let b = \"banan\"; a + b }")
    );
}

#[cfg(test)]
use typechecker::TypeChecker;
use typechecker::ResolvedType;

#[test]
fn type_check_simple_integer() {
    let mut parser = Parser::new(Lexer::new("10"));
    let expression = parser.program().unwrap();
    let mut type_checker = TypeChecker::new();
    let res = type_checker.resolve_type(&expression);
    assert_eq!(Ok(ResolvedType::Integer), res);
}

#[test]
fn type_check_simple_string() {
    let mut parser = Parser::new(Lexer::new("\"apa\""));
    let expression = parser.program().unwrap();
    let mut type_checker = TypeChecker::new();
    let res = type_checker.resolve_type(&expression);
    assert_eq!(Ok(ResolvedType::String), res);
}

#[test]
fn type_check_simple_bool() {
    let mut parser = Parser::new(Lexer::new("true"));
    let expression = parser.program().unwrap();
    let mut type_checker = TypeChecker::new();
    let res = type_checker.resolve_type(&expression);
    assert_eq!(Ok(ResolvedType::Bool), res);
}

#[test]
fn type_check_function() {
    let mut parser = Parser::new(Lexer::new("let a = fn() 10"));
    let expression = parser.program().unwrap();
    let mut type_checker = TypeChecker::new();
    let res = type_checker.resolve_type(&expression);
    assert_eq!(Ok(ResolvedType::Function), res);
}

#[test]
fn type_check_function_call() {
    let mut parser = Parser::new(Lexer::new("{ let a = fn() 10; a() }"));
    let expression = parser.program().unwrap();
    let mut type_checker = TypeChecker::new();
    let res = type_checker.resolve_type(&expression);
    assert_eq!(Ok(ResolvedType::Any), res);
}
