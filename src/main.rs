extern crate dirs;
extern crate rustyline;

#[macro_use]
extern crate log;
extern crate env_logger;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::rc::Rc;

mod expression;
mod interpreter;
mod lexer;
mod native;
mod parse_error;
mod parser;
mod resolvedtype;
mod symboltable;
mod token;
mod typecaster;
mod typechecker;
mod typedexpression;

use expression::Expression;
use expression::NativeFunctionExpr;
use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use symboltable::Closure;
use symboltable::SymbolTable;
use typechecker::TypeChecker;

use std::path::Path;

fn main() {
    env_logger::init();

    let mut rl = Editor::<()>::new();
    let homedir = dirs::home_dir().unwrap();
    let history_file_path = Path::new(&homedir).join(".maldives_history");
    if rl.load_history(&history_file_path).is_err() {
        println!("No previous history.");
    }
    let mut root = SymbolTable::new();
    root.bind(
        "println".to_string(),
        Closure::simple(Expression::NativeFunction(Rc::new(NativeFunctionExpr {
            function: native::native_println,
            eager: true,
        }))),
    );
    root.bind(
        "type".to_string(),
        Closure::simple(Expression::NativeFunction(Rc::new(NativeFunctionExpr {
            function: native::native_type,
            eager: false,
        }))),
    );
    root.bind(
        "typecast".to_string(),
        Closure::simple(Expression::NativeFunction(Rc::new(NativeFunctionExpr {
            function: native::native_typecast,
            eager: false,
        }))),
    );

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
                            match TypeChecker::new().resolve_type(&program) {
                                Ok(_) => match Interpreter::eval_expression(&program, &mut root) {
                                    Ok(a) => println!("{:?}", a),
                                    Err(e) => println!("{}", e),
                                },
                                Err(e) => println!("{}", e),
                            }
                        }
                        Err(e) => {
                            println!("Failed to parse: {:?}", e);
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
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

#[test]
fn eval_simple() {
    let mut parser = Parser::new(Lexer::new("apa"));
    let expression = parser.program().unwrap();
    let mut root = SymbolTable::new();
    root.bind("apa".to_string(), Closure::simple(Expression::Integer(4)));
    let res = Interpreter::eval_expression(&expression, &mut root);
    assert_eq!(Ok(Expression::Integer(4)), res);
}

#[cfg(test)]
fn eval_program(contents: &str) -> std::result::Result<Expression, interpreter::InterpreterError> {
    let mut parser = Parser::new(Lexer::new(contents));
    let expression = parser.program().unwrap();
    let mut root = SymbolTable::new();
    Interpreter::eval_expression(&expression, &mut root)
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
        eval_program("{ let apa = fn() -> int = 3; apa() }")
    );
}

#[test]
fn eval_anon_function_with_arg() {
    assert_eq!(
        Ok(Expression::Integer(3)),
        eval_program("{ let apa = fn(x: int) -> int = x; apa(3) }")
    );
}

#[test]
fn eval_anon_function_return_function() {
    assert_eq!(
        Ok(Expression::Integer(10)),
        eval_program("{ let f = fn() -> () -> int = fn () -> int = 10; f()() }")
    );
}

#[test]
fn eval_anon_function_as_arg() {
    assert_eq!(
        Ok(Expression::Integer(6)),
        eval_program(
            "{ let apply = fn (x: (int) -> int, arg: int) -> int = x(arg); let doubler = fn (i: int) -> int = i*2; apply(doubler, 3) }"
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
        eval_program("{ fn b() -> () -> int = { let a = 12; fn () -> int = a }; b()() }")
    );
}

#[test]
fn eval_closure_2() {
    assert_eq!(
        Ok(Expression::Integer(12)),
        eval_program(
            "{ let a = 1; fn b() -> () -> int = { let a = 12; fn () -> int = a }; b()() }"
        )
    );
}

#[test]
fn eval_closure_3() {
    assert_eq!(
        Ok(Expression::Integer(12)),
        eval_program(
            "{ let a = 1; fn b() -> () -> int = { let a = 12; fn () -> int = a }; b()() }"
        )
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
use resolvedtype::ResolvedFunctionType;
#[cfg(test)]
use resolvedtype::ResolvedType;

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
    let mut parser = Parser::new(Lexer::new("let a = fn() -> int = 10"));
    let expression = parser.program().unwrap();
    let mut type_checker = TypeChecker::new();
    let res = type_checker.resolve_type(&expression);
    let expected = ResolvedType::Function(Rc::new(ResolvedFunctionType {
        return_type: ResolvedType::Integer,
        parameters: Vec::new(),
    }));
    assert_eq!(Ok(expected), res);
}

#[test]
fn type_check_function_call() {
    let mut parser = Parser::new(Lexer::new("{ let a = fn() -> int = 10; a() }"));
    let expression = parser.program().unwrap();
    let mut type_checker = TypeChecker::new();
    let res = type_checker.resolve_type(&expression);
    assert_eq!(Ok(ResolvedType::Any), res);
}

#[test]
fn type_check_function_call_returning_function() {
    let mut parser = Parser::new(Lexer::new(
        "{ fn make_identity_fn() -> (int) -> int = fn(x: int) -> int = x }",
    ));
    let expression = parser.program().unwrap();
    let mut type_checker = TypeChecker::new();
    let res = type_checker.resolve_type(&expression);
    let expected = ResolvedType::Function(Rc::new(ResolvedFunctionType {
        return_type: ResolvedType::Function(Rc::new(ResolvedFunctionType {
            return_type: ResolvedType::Integer,
            parameters: vec![ResolvedType::Integer],
        })),
        parameters: vec![],
    }));
    assert_eq!(Ok(expected), res);
}
