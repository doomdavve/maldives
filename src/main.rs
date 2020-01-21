extern crate dirs;
extern crate rustyline;

#[macro_use]
extern crate log;
extern crate env_logger;

use std::env;
use std::fs;
use std::path::Path;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod expression;
mod interpreter;
mod lexer;
mod native;
mod parser;
mod resolvedtype;
mod string;
mod symboltable;
mod token;
mod typedexpression;
mod typeresolver;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use resolvedtype::ResolvedType;
use symboltable::SymbolTable;
use typedexpression::{TypedExpression, TypedExpressionNode};
use typeresolver::TypeResolver;

fn main() -> Result<(), String> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    match if args.len() > 1 { Some(&args[1]) } else { None } {
        Some(filename) => load_file(filename),
        None => repl(),
    }?;

    Ok(())
}

fn load_file(filename: &str) -> Result<i32, String> {
    let mut root = root_symboltable();
    match fs::read_to_string(filename) {
        Ok(contents) => {
            let tokens = Lexer::new(&contents);
            match Parser::new(tokens).program() {
                Ok(program) => {
                    debug!("Parsed program: {:?}", program);
                    match TypeResolver::resolve_in_env(&program, &mut root) {
                        Ok(resolved) => match Interpreter::eval(&resolved, &mut root) {
                            Ok(a) => match a.node {
                                TypedExpressionNode::Integer(i) => Ok(i),
                                _ => Ok(0),
                            },
                            Err(e) => Err(e.to_string()),
                        },
                        Err(e) => Err(e.to_string()),
                    }
                }
                Err(e) => Err(e.to_string()),
            }
        }
        Err(error) => Err(error.to_string()),
    }
}

fn root_symboltable() -> SymbolTable {
    let mut root = SymbolTable::new();
    root.bind(
        "println".to_string(),
        TypedExpression::native_function(
            native::native_println,
            ResolvedType::None,
            vec![ResolvedType::Any],
            true,
        ),
    );
    root.bind(
        "dbg".to_string(),
        TypedExpression::native_function(
            native::native_dbg,
            ResolvedType::None,
            vec![ResolvedType::Any],
            false,
        ),
    );
    root.bind(
        "array".to_string(),
        TypedExpression::native_function(
            native::native_array,
            ResolvedType::Array(Box::new(ResolvedType::TypeParameterId(0))),
            vec![ResolvedType::VarArgs],
            false,
        ),
    );
    root.bind(
        "env".to_string(),
        TypedExpression::native_function(native::native_env, ResolvedType::None, vec![], false),
    );
    root
}

fn repl() -> Result<i32, String> {
    let mut rl = Editor::<()>::new();
    let homedir = dirs::home_dir().unwrap();
    let history_file_path = Path::new(&homedir).join(".maldives_history");
    if rl.load_history(&history_file_path).is_err() {
        println!("No previous history.");
    }
    let mut root = root_symboltable();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if &line != "" {
                    rl.add_history_entry(line.as_str());
                    let tokens = Lexer::new(line.as_str());
                    match Parser::new(tokens).program() {
                        Ok(program) => {
                            debug!("Parsed program: {:?}", program);
                            match TypeResolver::resolve_in_env(&program, &mut root) {
                                Ok(resolved) => match Interpreter::eval(&resolved, &mut root) {
                                    Ok(a) => match a.resolved_type {
                                        ResolvedType::None => {}
                                        _ => println!("{}", a),
                                    },
                                    Err(e) => println!("{}", e),
                                },
                                Err(e) => println!("{}", e),
                            }
                        }
                        Err(e) => {
                            println!("{}", e.message);
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
    Ok(0)
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::interpreter::Interpreter;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::resolvedtype::ResolvedFunctionType;
    use crate::resolvedtype::ResolvedType;
    use crate::symboltable::SymbolTable;
    use crate::typedexpression::TypedExpression;
    use crate::typeresolver::Error;
    use crate::typeresolver::TypeResolver;

    fn type_check_program(
        contents: &str,
        mut root: &mut SymbolTable,
    ) -> Result<TypedExpression, Error> {
        let mut parser = Parser::new(Lexer::new(contents));
        TypeResolver::resolve_in_env(&parser.program().unwrap(), &mut root)
    }

    fn eval_program(contents: &str) -> Result<TypedExpression, String> {
        let mut root = SymbolTable::new();
        let expression = type_check_program(contents, &mut root).map_err(|e| e.message)?;
        Interpreter::eval(&expression, &mut root).map_err(|e| e.message)
    }

    #[test]
    fn eval_simple() {
        let mut parser = Parser::new(Lexer::new("apa"));
        let mut root = SymbolTable::new();
        root.bind("apa".to_string(), TypedExpression::integer(4));
        let expression =
            TypeResolver::resolve_in_env(&parser.program().unwrap(), &mut root).unwrap();
        let res = Interpreter::eval(&expression, &mut root);
        assert_eq!(Ok(TypedExpression::integer(4)), res);
    }

    #[test]
    fn eval_simple_assignment() {
        assert_eq!(
            Ok(TypedExpression::integer(3)),
            eval_program("{ let apa = 3; apa }")
        );
    }

    #[test]
    fn eval_assignment() {
        assert_eq!(Ok(TypedExpression::integer(3)), eval_program("let apa = 3"));
    }

    #[test]
    fn eval_anon_function() {
        assert_eq!(
            Ok(TypedExpression::integer(3)),
            eval_program("{ let apa = fn() -> int = 3; apa() }")
        );
    }

    #[test]
    fn eval_anon_function_with_arg() {
        assert_eq!(
            Ok(TypedExpression::integer(3)),
            eval_program("{ let apa = fn(x: int) -> int = x; apa(3) }")
        );
    }

    #[test]
    fn eval_anon_function_return_function() {
        assert_eq!(
            Ok(TypedExpression::integer(10)),
            eval_program("{ let f = fn() -> () -> int = fn () -> int = 10; f()() }")
        );
    }

    #[test]
    fn eval_anon_function_as_arg() {
        assert_eq!(
            Ok(TypedExpression::integer(6)),
            eval_program(
                "{ let apply = fn (x: (int) -> int, arg: int) -> int = x(arg); let doubler = fn (i: int) -> int = i*2; apply(doubler, 3) }"
            )
        );
    }

    #[test]
    fn eval_infix() {
        assert_eq!(Ok(TypedExpression::integer(5)), eval_program("3 + 2"));
    }

    #[test]
    fn eval_infix_in_succession() {
        assert_eq!(Ok(TypedExpression::integer(15)), eval_program("3 + 2 + 10"));
    }

    #[test]
    fn eval_infix_operators_grouping() {
        assert_eq!(
            Ok(TypedExpression::integer(60)),
            eval_program("(3 + 3) * 10")
        );
    }

    #[test]
    fn eval_infix_division() {
        assert_eq!(
            Ok(TypedExpression::integer(545)),
            eval_program("125895 / 231")
        );
    }

    #[test]
    fn eval_closure_1() {
        assert_eq!(
            Ok(TypedExpression::integer(12)),
            eval_program("{ fn b() -> () -> int = { let a = 12; fn () -> int = a }; b()() }")
        );
    }

    #[test]
    fn eval_closure_2() {
        assert_eq!(
            Ok(TypedExpression::integer(12)),
            eval_program(
                "{ let a = 1; fn b() -> () -> int = { let a = 12; fn () -> int = a }; b()() }"
            )
        );
    }

    #[test]
    fn eval_closure_3() {
        assert_eq!(
            Ok(TypedExpression::integer(12)),
            eval_program(
                "{ let a = 1; fn b() -> () -> int = { let a = 12; fn () -> int = a }; b()() }"
            )
        );
    }

    #[test]
    fn eval_string_concatenation() {
        assert_eq!(
            Ok(TypedExpression::string("apabanan".to_string())),
            eval_program("{ let a = \"apa\"; let b = \"banan\"; a + b }")
        );
    }

    #[test]
    fn test_check_operator_precedence() {
        assert_eq!(
            Ok(TypedExpression::bool(true)),
            eval_program("77 == 1 + 8**2 + 2 * (3 + 3)")
        );
    }

    #[test]
    fn test_check_loop_and_break() {
        assert_eq!(
            Ok(TypedExpression::integer(46368)),
            eval_program(
                "let a = 1;
                 let b = 1;
                 loop {
                     let c = a;
                     a = a + b;
                     b = c;
                     if a > 30000 {
                         break a
                     }
                 }"
            )
        );
    }

    #[test]
    fn test_assignment() {
        assert_eq!(
            Ok(TypedExpression::integer(2)),
            eval_program("{ let a = 1; a = 2; a }")
        );
    }

    #[test]
    fn test_assignment_error_type_mismatch() {
        assert_eq!(
            Err("type mismatch in assignment".to_string()),
            eval_program("{ let a = 1; a = \"some string\"; a }")
        );
    }

    #[test]
    fn test_assignment_error_unbound() {
        assert_eq!(
            Err("Unable to resolve type of symbol 'a'".to_string()),
            eval_program("{ a = \"some string\"; a }")
        );
    }

    #[test]
    fn test_scope() {
        assert_eq!(
            Ok(TypedExpression::integer(10)),
            eval_program("let a = 10; { let a = 20; }; a")
        );
    }

    #[test]
    fn test_scope_loop() {
        assert_eq!(
            Ok(TypedExpression::integer(10)),
            eval_program("let a = 10; loop { let a = 20; break }; a")
        );
    }

    #[test]
    fn type_check_simple_integer() {
        let res = type_check_program("10", &mut SymbolTable::new()).unwrap();
        assert_eq!(ResolvedType::Integer, res.resolved_type);
    }

    #[test]
    fn type_check_simple_string() {
        let res = type_check_program("\"apa\"", &mut SymbolTable::new()).unwrap();
        assert_eq!(ResolvedType::String, res.resolved_type);
    }

    #[test]
    fn type_check_simple_bool() {
        let res = type_check_program("true", &mut SymbolTable::new()).unwrap();
        assert_eq!(ResolvedType::Bool, res.resolved_type);
    }

    #[test]
    fn type_check_function() {
        let res = type_check_program("let a = fn() -> int = 10", &mut SymbolTable::new()).unwrap();
        let expected = ResolvedType::Function(Rc::new(ResolvedFunctionType {
            return_type: ResolvedType::Integer,
            parameters: Vec::new(),
        }));
        assert_eq!(expected, res.resolved_type);
    }

    #[test]
    fn type_check_function_call() {
        let res = type_check_program("{ let a = fn() -> int = 10; a() }", &mut SymbolTable::new())
            .unwrap();
        assert_eq!(ResolvedType::Integer, res.resolved_type);
    }

    #[test]
    fn type_check_function_call_returning_function() {
        let res = type_check_program(
            "{ fn make_identity_fn() -> (int) -> int = fn(x: int) -> int = x }",
            &mut SymbolTable::new(),
        )
        .unwrap();
        let expected = ResolvedType::Function(Rc::new(ResolvedFunctionType {
            return_type: ResolvedType::Function(Rc::new(ResolvedFunctionType {
                return_type: ResolvedType::Integer,
                parameters: vec![ResolvedType::Integer],
            })),
            parameters: vec![],
        }));
        assert_eq!(expected, res.resolved_type);
    }

    #[test]
    fn test_check_function_call_args() {
        let res = type_check_program("{ fn f(i:int)=i; f(true) }", &mut SymbolTable::new());
        assert!(res.is_err());
    }
}
