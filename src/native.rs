use crate::expression::Expression;
use crate::typechecker::TypeChecker;

pub fn native_println(e: &Expression) -> Result<Expression, String> {
    match e {
        Expression::String(s) => {
            println!("{}", s);
            Ok(Expression::Void)
        }
        _ => Err("Unexpected argument to println".to_string()),
    }
}

pub fn native_type(e: &Expression) -> Result<Expression, String> {
    let mut type_checker = TypeChecker::new();
    Ok(Expression::String(format!(
        "{:?}",
        type_checker
            .resolve_type(e)
            .map_err(|_e| String::from("Type checker failed"))?
    )))
}
