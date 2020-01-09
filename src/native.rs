use crate::expression::Expression;
use crate::symboltable::SymbolTable;
use crate::typecaster::TypeCaster;
use crate::typechecker::TypeChecker;
use crate::typedexpression::TypedExpression;

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

pub fn native_typecast(e: &Expression) -> Result<Expression, String> {
    let mut env = SymbolTable::<TypedExpression>::new();
    println!("cast: {:?}", e);
    Ok(Expression::String(format!(
        "{:?}",
        TypeCaster::cast_expression(e, &mut env).map_err(|err| err.message)?
    )))
}
