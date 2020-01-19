use crate::resolvedtype::ResolvedType;
use crate::symboltable::SymbolTable;
use crate::typedexpression::{TypedExpression, TypedExpressionNode};

pub fn native_println(_env: &SymbolTable, e: &TypedExpression) -> Result<TypedExpression, String> {
    match &e.node {
        TypedExpressionNode::String(s) => {
            println!("{}", s);
            Ok(TypedExpression::void())
        }
        TypedExpressionNode::Integer(i) => {
            println!("{}", i);
            Ok(TypedExpression::void())
        }
        _ => Err("Unexpected argument to println".to_string()),
    }
}

pub fn native_dbg(_env: &SymbolTable, e: &TypedExpression) -> Result<TypedExpression, String> {
    println!("{:?}", e);
    Ok(TypedExpression::void())
}

pub fn native_integer_array(
    _env: &SymbolTable,
    e: &TypedExpression,
) -> Result<TypedExpression, String> {
    match &e.node {
        TypedExpressionNode::Integer(size) if *size > 0 => {
            let v: Vec<TypedExpression> = Vec::with_capacity(*size as usize);
            Ok(TypedExpression::array(ResolvedType::Integer, v))
        }
        _ => Err("pass initial size of array as first argument".to_string()),
    }
}
