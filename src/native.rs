use crate::typedexpression::{TypedExpression, TypedExpressionNode};

pub fn native_println(e: &TypedExpression) -> Result<TypedExpression, String> {
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

pub fn native_dbg(e: &TypedExpression) -> Result<TypedExpression, String> {
    println!("{:?}", e);
    Ok(TypedExpression::void())
}
