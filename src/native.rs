use crate::resolvedtype::ResolvedType;
use crate::symboltable::SymbolTable;
use crate::typedexpression::{TypedExpression, TypedExpressionNode};

pub fn native_println(
    _env: &SymbolTable,
    arguments: &Vec<TypedExpression>,
) -> Result<TypedExpression, String> {
    for argument in arguments {
        match &argument.node {
            TypedExpressionNode::String(s) => {
                println!("{}", s);
            }
            TypedExpressionNode::Integer(i) => {
                println!("{}", i);
            }
            _ => break,
        }
    }
    Ok(TypedExpression::void())
}

pub fn native_dbg(
    _env: &SymbolTable,
    arguments: &Vec<TypedExpression>,
) -> Result<TypedExpression, String> {
    for argument in arguments {
        println!("{:?}", argument);
    }
    Ok(TypedExpression::void())
}

pub fn native_integer_array(
    _env: &SymbolTable,
    arguments: &Vec<TypedExpression>,
) -> Result<TypedExpression, String> {
    let mut v: Vec<TypedExpression> = Vec::with_capacity(arguments.len());
    for argument in arguments {
        match &argument.node {
            TypedExpressionNode::Integer(i) => v.push(TypedExpression::integer(*i)),
            _ => unreachable!(),
        }
    }
    Ok(TypedExpression::array(ResolvedType::Integer, v))
}

pub fn native_env(env: &SymbolTable, _: &Vec<TypedExpression>) -> Result<TypedExpression, String> {
    print!("{}", env);
    Ok(TypedExpression::void())
}
