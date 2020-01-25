use std::convert::TryInto;

use crate::resolvedtype::ResolvedType;
use crate::symboltable::SymbolTable;
use crate::typedexpression::TypedExpression;
use crate::typedexpressionnode::TypedExpressionNode;

pub fn native_println(
    _env: &SymbolTable,
    arguments: &Vec<TypedExpression>,
    _type_arguments: &Option<Vec<ResolvedType>>,
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
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    for argument in arguments {
        println!("{:?}", argument);
    }
    Ok(TypedExpression::void())
}

pub fn native_array(
    _env: &SymbolTable,
    arguments: &Vec<TypedExpression>,
    type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match type_arguments {
        Some(types) if types.len() == 1 => {
            let v = match types[0] {
                ResolvedType::Integer => arguments
                    .iter()
                    .flat_map(|expr| match expr.node {
                        TypedExpressionNode::Integer(i) => Some(i),
                        _ => None,
                    })
                    .collect(),
                _ => unimplemented!(),
            };
            Ok(TypedExpression::array_i32(ResolvedType::Integer, v))
        }
        _ => Err("Missing or wrong number of type arguments to array constructor".to_string()),
    }
}

pub fn native_array_len(
    _env: &SymbolTable,
    arguments: &Vec<TypedExpression>,
    _: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match &arguments[..] {
        [first_arg] => match &first_arg.node {
            TypedExpressionNode::IntArray(array) => Ok(TypedExpression::integer(
                array.array.len().try_into().unwrap(),
            )),
            _ => Err("Missing or wrong number of arguments".to_string()),
        },
        _ => Err("Missing or wrong number of arguments".to_string()),
    }
}

pub fn native_env(
    env: &SymbolTable,
    _: &Vec<TypedExpression>,
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    print!("{}", env);
    Ok(TypedExpression::void())
}
