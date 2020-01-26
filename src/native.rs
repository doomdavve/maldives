use std::convert::TryInto;

use crate::interpreter::Interpreter;
use crate::resolvedtype::ResolvedType;
use crate::symboltable::SymbolTable;
use crate::typedexpression::TypedExpression;
use crate::typedexpressionnode::TypedExpressionNode;

pub fn native_println(
    _env: &mut SymbolTable,
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
    _env: &mut SymbolTable,
    arguments: &Vec<TypedExpression>,
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    for argument in arguments {
        println!("{:?}", argument);
    }
    Ok(TypedExpression::void())
}

pub fn native_array(
    _env: &mut SymbolTable,
    arguments: &Vec<TypedExpression>,
    type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match type_arguments {
        Some(types) if types.len() == 1 => match types[0] {
            _ => Ok(TypedExpression::array(
                ResolvedType::Integer,
                arguments.clone(),
            )),
        },
        _ => Err("Missing or wrong number of type arguments to array constructor".to_string()),
    }
}

pub fn native_array_len(
    _env: &mut SymbolTable,
    arguments: &Vec<TypedExpression>,
    _: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match &arguments[..] {
        [first_arg] => match &first_arg.node {
            TypedExpressionNode::Array(array) => Ok(TypedExpression::integer(
                array.array.len().try_into().unwrap(),
            )),
            _ => Err("Missing or wrong number of arguments".to_string()),
        },
        _ => Err("Missing or wrong number of arguments".to_string()),
    }
}

pub fn native_array_map(
    env: &mut SymbolTable,
    arguments: &Vec<TypedExpression>,
    _: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match &arguments[..] {
        [first_arg, second_arg] => match &first_arg.node {
            TypedExpressionNode::Array(array) => {
                let mut new_vec: Vec<TypedExpression> = Vec::new();
                for expr in &array.array {
                    let n = Interpreter::call(second_arg, &vec![expr.clone()], env)
                        .map_err(|e| e.message)?;
                    new_vec.push(n);
                }
                Ok(TypedExpression::array(
                    second_arg.resolved_type.clone(),
                    new_vec,
                ))
            }
            _ => Err("expected array".to_string()),
        },
        _ => Err("Missing or wrong number of arguments".to_string()),
    }
}

pub fn native_env(
    env: &mut SymbolTable,
    _: &Vec<TypedExpression>,
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    print!("{}", env);
    Ok(TypedExpression::void())
}
