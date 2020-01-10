use std::error;
use std::fmt;

use crate::expression::{BinaryOperation, Expression};
use crate::resolvedtype::ResolvedType;
use crate::symboltable::Closure;
use crate::symboltable::SymbolTable;
use crate::typedexpression::{TypedBinaryOperation, TypedExpression};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCasterError {
    pub message: String,
    pub env: SymbolTable<TypedExpression>,
}

impl fmt::Display for TypeCasterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TypeCaster error: {}", self.message)
        //write!(f, "Environment: {:?}", self.env)
    }
}

impl error::Error for TypeCasterError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl TypeCasterError {
    fn new(message: String, env: &SymbolTable<TypedExpression>) -> TypeCasterError {
        TypeCasterError {
            message,
            env: env.clone(),
        }
    }
}

pub struct TypeCaster;

impl TypeCaster {
    pub fn cast_expression(
        expression: &Expression,
        env: &mut SymbolTable<TypedExpression>,
    ) -> Result<TypedExpression, TypeCasterError> {
        let closure = TypeCaster::cast(expression, env)?;
        Ok(closure.expr)
    }

    fn cast(
        expression: &Expression,
        env: &mut SymbolTable<TypedExpression>,
    ) -> Result<Closure<TypedExpression>, TypeCasterError> {
        match expression {
            Expression::Void => Ok(Closure::simple(TypedExpression::void())),
            Expression::Integer(i) => Ok(Closure::simple(TypedExpression::integer(*i))),
            Expression::Bool(b) => Ok(Closure::simple(TypedExpression::bool(*b))),
            Expression::String(s) => Ok(Closure::simple(TypedExpression::string(s.to_string()))),
            Expression::Group(group) => {
                let typed_group = TypeCaster::cast(&group.expr, env)?;
                Ok(Closure::simple(TypedExpression::group(typed_group.expr)))
            }
            Expression::Symbol(symbol) => {
                let value = env.lookup(symbol).ok_or_else(|| {
                    TypeCasterError::new(format!("unknown symbol '{}'", symbol), env)
                })?;
                Ok(Closure::simple(value.expr.clone()))
            }
            Expression::Binary(b) => {
                let left = TypeCaster::cast(&b.left, env)?;
                let right = TypeCaster::cast(&b.right, env)?;
                match (
                    b.operation,
                    left.expr.resolved_type.clone(),
                    right.expr.resolved_type.clone(),
                ) {
                    (BinaryOperation::Sum, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(Closure::simple(TypedExpression::binary_operation(
                            TypedBinaryOperation::Sum,
                            ResolvedType::Integer,
                            left.expr,
                            right.expr,
                        )))
                    }
                    (BinaryOperation::Sum, ResolvedType::String, ResolvedType::String) => {
                        Ok(Closure::simple(TypedExpression::binary_operation(
                            TypedBinaryOperation::Concat,
                            ResolvedType::String,
                            left.expr,
                            right.expr,
                        )))
                    }
                    (BinaryOperation::Difference, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(Closure::simple(TypedExpression::binary_operation(
                            TypedBinaryOperation::Difference,
                            ResolvedType::Integer,
                            left.expr,
                            right.expr,
                        )))
                    }
                    (BinaryOperation::Multiply, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(Closure::simple(TypedExpression::binary_operation(
                            TypedBinaryOperation::Multiply,
                            ResolvedType::Integer,
                            left.expr,
                            right.expr,
                        )))
                    }
                    (BinaryOperation::Divide, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(Closure::simple(TypedExpression::binary_operation(
                            TypedBinaryOperation::Divide,
                            ResolvedType::Integer,
                            left.expr,
                            right.expr,
                        )))
                    }

                    (BinaryOperation::LessThan, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(Closure::simple(TypedExpression::binary_operation(
                            TypedBinaryOperation::LessThan,
                            ResolvedType::Bool,
                            left.expr,
                            right.expr,
                        )))
                    }
                    (
                        BinaryOperation::LessEqualThan,
                        ResolvedType::Integer,
                        ResolvedType::Integer,
                    ) => Ok(Closure::simple(TypedExpression::binary_operation(
                        TypedBinaryOperation::LessEqualThan,
                        ResolvedType::Bool,
                        left.expr,
                        right.expr,
                    ))),
                    (
                        BinaryOperation::GreaterThan,
                        ResolvedType::Integer,
                        ResolvedType::Integer,
                    ) => Ok(Closure::simple(TypedExpression::binary_operation(
                        TypedBinaryOperation::GreaterThan,
                        ResolvedType::Bool,
                        left.expr,
                        right.expr,
                    ))),
                    (
                        BinaryOperation::GreaterEqualThan,
                        ResolvedType::Integer,
                        ResolvedType::Integer,
                    ) => Ok(Closure::simple(TypedExpression::binary_operation(
                        TypedBinaryOperation::GreaterEqualThan,
                        ResolvedType::Bool,
                        left.expr,
                        right.expr,
                    ))),

                    _ => Err(TypeCasterError::new("stuff".to_string(), env)),
                }
            }
            Expression::Conditional(c) => {
                let condition = TypeCaster::cast(&c.condition, env)?.expr;
                let true_branch = TypeCaster::cast(&c.true_branch, env)?.expr;
                let false_branch = match c.false_branch.clone() {
                    Some(false_branch) => Some(TypeCaster::cast(&false_branch, env)?.expr),
                    None => None,
                };
                let true_branch_resolved_type = true_branch.resolved_type.clone();
                let false_branch_resolved_type = false_branch
                    .clone()
                    .and_then(|expr| Some(expr.resolved_type))
                    .unwrap_or(true_branch.resolved_type.clone());
                if condition.resolved_type == ResolvedType::Bool
                    && true_branch_resolved_type == false_branch_resolved_type
                {
                    Ok(Closure::simple(TypedExpression::conditional(
                        condition,
                        true_branch_resolved_type,
                        true_branch.clone(),
                        false_branch.clone(),
                    )))
                } else {
                    Err(TypeCasterError::new("stuff".to_string(), env))
                }
            }
            Expression::Block(b) => {
                let mut list = Vec::new();
                for expr in &b.list {
                    list.push(TypeCaster::cast(&expr, env)?.expr);
                }
                Ok(Closure::simple(TypedExpression::block(list)))
            }
            Expression::Bind(bind) => {
                let closure = TypeCaster::cast(&bind.expr, env)?;
                let resolved_sym_type: Option<ResolvedType> = match bind.sym_type.clone() {
                    Some(decl) => ResolvedType::from_decl(&decl),
                    None => Some(closure.expr.resolved_type.clone()),
                };
                if resolved_sym_type == Some(closure.expr.resolved_type.clone()) {
                    Ok(Closure::simple(TypedExpression::bind(
                        String::from(&bind.sym),
                        closure.expr,
                    )))
                } else {
                    Err(TypeCasterError::new(
                        format!(
                            "Type mismatch: Can't bind symbol '{}' of type '{:?}' to expression of type {:?}",
                            &bind.sym, resolved_sym_type, closure.expr.resolved_type.clone()
                        ),
                        env,
                    ))
                }
            }
            _ => Err(TypeCasterError::new("Unimplemented".to_string(), env)),
            //            Expression::Function(f) => {
            //                let closure = Closure {
            //                    expr: expression.clone(),
            //                    env: Some(env.clone()),
            //                };
            //                if let Some(sym) = &f.sym {
            //                    env.bind(String::from(sym), closure.clone());
            //                }
            //                Ok(closure.clone())
            //            }
            //            Expression::NativeFunction(n) => {
            //                Ok(Closure::simple(Expression::NativeFunction(n.clone())))
            //            }
            //            Expression::FunctionCall(fc) => {
            //                let value = TypeCaster::cast(&fc.expr, env)?;
            //                match (value.expr, value.env) {
            //                    (Expression::Function(f), Some(function_env)) => {
            //                        debug!("Parameters: {:?}", f.parameters);
            //                        let mut call_env = function_env.clone();
            //                        for (idx, parameter) in f.parameters.iter().enumerate() {
            //                            let argument = fc.arguments.get(idx).unwrap_or(&Expression::Void);
            //                            let val = TypeCaster::cast(argument, env)?;
            //                            debug!(
            //                                "Mapping: {:?} -> {:?} in calling environment",
            //                                parameter, val
            //                            );
            //                            call_env.bind(
            //                                String::from(&parameter.0),
            //                                Closure::complete(val.expr.clone(), val.env.clone()),
            //                            );
            //                        }
            //                        debug!("Calling function with env: {:?}", &call_env);
            //                        let result = TypeCaster::cast(&f.expr, &mut call_env)?;
            //                        Ok(result)
            //                    }
            //                    (Expression::NativeFunction(f), _) => {
            //                        let native_function = f.function;
            //                        match fc.arguments.as_slice() {
            //                            [only_one] => {
            //                                let arg = TypeCaster::cast(&only_one, env)?;
            //                                let res = native_function(&arg.expr)
            //                                    .map_err(|message| TypeCasterError::new(message, env))?;
            //                                Ok(Closure::simple(res))
            //                            }
            //                            _ => Err(TypeCasterError::new(
            //                                format!("unexpected number of argumens to native function"),
            //                                env,
            //                            )),
            //                        }
            //                    }
            //                    _ => Err(TypeCasterError::new(
            //                        format!("unexpected {:?}, expected function", fc.expr),
            //                        env,
            //                    )),
            //                }
            //            }
        }
    }
}
