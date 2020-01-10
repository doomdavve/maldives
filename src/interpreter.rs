use std::error;
use std::fmt;

use crate::typedexpression::TypedBinaryOperation;
use crate::typedexpression::TypedExpression;
use crate::typedexpression::TypedExpressionNode;
use crate::symboltable::{SymbolTable, Closure};

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError {
    message: String,
    env: SymbolTable,
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Interpreter error: {}", self.message)?;
        write!(f, "Environment: {:?}", self.env)
    }
}

impl error::Error for InterpreterError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl InterpreterError {
    fn new(message: String, env: &SymbolTable) -> InterpreterError {
        InterpreterError {
            message,
            env: env.clone(),
        }
    }
}

pub struct Interpreter;

impl Interpreter {
    pub fn eval_expression(
        expr: &TypedExpression,
        env: &mut SymbolTable,
    ) -> Result<TypedExpression, InterpreterError> {
        let closure = Interpreter::eval(expr, env)?;
        Ok(closure.expr)
    }

    fn eval(
        expr: &TypedExpression,
        env: &mut SymbolTable,
    ) -> Result<Closure, InterpreterError> {
        debug!("Evaulating {:?} with vars: {:?}", expr, env);

        match &expr.node {
            TypedExpressionNode::Void => Ok(Closure::simple(TypedExpression::void())),
            TypedExpressionNode::BinaryOperation(b) => {
                let l = Interpreter::eval(&b.left, env)?;
                let r = Interpreter::eval(&b.right, env)?;
                match b.operation {
                    TypedBinaryOperation::Sum => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::integer(li + ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("Unexpected terms in sum operator"),
                            env,
                        )),
                    },
                    TypedBinaryOperation::Difference => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::integer(li - ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-integer terms to difference operator"),
                            env,
                        )),
                    },
                    TypedBinaryOperation::Multiply => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::integer(li * ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-integer terms to multiply operator"),
                            env,
                        )),
                    },
                    TypedBinaryOperation::Divide => {
                        // TODO: handle division by zero.
                        match (l.expr.node, r.expr.node) {
                            (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                                Ok(Closure::simple(TypedExpression::integer(li / ri)))
                            }
                            _ => Err(InterpreterError::new(
                                format!("One or more non-integer terms to divide operator"),
                                env,
                            )),
                        }
                    }
                    TypedBinaryOperation::LessThan => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li < ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-boolean terms to divide operator"),
                            env,
                        )),
                    },
                    TypedBinaryOperation::GreaterThan => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li > ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-boolean terms to divide operator"),
                            env,
                        )),
                    },
                    TypedBinaryOperation::LessEqualThan => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li <= ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-boolean terms to divide operator"),
                            env,
                        )),
                    },
                    TypedBinaryOperation::GreaterEqualThan => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li >= ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-boolean terms to divide operator"),
                            env,
                        )),
                    },
                    TypedBinaryOperation::Concat => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::String(li), TypedExpressionNode::String(ri)) => {
                            Ok(Closure::simple(TypedExpression::string(li + &ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-string terms to concatenation operator"),
                            env,
                        )),
                    }
                }
            }
            TypedExpressionNode::Conditional(c) => {
                let premise = Interpreter::eval(&c.condition, env)?;
                match (premise.expr.node, c.false_branch.as_ref()) {
                    (TypedExpressionNode::Bool(true), _) => Ok(Interpreter::eval(&c.true_branch, env)?),
                    (TypedExpressionNode::Bool(false), Some(false_branch)) => {
                        Ok(Interpreter::eval(&false_branch, env)?)
                    }
                    (TypedExpressionNode::Bool(false), _) => Ok(Closure::simple(TypedExpression::void())),
                    _ => Err(InterpreterError::new(
                        format!("Unexpected result of conditional"),
                        env,
                    )),
                }
            }
            TypedExpressionNode::Group(g) => Interpreter::eval(&g.expr, env),
            TypedExpressionNode::Bool(b) => Ok(Closure::simple(TypedExpression::bool(*b))),
            TypedExpressionNode::Integer(i) => Ok(Closure::simple(TypedExpression::integer(*i))),
            TypedExpressionNode::String(s) => Ok(Closure::simple(TypedExpression::string(s.to_string()))),
            TypedExpressionNode::Symbol(s) => {
                let value = env
                    .lookup(s)
                    .ok_or_else(|| InterpreterError::new(format!("unknown symbol '{}'", s), env))?;
                Ok(Closure::complete(value.expr.clone(), value.env.clone()))
            }
            TypedExpressionNode::Bind(b) => {
                let val = Interpreter::eval(&b.expr, env)?;
                env.bind(
                    String::from(&b.sym),
                    Closure {
                        expr: val.expr.clone(),
                        env: val.env.clone(),
                    },
                );
                Ok(val)
            }
            TypedExpressionNode::Block(b) => {
                let mut last = Ok(Closure::simple(TypedExpression::void()));
                let mut scope = env.clone();
                for expr in &b.list {
                    last = Interpreter::eval(&expr, &mut scope);
                }
                last
            }
            TypedExpressionNode::Function(f) => {
                let closure = Closure {
                    expr: expr.clone(),
                    env: Some(env.clone()),
                };
                if let Some(sym) = &f.sym {
                    env.bind(String::from(sym), closure.clone());
                }
                Ok(closure.clone())
            }
            TypedExpressionNode::NativeFunction(_) => {
                Ok(Closure::simple(expr.clone()))
            }
            TypedExpressionNode::FunctionCall(fc) => {
                let value = Interpreter::eval(&fc.expr, env)?;
                match (value.expr.node, value.env) {
                    (TypedExpressionNode::Function(f), Some(function_env)) => {
                        debug!("Parameters: {:?}", f.parameters);
                        let mut call_env = function_env.clone();
                        for (idx, parameter) in f.parameters.iter().enumerate() {
                            let void = TypedExpression::void();
                            let argument = fc.arguments.get(idx).unwrap_or(&void);
                            let val = Interpreter::eval(argument, env)?;
                            debug!(
                                "Mapping: {:?} -> {:?} in calling environment",
                                parameter, val
                            );
                            call_env.bind(
                                String::from(&parameter.0),
                                Closure::complete(val.expr.clone(), val.env.clone()),
                            );
                        }
                        debug!("Calling function with env: {:?}", &call_env);
                        let result = Interpreter::eval(&f.expr, &mut call_env)?;
                        Ok(result)
                    }
                    (TypedExpressionNode::NativeFunction(f), _) => {
                        let native_function = f.function;
                        match fc.arguments.as_slice() {
                            [only_one] => {
                                let arg = if f.call_by_value {
                                    Interpreter::eval(&only_one, env)?.expr
                                } else {
                                    only_one.clone()
                                };
                                let res = native_function(&arg)
                                    .map_err(|message| InterpreterError::new(message, env))?;
                                Ok(Closure::simple(res))
                            }
                            _ => Err(InterpreterError::new(
                                format!("unexpected number of arguments to native function"),
                                env,
                            )),
                        }
                    }
                    _ => Err(InterpreterError::new(
                        format!("unexpected {:?}, expected function", fc.expr),
                        env,
                    )),
                }
            }
        }
    }
}
