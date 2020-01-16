use std::convert::TryInto;
use std::error;
use std::fmt;

use crate::symboltable::{Closure, SymbolTable};
use crate::typedexpression::TypedBinaryOperation;
use crate::typedexpression::TypedExpression;
use crate::typedexpression::TypedExpressionNode;

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError {
    pub message: String,
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Interpreter error: {}", self.message)
    }
}

impl error::Error for InterpreterError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl InterpreterError {
    fn new(message: String) -> InterpreterError {
        InterpreterError { message }
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
        mut env: &mut SymbolTable,
    ) -> Result<Closure, InterpreterError> {
        debug!("Evaulating {:?} with vars: {:?}", expr, env);
        match &expr.node {
            TypedExpressionNode::Void => Ok(Closure::simple(TypedExpression::void())),
            TypedExpressionNode::BinaryOperation(b) => {
                let l = if b.operation.left_hand_by_name() {
                    Closure::simple(b.left.clone())
                } else {
                    Interpreter::eval(&b.left, env)?
                };
                let r = Interpreter::eval(&b.right, env)?;
                match b.operation {
                    TypedBinaryOperation::Sum => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::integer(li + ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "Unexpected terms in sum operator"
                        ))),
                    },
                    TypedBinaryOperation::Difference => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::integer(li - ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-integer terms to difference operator"
                        ))),
                    },
                    TypedBinaryOperation::Multiply => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::integer(li * ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-integer terms to multiply operator"
                        ))),
                    },
                    TypedBinaryOperation::ToThePowerOf => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::integer(
                                li.pow(ri.try_into().unwrap()),
                            )))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "Unexpected terms in power operator"
                        ))),
                    },

                    TypedBinaryOperation::Divide => {
                        // TODO: handle division by zero.
                        match (l.expr.node, r.expr.node) {
                            (
                                TypedExpressionNode::Integer(li),
                                TypedExpressionNode::Integer(ri),
                            ) => Ok(Closure::simple(TypedExpression::integer(li / ri))),
                            _ => Err(InterpreterError::new(format!(
                                "One or more non-integer terms to divide operator"
                            ))),
                        }
                    }
                    TypedBinaryOperation::LessThan => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li < ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-boolean terms to divide operator"
                        ))),
                    },
                    TypedBinaryOperation::GreaterThan => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li > ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-boolean terms to divide operator"
                        ))),
                    },
                    TypedBinaryOperation::LessEqualThan => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li <= ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-boolean terms to divide operator"
                        ))),
                    },
                    TypedBinaryOperation::GreaterEqualThan => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li >= ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-boolean terms to divide operator"
                        ))),
                    },
                    TypedBinaryOperation::Concat => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::String(li), TypedExpressionNode::String(ri)) => {
                            Ok(Closure::simple(TypedExpression::string(li + &ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-string terms to concatenation operator"
                        ))),
                    },
                    TypedBinaryOperation::Equal => match (l.expr.node, r.expr.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(Closure::simple(TypedExpression::bool(li == ri)))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-integer terms to equal operator"
                        ))),
                    },
                    TypedBinaryOperation::Assign => match l.expr.node {
                        TypedExpressionNode::Symbol(sym) => {
                            let val = Interpreter::eval(&r.expr, env)?;
                            env.update(
                                String::from(&sym),
                                Closure {
                                    expr: val.expr.clone(),
                                    env: val.env.clone(),
                                },
                            )
                            .ok_or(InterpreterError::new(format!(
                                "{} not found in this scope",
                                sym
                            )))?;
                            Ok(val)
                        }
                        _ => Err(InterpreterError::new(format!(
                            "unexpected left hand side of assignment"
                        ))),
                    },
                }
            }
            TypedExpressionNode::Conditional(c) => {
                let premise = Interpreter::eval(&c.condition, env)?;
                match (premise.expr.node, c.false_branch.as_ref()) {
                    (TypedExpressionNode::Bool(true), _) => {
                        Ok(Interpreter::eval(&c.true_branch, env)?)
                    }
                    (TypedExpressionNode::Bool(false), Some(false_branch)) => {
                        Ok(Interpreter::eval(&false_branch, env)?)
                    }
                    (TypedExpressionNode::Bool(false), _) => {
                        Ok(Closure::simple(TypedExpression::void()))
                    }
                    _ => Err(InterpreterError::new(format!(
                        "Unexpected result of conditional"
                    ))),
                }
            }
            TypedExpressionNode::Group(g) => Interpreter::eval(&g.expr, env),
            TypedExpressionNode::Bool(b) => Ok(Closure::simple(TypedExpression::bool(*b))),
            TypedExpressionNode::Integer(i) => Ok(Closure::simple(TypedExpression::integer(*i))),
            TypedExpressionNode::String(s) => {
                Ok(Closure::simple(TypedExpression::string(s.to_string())))
            }
            TypedExpressionNode::Array(_) => Ok(Closure::simple(expr.clone())),
            TypedExpressionNode::Symbol(s) => {
                let value = env
                    .lookup(s)
                    .ok_or_else(|| InterpreterError::new(format!("unknown symbol '{}'", s)))?;
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
                let mut last = Closure::simple(TypedExpression::void());
                let mut scope = env.clone();
                for expr in &b.list {
                    last = Interpreter::eval(&expr, &mut scope)?;
                    match &last.expr.node {
                        TypedExpressionNode::Break(break_expr) => {
                            Some(Closure::simple(break_expr.expr.clone()));
                            break;
                        }
                        _ => (),
                    }
                }
                Ok(last)
            }
            TypedExpressionNode::Loop(b) => {
                let mut last: Option<Closure> = None;
                let mut scope = env.clone();
                while last.is_none() {
                    for expr in &b.list {
                        let res = Interpreter::eval(&expr, &mut scope)?;
                        last = match res.expr.node {
                            TypedExpressionNode::Break(break_expr) => {
                                Some(Closure::simple(break_expr.expr.clone()))
                            }
                            _ => None,
                        }
                    }
                }
                last.ok_or(InterpreterError::new("internal error".to_string()))
            }
            TypedExpressionNode::Break(b) => {
                let expr = &b.expr;
                let closure = Interpreter::eval(&expr, &mut env)?;
                Ok(Closure::complete(
                    TypedExpression::r#break(closure.expr),
                    closure.env,
                ))
            }
            TypedExpressionNode::Program(program) => {
                let mut last = Ok(Closure::simple(TypedExpression::void()));
                for expr in &program.list {
                    last = Interpreter::eval(&expr, &mut env);
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

            TypedExpressionNode::NativeFunction(_) => Ok(Closure::simple(expr.clone())),
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
                                    .map_err(|message| InterpreterError::new(message))?;
                                Ok(Closure::simple(res))
                            }
                            _ => Err(InterpreterError::new(format!(
                                "unexpected number of arguments to native function"
                            ))),
                        }
                    }
                    _ => Err(InterpreterError::new(format!(
                        "unexpected {:?}, expected function",
                        fc.expr
                    ))),
                }
            }
        }
    }
}
