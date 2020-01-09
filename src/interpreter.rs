use std::error;
use std::fmt;

use crate::expression::BinaryOperation;
use crate::expression::Expression;
use crate::symboltable::Closure;
use crate::symboltable::SymbolTable;

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
        expression: &Expression,
        env: &mut SymbolTable,
    ) -> Result<Expression, InterpreterError> {
        let closure = Interpreter::eval(expression, env)?;
        Ok(closure.expr)
    }

    fn eval(expression: &Expression, env: &mut SymbolTable) -> Result<Closure, InterpreterError> {
        debug!("Evaulating {:?} with vars: {:?}", expression, env);

        match expression {
            Expression::Void => Ok(Closure::simple(Expression::Void)),
            Expression::Binary(b) => {
                let l = Interpreter::eval(&b.left, env)?;
                let r = Interpreter::eval(&b.right, env)?;
                match b.operation {
                    BinaryOperation::Sum => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Integer(li + ri)))
                        }
                        (Expression::String(li), Expression::String(ri)) => {
                            Ok(Closure::simple(Expression::String(li + &ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("Unexpected terms in sum operator"),
                            env,
                        )),
                    },
                    BinaryOperation::Difference => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Integer(li - ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-integer terms to difference operator"),
                            env,
                        )),
                    },
                    BinaryOperation::Multiply => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Integer(li * ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-integer terms to multiply operator"),
                            env,
                        )),
                    },
                    BinaryOperation::Divide => {
                        // TODO: handle division by zero.
                        match (l.expr, r.expr) {
                            (Expression::Integer(li), Expression::Integer(ri)) => {
                                Ok(Closure::simple(Expression::Integer(li / ri)))
                            }
                            _ => Err(InterpreterError::new(
                                format!("One or more non-integer terms to divide operator"),
                                env,
                            )),
                        }
                    }
                    BinaryOperation::LessThan => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Bool(li < ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-boolean terms to divide operator"),
                            env,
                        )),
                    },
                    BinaryOperation::GreaterThan => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Bool(li > ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-boolean terms to divide operator"),
                            env,
                        )),
                    },
                    BinaryOperation::LessEqualThan => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Bool(li <= ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-boolean terms to divide operator"),
                            env,
                        )),
                    },
                    BinaryOperation::GreaterEqualThan => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Bool(li >= ri)))
                        }
                        _ => Err(InterpreterError::new(
                            format!("One or more non-boolean terms to divide operator"),
                            env,
                        )),
                    },
                }
            }
            Expression::Conditional(c) => {
                let premise = Interpreter::eval(&c.condition, env)?;
                match (premise.expr, c.false_branch.as_ref()) {
                    (Expression::Bool(true), _) => Ok(Interpreter::eval(&c.true_branch, env)?),
                    (Expression::Bool(false), Some(false_branch)) => {
                        Ok(Interpreter::eval(&false_branch, env)?)
                    }
                    (Expression::Bool(false), _) => Ok(Closure::simple(Expression::Void)),
                    _ => Err(InterpreterError::new(
                        format!("Unexpected result of conditional"),
                        env,
                    )),
                }
            }
            Expression::Group(g) => Interpreter::eval(&g.expr, env),
            Expression::Bool(b) => Ok(Closure::simple(Expression::Bool(*b))),
            Expression::Integer(i) => Ok(Closure::simple(Expression::Integer(*i))),
            Expression::String(s) => Ok(Closure::simple(Expression::String(s.to_string()))),
            Expression::Symbol(s) => {
                let value = env
                    .lookup(s)
                    .ok_or_else(|| InterpreterError::new(format!("unknown symbol '{}'", s), env))?;
                Ok(Closure::complete(value.expr.clone(), value.env.clone()))
            }
            Expression::Bind(b) => {
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
            Expression::Block(b) => {
                let mut last = Ok(Closure::simple(Expression::Void));
                let mut scope = env.clone();
                for expr in &b.list {
                    last = Interpreter::eval(&expr, &mut scope);
                }
                last
            }
            Expression::Function(f) => {
                let closure = Closure {
                    expr: expression.clone(),
                    env: Some(env.clone()),
                };
                if let Some(sym) = &f.sym {
                    env.bind(String::from(sym), closure.clone());
                }
                Ok(closure.clone())
            }
            Expression::NativeFunction(n) => {
                Ok(Closure::simple(Expression::NativeFunction(n.clone())))
            }
            Expression::FunctionCall(fc) => {
                let value = Interpreter::eval(&fc.expr, env)?;
                match (value.expr, value.env) {
                    (Expression::Function(f), Some(function_env)) => {
                        debug!("Parameters: {:?}", f.parameters);
                        let mut call_env = function_env.clone();
                        for (idx, parameter) in f.parameters.iter().enumerate() {
                            let argument = fc.arguments.get(idx).unwrap_or(&Expression::Void);
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
                    (Expression::NativeFunction(f), _) => {
                        let native_function = f.function;
                        match fc.arguments.as_slice() {
                            [only_one] => {
                                let arg = Interpreter::eval(&only_one, env)?;
                                let res = native_function(&arg.expr)
                                    .map_err(|message| InterpreterError::new(message, env))?;
                                Ok(Closure::simple(res))
                            }
                            _ => Err(InterpreterError::new(
                                format!("unexpected number of argumens to native function"),
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
