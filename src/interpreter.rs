use std::rc::Rc;

use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::expression::Expression;
use crate::expression::BinaryOperation;
use crate::expression::NativeFunctionExpr;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    expr: Expression,
    env: Option<SymbolTable>,
}

impl Closure {
    fn simple(expr: Expression) -> Closure {
        Closure { expr, env: None }
    }
    fn complete(expr: Expression, env: Option<SymbolTable>) -> Closure {
        Closure { expr, env: env }
    }
}

type SymbolTable = HashMap<String, Closure>;

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError {
    message: String,
    vars: SymbolTable,
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Interpreter error: {}", self.message)?;
        write!(f, "Environment: {:?}", self.vars)
    }
}

impl error::Error for InterpreterError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl InterpreterError {
    fn new(message: String, vars: SymbolTable) -> InterpreterError {
        InterpreterError { message, vars }
    }
}

pub struct Interpreter {
    vars: SymbolTable,
}

fn native_println(e: &Expression) -> Result<Expression, String> {
    match e {
        Expression::String(s) => {
            println!("{}", s);
            Ok(Expression::Void)
        }
        _ => Err("Unexpected argument to println".to_string())
    }
}

fn native_type(e: &Expression) -> Result<Expression, String> {
    Ok(Expression::String(format!("{:?}", e.resolve_type())))
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut interpreter = Interpreter {
            vars: SymbolTable::new(),
        };
        interpreter.vars.insert("println".to_string(), Closure::simple(Expression::NativeFunction(Rc::new(NativeFunctionExpr{ function: native_println }))));
        interpreter.vars.insert("type".to_string(), Closure::simple(Expression::NativeFunction(Rc::new(NativeFunctionExpr{ function: native_type }))));
        interpreter
    }

    fn error(&self, message: String) -> InterpreterError {
        InterpreterError::new(message, self.vars.clone())
    }

    #[cfg(test)]
    pub fn set(&mut self, symbol: String, value: Expression) -> Option<Closure> {
        self.vars.insert(
            symbol,
            Closure {
                expr: value,
                env: None,
            },
        )
    }

    pub fn eval_global(&mut self, expression: &Expression) -> Result<Expression, InterpreterError> {
        let closure = self.eval(expression, &self.vars.clone())?;
        Ok(closure.expr)
    }

    fn eval(
        &mut self,
        expression: &Expression,
        env: &SymbolTable,
    ) -> Result<Closure, InterpreterError> {
        debug!("Evaulating {:?} with vars: {:?}", expression, self.vars);

        match expression {
            Expression::Void => Ok(Closure::simple(Expression::Void)),
            Expression::Binary(b) => {
                let l = self.eval(&b.left, env)?;
                let r = self.eval(&b.right, env)?;
                match b.operation {
                    BinaryOperation::Sum => {
                        match (l.expr, r.expr) {
                            (Expression::Integer(li), Expression::Integer(ri)) => {
                                Ok(Closure::simple(Expression::Integer(li + ri)))
                            }
                            (Expression::String(li), Expression::String(ri)) => {
                                Ok(Closure::simple(Expression::String(li + &ri)))
                            }
                            _ => Err(self
                                .error(format!("Unexpected terms in sum operator"))),
                        }
                    }
                    BinaryOperation::Difference => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Integer(li - ri)))
                        }
                        _ => Err(self.error(format!(
                            "One or more non-integer terms to difference operator"
                        ))),
                    },
                    BinaryOperation::Multiply => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Integer(li * ri)))
                        }
                        _ => Err(self.error(format!(
                            "One or more non-integer terms to multiply operator"
                        ))),
                    },
                    BinaryOperation::Divide => {
                        // TODO: handle division by zero.
                        match (l.expr, r.expr) {
                            (Expression::Integer(li), Expression::Integer(ri)) => {
                                Ok(Closure::simple(Expression::Integer(li / ri)))
                            }
                            _ => Err(self.error(format!(
                                "One or more non-integer terms to divide operator"
                            ))),
                        }
                    }
                    BinaryOperation::LessThan => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Bool(li < ri)))
                        }
                        _ => {
                            Err(self
                                .error(format!("One or more non-boolean terms to divide operator")))
                        }
                    },
                    BinaryOperation::GreaterThan => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Bool(li > ri)))
                        }
                        _ => {
                            Err(self
                                .error(format!("One or more non-boolean terms to divide operator")))
                        }
                    },
                    BinaryOperation::LessEqualThan => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Bool(li <= ri)))
                        }
                        _ => {
                            Err(self
                                .error(format!("One or more non-boolean terms to divide operator")))
                        }
                    },
                    BinaryOperation::GreaterEqualThan => match (l.expr, r.expr) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Closure::simple(Expression::Bool(li >= ri)))
                        }
                        _ => {
                            Err(self
                                .error(format!("One or more non-boolean terms to divide operator")))
                        }
                    },
                }
            }
            Expression::Conditional(c) => {
                let premise = self.eval(&c.condition, env)?;
                match (premise.expr, c.false_branch.as_ref()) {
                    (Expression::Bool(true), _) => Ok(self.eval(&c.true_branch, env)?),
                    (Expression::Bool(false), Some(false_branch)) => {
                        Ok(self.eval(&false_branch, env)?)
                    }
                    (Expression::Bool(false), _) => Ok(Closure::simple(Expression::Void)),
                    _ => Err(self.error(format!("Unexpected result of conditional"))),
                }
            }
            Expression::Group(g) => self.eval(&g.expr, env),
            Expression::Bool(b) => Ok(Closure::simple(Expression::Bool(*b))),
            Expression::Integer(i) => Ok(Closure::simple(Expression::Integer(*i))),
            Expression::String(s) => Ok(Closure::simple(Expression::String(s.to_string()))),
            Expression::Symbol(s) => {
                let value = self
                    .vars
                    .get(s)
                    .ok_or_else(|| self.error(format!("unknown symbol '{}'", s)))?;
                Ok(Closure::complete(value.expr.clone(), value.env.clone()))
            }
            Expression::Bind(b) => {
                let val = self.eval(&b.expr, env)?;
                self.vars.insert(
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
                let scope = env.clone();
                for expr in &b.list {
                    last = self.eval(&expr, &scope);
                }
                last
            }
            Expression::Function(f) => {
                let closure = Closure {
                    expr: expression.clone(),
                    env: Some(env.clone()),
                };
                if let Some(sym) = &f.sym {
                    self.vars.insert(String::from(sym), closure.clone());
                }
                Ok(closure.clone())
            }
            Expression::NativeFunction(n) => Ok(Closure::simple(Expression::NativeFunction(n.clone()))),
            Expression::FunctionCall(fc) => {
                let value = self.eval(&fc.expr, env)?;
                match (value.expr, value.env) {
                    (Expression::Function(f), Some(function_env)) => {
                        debug!("Parameters: {:?}", f.parameters);
                        for (idx, parameter) in f.parameters.iter().enumerate() {
                            let argument = fc.arguments.get(idx).unwrap_or(&Expression::Void);
                            let val = self.eval(argument, env)?;
                            debug!("Mapping: {:?} -> {:?}", parameter, val);
                            self.vars.insert(
                                String::from(parameter),
                                Closure::complete(val.expr.clone(), val.env.clone()),
                            );
                        }
                        debug!("Calling function with env: {:?}", &function_env);
                        let result = self.eval(&f.expr, &function_env)?;
                        Ok(result)
                    }
                    (Expression::NativeFunction(f), _) => {
                        let native_function = f.function;
                        match fc.arguments.as_slice() {
                            [only_one] => {
                                let arg = self.eval(&only_one, env)?;
                                let res = native_function(&arg.expr).map_err(|message| self.error(message))?;
                                Ok(Closure::simple(res))
                            }
                            _ =>  {
                                Err(self.error(format!("unexpected number of argumens to native function")))
                            }
                        }
                    }
                    _ => Err(self.error(format!("unexpected {:?}, expected function", fc.expr))),
                }
            }
        }
    }
}
