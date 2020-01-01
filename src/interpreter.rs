use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::parser::Expression;
use crate::parser::Operation;

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError {
    message: String,
    vars: HashMap<String, Expression>,
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
    fn new(message: String, vars: HashMap<String, Expression>) -> InterpreterError {
        InterpreterError { message, vars }
    }
}

pub struct Interpreter {
    vars: HashMap<String, Expression>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            vars: HashMap::new(),
        }
    }

    fn error(&self, message: String) -> InterpreterError {
        InterpreterError::new(message, self.vars.clone())
    }

    #[cfg(test)]
    pub fn set(&mut self, symbol: String, value: Expression) -> Option<Expression> {
        self.vars.insert(symbol, value)
    }

    pub fn eval(&mut self, expression: &Expression) -> Result<Expression, InterpreterError> {
        debug!("Evaulating {:?} with vars: {:?}", expression, self.vars);

        match expression {
            Expression::Void => Ok(Expression::Void),
            Expression::Binary(b) => {
                let l = self.eval(&b.left)?;
                let r = self.eval(&b.right)?;
                match b.operation {
                    Operation::Sum => {
                        match (l, r) {
                            (Expression::Integer(li), Expression::Integer(ri)) => {
                                Ok(Expression::Integer(li + ri))
                            }
                            _ => Err(self
                                .error(format!("One or more non-integer terms to sum operator"))),
                        }
                    }
                    Operation::Difference => match (l, r) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Expression::Integer(li - ri))
                        }
                        _ => Err(self.error(format!(
                            "One or more non-integer terms to difference operator"
                        ))),
                    },
                    Operation::Multiply => match (l, r) {
                        (Expression::Integer(li), Expression::Integer(ri)) => {
                            Ok(Expression::Integer(li * ri))
                        }
                        _ => Err(self.error(format!(
                            "One or more non-integer terms to multiply operator"
                        ))),
                    },
                    Operation::Divide => {
                        // TODO: handle division by zero.
                        match (l, r) {
                            (Expression::Integer(li), Expression::Integer(ri)) => {
                                Ok(Expression::Integer(li / ri))
                            }
                            _ => Err(self.error(format!(
                                "One or more non-integer terms to divide operator"
                            ))),
                        }
                    }
                }
            }
            Expression::Group(g) => self.eval(&g.expr),
            Expression::Integer(i) => Ok(Expression::Integer(*i)),
            Expression::Symbol(s) => {
                let value = self
                    .vars
                    .get(s)
                    .ok_or_else(|| self.error(format!("unknown symbol '{}'", s)))?;
                Ok(value.clone())
            }
            Expression::Bind(b) => {
                let val = self.eval(&b.expr)?;
                self.vars.insert(String::from(&b.sym), val.clone());
                Ok(val)
            }
            Expression::Block(b) => {
                let mut last = Ok(Expression::Void);
                for expr in &b.list {
                    last = self.eval(&expr);
                }
                last
            }
            Expression::Function(f) => {
                if let Some(sym) = &f.sym {
                    self.vars.insert(String::from(sym), expression.clone());
                }
                Ok(expression.clone())
            }
            Expression::FunctionCall(fc) => {
                let value = self.eval(&fc.expr)?;
                match value {
                    Expression::Function(f) => {
                        let old_vars = self.vars.clone();
                        debug!("Parameters: {:?}", f.parameters);
                        for (idx, parameter) in f.parameters.iter().enumerate() {
                            let argument = fc.arguments.get(idx).unwrap_or(&Expression::Void);
                            let val = self.eval(argument)?;
                            debug!("Mapping: {:?} -> {:?}", parameter, val);
                            self.vars.insert(String::from(parameter), val.clone());
                        }
                        let result = self.eval(&f.expr)?;
                        self.vars = old_vars;
                        Ok(result)
                    }
                    _ => Err(self.error(format!(
                        "{:?} evaluated to {:?}; expected function",
                        fc.expr, value
                    ))),
                }
            }
        }
    }
}
