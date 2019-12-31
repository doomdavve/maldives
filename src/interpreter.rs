use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::parser::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError {
    message: String,
    vars: HashMap<String, Expression>
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Interpreter error: {}\n", self.message)?;
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
        InterpreterError{ message, vars }
    }
}

pub struct Interpreter
{
    vars: HashMap<String, Expression>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            vars: HashMap::new()
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
        match expression {
            Expression::Void => {
                Ok(Expression::Void)
            }
            Expression::Integer(i) => {
                Ok(Expression::Integer(*i))
            }
            Expression::Symbol(s) => {
                let val = {
                    let nested = self.vars.get(s).ok_or(self.error(format!("unknown symbol '{}'", s)))?;
                    nested.clone()
                };
                let value = self.eval(&val)?;
                Ok(value)
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
                self.vars.insert(String::from(&f.sym), expression.clone());
                Ok(Expression::Symbol(f.sym.clone()))
            },
            Expression::FunctionCall(fc) => {
                let value = {
                    let nested = self.vars.get(&fc.sym).ok_or(self.error(format!("unknown symbol '{}'", fc.sym)))?;
                    nested.clone()
                };
                match value {
                    Expression::Function(f) => {
                        let old_vars = self.vars.clone();
                        for (idx, parameter) in f.parameters.iter().enumerate() {
                            let argument = fc.arguments.get(idx).unwrap_or(&Expression::Void);
                            let val = self.eval(argument)?;
                            self.vars.insert(String::from(parameter), val.clone());
                        }
                        let result = self.eval(&f.expr)?;
                        self.vars = old_vars;
                        Ok(result)
                    }
                    _ => Err(self.error(format!("symbol '{}' did not evaluate to function", fc.sym)))
                }
            }
        }
    }
}

