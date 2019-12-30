use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::parser::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError;

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Interpreter error")
    }
}

impl error::Error for InterpreterError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

pub struct Interpreter
{
    vars: HashMap<String, i32>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            vars: HashMap::new()
        }
    }

    #[cfg(test)]
    pub fn set(&mut self, symbol: String, value: i32) -> Option<i32> {
        self.vars.insert(symbol, value)
    }

    pub fn eval(&mut self, expression: Expression) -> Result<i32, InterpreterError> {
        match expression {
            Expression::Integer(i) => {
                Ok(i)
            }
            Expression::Symbol(s) => {
                self.vars.get(&s).copied().ok_or(InterpreterError)
            }
            Expression::Bind(b) => {
                let bind_expr = *b;
                let val = self.eval(bind_expr.expr)?;
                self.vars.insert(bind_expr.sym, val).ok_or(InterpreterError)
            }
            Expression::Block(b) => {
                let block_expr = *b;
                let mut last = Ok(0);
                for expr in block_expr.list {
                    last = self.eval(expr);
                }
                last
            }
            _ => Err(InterpreterError)
        }
    }
}

