use std::str::FromStr;
use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::expression::{BinaryOperation, Expression};

pub type TypeTable = HashMap<String, ResolvedType>;

#[derive(Debug, PartialEq, Clone)]
pub enum ResolvedType {
    Integer,
    Bool,
    String,
    Function,
    Any,
    None,
}

impl FromStr for ResolvedType {
    type Err = ();

    fn from_str(s: &str) -> Result<ResolvedType, ()> {
        match s {
            "int" => Ok(ResolvedType::Integer),
            "bool" => Ok(ResolvedType::Bool),
            "string" => Ok(ResolvedType::String),
            "fn" => Ok(ResolvedType::Function),
            "any" => Ok(ResolvedType::Any),
            "none" => Ok(ResolvedType::None),
            _ => Err(()),
        }
    }
}

pub struct TypeChecker {
    vars: TypeTable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckerError {
    message: String
}

impl fmt::Display for TypeCheckerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "TypeChecker error: {}", self.message)
    }
}

impl error::Error for TypeCheckerError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl TypeCheckerError {
    fn new(message: String) -> TypeCheckerError {
        TypeCheckerError { message }
    }
}


impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            vars: TypeTable::new(),
        }
    }

    pub fn resolve_binary_operation_type(
        &mut self,
        operation: &BinaryOperation,
        left: &Expression,
        right: &Expression,
    ) -> Result<ResolvedType, TypeCheckerError> {
        match operation {
            BinaryOperation::Sum => {
                let left_type = self.resolve_type(left)?;
                let right_type = self.resolve_type(right)?;
                if left_type == right_type {
                    Ok(left_type)
                } else {
                    Err(TypeCheckerError::new("Left and right expressions have different types".to_string()))
                }
            }
            BinaryOperation::Difference | BinaryOperation::Multiply | BinaryOperation::Divide => {
                Ok(ResolvedType::Integer)
            }
            BinaryOperation::LessThan
            | BinaryOperation::GreaterThan
            | BinaryOperation::LessEqualThan
            | BinaryOperation::GreaterEqualThan => Ok(ResolvedType::Bool),
        }
    }

    pub fn resolve_type(&mut self, expr: &Expression) -> Result<ResolvedType, TypeCheckerError> {
        match expr {
            Expression::Integer(_) => Ok(ResolvedType::Integer),
            Expression::String(_) => Ok(ResolvedType::String),
            Expression::Bool(_) => Ok(ResolvedType::Bool),
            Expression::Function(function) => {
                if let Some(sym) = &function.sym {
                    self.vars.insert(String::from(sym), ResolvedType::Function);
                }
                Ok(ResolvedType::Function)
            },
            Expression::Bind(bind) => {
                let resolved_type = self.resolve_type(&bind.expr)?;
                self.vars.insert(String::from(&bind.sym), resolved_type.clone());
                Ok(resolved_type)
            }
            Expression::Block(block) => {
                let mut last = ResolvedType::None;
                for expr in &block.list {
                    last = self.resolve_type(&expr)?;
                }
                Ok(last)
            }
            Expression::FunctionCall(_) => Ok(ResolvedType::Any), // TODO: here we have work to do. If self.resolve_type(&fc.expr) would return Function(ResolvedType)...
            Expression::Binary(binary) => {
                self.resolve_binary_operation_type(&binary.operation, &binary.left, &binary.right)
            }
            Expression::Group(group) => self.resolve_type(&group.expr),
            Expression::Conditional(_) => Ok(ResolvedType::Any), // Check both braches and assert they are the same
            Expression::NativeFunction(_) => Ok(ResolvedType::Any), // One more thing left to do...
            Expression::Void => Ok(ResolvedType::None),
            Expression::Symbol(symbol) => {
                let resolved_type = self.vars.get(symbol).ok_or_else(|| TypeCheckerError::new(format!("unknown symbol '{}'", symbol)))?;
                Ok(resolved_type.clone())
            }
        }
    }
}
