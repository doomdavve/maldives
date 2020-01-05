use std::str::FromStr;
use crate::expression::{Expression, BinaryOperation};
#[derive(Debug, PartialEq)]
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

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {}
    }

    pub fn resolve_binary_operation_type(&mut self, operation: &BinaryOperation, left: &Expression, right: &Expression) -> Result<ResolvedType, String> {
        match operation {
            BinaryOperation::Sum => {
                let left_type = self.resolve_type(left)?;
                let right_type = self.resolve_type(right)?;
                if left_type == right_type {
                    Ok(left_type)
                } else {
                    Err("Left and right expressions have different types".to_string())
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

    // Should we pass in the symbol table here? Which one? :)
    pub fn resolve_type(&mut self, expr: &Expression) -> Result<ResolvedType, String> {
        match expr {
            Expression::Integer(_) => Ok(ResolvedType::Integer),
            Expression::String(_) => Ok(ResolvedType::String),
            Expression::Bool(_) => Ok(ResolvedType::Bool),
            Expression::Function(_) => Ok(ResolvedType::Function),
            Expression::Bind(bind) => self.resolve_type(&bind.expr),
            Expression::Block(block) => block
                .list
                .last()
                .map(|e| self.resolve_type(e))
                .unwrap_or(Ok(ResolvedType::None)),
            Expression::FunctionCall(fc) => self.resolve_type(&fc.expr), // TODO: here we have work to do.
            Expression::Binary(binary) => {
                self.resolve_binary_operation_type(&binary.operation, &binary.left, &binary.right)
            }
            Expression::Group(group) => self.resolve_type(&group.expr),
            Expression::Conditional(_) => Ok(ResolvedType::Any), // Check both braches and assert they are the same
            Expression::NativeFunction(_) => Ok(ResolvedType::Any), // One more thing left to do...
            Expression::Void => Ok(ResolvedType::None),
            Expression::Symbol(_) => Ok(ResolvedType::Any), // Need the symbol table to tell
        }
    }
}
