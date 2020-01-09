use crate::expression::TypeDeclaration;
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::rc::Rc;

use crate::expression::{BinaryOperation, Expression};

pub type TypeTable = HashMap<String, ResolvedType>;

#[derive(Debug, PartialEq)]
pub struct ResolvedFunctionType {
    pub return_type: ResolvedType,
    pub parameters: Vec<ResolvedType>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ResolvedType {
    Integer,
    Bool,
    String,
    Function(Rc<ResolvedFunctionType>),
    Any,
    None,
}

impl ResolvedType {
    fn from_optional_decl(optional_decl: &Option<TypeDeclaration>) -> Result<ResolvedType, ()> {
        match optional_decl {
            Some(decl) => ResolvedType::from_decl(decl),
            None => Ok(ResolvedType::Any),
        }
    }

    fn from_decl(decl: &TypeDeclaration) -> Result<ResolvedType, ()> {
        match decl {
            TypeDeclaration::Symbol(s) => match s.as_ref() {
                "int" => Ok(ResolvedType::Integer),
                "bool" => Ok(ResolvedType::Bool),
                "string" => Ok(ResolvedType::String),
                "any" => Ok(ResolvedType::Any),
                "none" => Ok(ResolvedType::None),
                _ => Err(()),
            },
            TypeDeclaration::Function(f) => {
                let return_type = ResolvedType::from_decl(&f.return_type)?;
                let mut parameters: Vec<ResolvedType> = Vec::new();
                for parameter in &f.parameters {
                    parameters.push(ResolvedType::from_decl(&parameter)?)
                }
                Ok(ResolvedType::Function(Rc::new(ResolvedFunctionType {
                    return_type,
                    parameters,
                })))
            }
        }
    }
}

pub struct TypeChecker {
    vars: TypeTable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckerError {
    message: String,
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
                    Err(TypeCheckerError::new(
                        "Left and right expressions have different types".to_string(),
                    ))
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
                let resolved_return_type =
                    ResolvedType::from_decl(&function.return_type).map_err(|_e| {
                        TypeCheckerError::new("Could not resolve return type".to_string())
                    })?;

                let mut resolved_parameter_types: Vec<ResolvedType> = Vec::new();
                for parameter in &function.parameters {
                    resolved_parameter_types.push(ResolvedType::from_decl(&parameter.1).map_err(
                        |_e| TypeCheckerError::new("Could not resolve parameter type".to_string()),
                    )?);
                }

                let resolved_function_type = ResolvedFunctionType {
                    return_type: resolved_return_type,
                    parameters: resolved_parameter_types,
                };
                let rft = Rc::new(resolved_function_type);
                if let Some(sym) = &function.sym {
                    self.vars
                        .insert(String::from(sym), ResolvedType::Function(rft.clone()));
                }
                Ok(ResolvedType::Function(rft.clone()))
            }
            Expression::Bind(bind) => {
                let resolved_type = TypeChecker::match_type(
                    ResolvedType::from_optional_decl(&bind.sym_type).map_err(|_e| {
                        TypeCheckerError::new("Could not resolve type".to_string())
                    })?,
                    self.resolve_type(&bind.expr)?,
                )?;
                self.vars
                    .insert(String::from(&bind.sym), resolved_type.clone());
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
                let resolved_type = self
                    .vars
                    .get(symbol)
                    .ok_or_else(|| TypeCheckerError::new(format!("unknown symbol '{}'", symbol)))?;
                Ok(resolved_type.clone())
            }
        }
    }

    fn match_type(
        _specified_type: ResolvedType,
        resolved_type: ResolvedType,
    ) -> Result<ResolvedType, TypeCheckerError> {
        Ok(resolved_type)
    }
}
