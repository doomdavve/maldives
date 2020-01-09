use crate::expression::TypeDeclaration;

use std::rc::Rc;

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
    pub fn from_optional_decl(optional_decl: &Option<TypeDeclaration>) -> Result<ResolvedType, ()> {
        match optional_decl {
            Some(decl) => ResolvedType::from_decl(decl),
            None => Ok(ResolvedType::Any),
        }
    }

    pub fn from_decl(decl: &TypeDeclaration) -> Result<ResolvedType, ()> {
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
