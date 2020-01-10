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
    pub fn from_optional_decl(optional_decl: &Option<TypeDeclaration>) -> Option<ResolvedType> {
        match optional_decl {
            Some(decl) => ResolvedType::from_decl(decl),
            None => Some(ResolvedType::Any),
        }
    }

    pub fn from_decl(decl: &TypeDeclaration) -> Option<ResolvedType> {
        match decl {
            TypeDeclaration::Symbol(s) => match s.as_ref() {
                "int" => Some(ResolvedType::Integer),
                "bool" => Some(ResolvedType::Bool),
                "string" => Some(ResolvedType::String),
                "any" => Some(ResolvedType::Any),
                "none" => Some(ResolvedType::None),
                _ => None,
            },
            TypeDeclaration::Function(f) => {
                let return_type = ResolvedType::from_decl(&f.return_type)?;
                let mut parameters: Vec<ResolvedType> = Vec::new();
                for parameter in &f.parameters {
                    parameters.push(ResolvedType::from_decl(&parameter)?)
                }
                Some(ResolvedType::Function(Rc::new(ResolvedFunctionType {
                    return_type,
                    parameters,
                })))
            }
        }
    }
}
