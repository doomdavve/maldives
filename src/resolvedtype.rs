use std::fmt;
use std::rc::Rc;

use crate::expression::TypeDeclaration;

#[derive(Debug, PartialEq)]
pub struct ResolvedFunctionType {
    pub return_type: ResolvedType,
    pub parameters: Vec<ResolvedType>,
}

impl fmt::Display for ResolvedFunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parameters: Vec<String> = self
            .parameters
            .clone()
            .into_iter()
            .map(|i| i.to_string())
            .collect();
        write!(f, "({}) -> {}", parameters.join(", "), self.return_type)
    }
}

pub enum AllocatedStructIds {
    Array,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ResolvedType {
    Integer,
    Bool,
    String,
    Function(Rc<ResolvedFunctionType>),
    Any,
    None,
    Never,
    Break(Box<ResolvedType>),
    Array(Box<ResolvedType>),
    TypeParameterId(usize),
    VarArgs,
    Struct(u32),
    Sdl,
}

impl fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ResolvedType::Integer => write!(f, "int"),
            ResolvedType::Bool => write!(f, "bool"),
            ResolvedType::String => write!(f, "string"),
            ResolvedType::Function(resolved_fn) => write!(f, "{}", resolved_fn),
            ResolvedType::Any => write!(f, "any"),
            ResolvedType::None => write!(f, "none"),
            ResolvedType::Never => write!(f, "never"),
            ResolvedType::Break(break_expression) => write!(f, "break <{}>", break_expression),
            ResolvedType::Array(t) => write!(f, "array[{}]", t),
            ResolvedType::TypeParameterId(id) => write!(f, "T[{}]", id),
            ResolvedType::VarArgs => write!(f, "..."),
            ResolvedType::Struct(id) => write!(f, "struct <{}>", id),
            ResolvedType::Sdl => write!(f, "sdl"),
        }
    }
}

impl ResolvedType {
    pub fn from_decl(decl: &TypeDeclaration) -> Option<ResolvedType> {
        match decl {
            TypeDeclaration::Symbol(s) => match s.as_ref() {
                "int" => Some(ResolvedType::Integer),
                "bool" => Some(ResolvedType::Bool),
                "string" => Some(ResolvedType::String),
                "any" => Some(ResolvedType::Any),
                "none" => Some(ResolvedType::None),
                "!" => Some(ResolvedType::Never),
                _ => None,
            },
            TypeDeclaration::Function(f) => {
                let return_type = ResolvedType::from_decl(&f.return_type)?;
                let mut parameters: Vec<ResolvedType> = Vec::new();
                for parameter in &f.parameters {
                    parameters.push(ResolvedType::from_decl(parameter)?)
                }
                Some(ResolvedType::Function(Rc::new(ResolvedFunctionType {
                    return_type,
                    parameters,
                })))
            }
        }
    }

    pub fn function(return_type: ResolvedType, parameters: Vec<ResolvedType>) -> ResolvedType {
        ResolvedType::Function(Rc::new(ResolvedFunctionType {
            return_type,
            parameters,
        }))
    }

    pub fn complete_type(
        resolved_type: &ResolvedType,
        type_arguments: &[TypeDeclaration],
    ) -> Option<ResolvedType> {
        match resolved_type {
            Self::Array(t) => Some(ResolvedType::Array(Box::new(ResolvedType::complete_type(
                t,
                type_arguments,
            )?))),
            Self::TypeParameterId(id) => Self::from_decl(&type_arguments[*id]),
            Self::Function(resolved_fn) => {
                let mut parameters: Vec<ResolvedType> =
                    Vec::with_capacity(resolved_fn.parameters.len());
                for parameter in &resolved_fn.parameters {
                    parameters.push(ResolvedType::complete_type(parameter, type_arguments)?)
                }
                let return_type =
                    ResolvedType::complete_type(&resolved_fn.return_type, type_arguments)?;
                Some(ResolvedType::Function(Rc::new(ResolvedFunctionType {
                    return_type,
                    parameters,
                })))
            }
            _ => Some(resolved_type.clone()),
        }
    }

    pub fn struct_id(&self) -> Result<u32, String> {
        match self {
            ResolvedType::Struct(id) => Ok(*id),
            ResolvedType::Array(_) => Ok(AllocatedStructIds::Array as u32),
            _ => Err("unexpected error2".to_string()),
        }
    }
}
