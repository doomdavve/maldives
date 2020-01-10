use std::fmt;
use std::rc::Rc;

use crate::resolvedtype::ResolvedType;

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpression {
    pub resolved_type: ResolvedType,
    pub node: TypedExpressionNode,
}

impl TypedExpression {
    pub fn void() -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::None,
            node: TypedExpressionNode::Void,
        }
    }
    pub fn integer(i: i32) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Integer,
            node: TypedExpressionNode::Integer(i),
        }
    }
    pub fn bool(b: bool) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Bool,
            node: TypedExpressionNode::Bool(b),
        }
    }
    pub fn string(s: String) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::String,
            node: TypedExpressionNode::String(s),
        }
    }
    pub fn symbol(s: String, resolved_type: ResolvedType) -> TypedExpression {
        TypedExpression {
            resolved_type,
            node: TypedExpressionNode::Symbol(s),
        }
    }
    pub fn group(expr: TypedExpression) -> TypedExpression {
        TypedExpression {
            resolved_type: expr.resolved_type.clone(),
            node: TypedExpressionNode::Group(Rc::new(TypedGroupExpr { expr })),
        }
    }
    pub fn conditional(
        condition: TypedExpression,
        return_type: ResolvedType,
        true_branch: TypedExpression,
        false_branch: Option<TypedExpression>,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type: return_type,
            node: TypedExpressionNode::Conditional(Rc::new(TypedConditionalExpr {
                condition,
                true_branch,
                false_branch,
            })),
        }
    }
    pub fn binary_operation(
        operation: TypedBinaryOperation,
        operation_type: ResolvedType,
        left: TypedExpression,
        right: TypedExpression,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type: operation_type,
            node: TypedExpressionNode::BinaryOperation(Rc::new(TypedBinaryOperationExpr {
                operation,
                left,
                right,
            })),
        }
    }
    pub fn block(list: Vec<TypedExpression>) -> TypedExpression {
        let block_resolved_type = list
            .last()
            .map(|last| last.resolved_type.clone())
            .unwrap_or_else(|| ResolvedType::None);
        TypedExpression {
            resolved_type: block_resolved_type,
            node: TypedExpressionNode::Block(Rc::new(TypedBlockExpr { list })),
        }
    }
    pub fn bind(symbol: String, expr: TypedExpression) -> TypedExpression {
        TypedExpression {
            resolved_type: expr.resolved_type.clone(),
            node: TypedExpressionNode::Bind(Rc::new(TypedBindExpr { sym: symbol, expr })),
        }
    }

    pub fn function(sym: Option<String>, return_type: ResolvedType, parameters: Vec<(String, ResolvedType)>, expr: TypedExpression) -> TypedExpression {
        TypedExpression {
            resolved_type: return_type,
            node: TypedExpressionNode::Function(Rc::new( TypedFunctionExpr {
                sym,
                parameters,
                expr
            }))
        }
    }

    pub fn call(expr: TypedExpression, return_type: ResolvedType, arguments: Vec<TypedExpression>) -> TypedExpression {
        TypedExpression {
            resolved_type: return_type,
            node: TypedExpressionNode::FunctionCall(Rc::new( TypedFunctionCallExpr {
                arguments,
                expr
            }))
        }
    }

    pub fn native_function(f: fn(e: &TypedExpression) -> Result<TypedExpression, String>) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::None,
            node: TypedExpressionNode::NativeFunction(Rc::new(TypedNativeFunctionExpr {
                function: f,
                call_by_value: true,
            }))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpressionNode {
    Integer(i32),
    Bool(bool),
    Function(Rc<TypedFunctionExpr>),
    NativeFunction(Rc<TypedNativeFunctionExpr>),
    BinaryOperation(Rc<TypedBinaryOperationExpr>),
    FunctionCall(Rc<TypedFunctionCallExpr>),
    Bind(Rc<TypedBindExpr>),
    Block(Rc<TypedBlockExpr>),
    Group(Rc<TypedGroupExpr>),
    String(String),
    Symbol(String),
    Conditional(Rc<TypedConditionalExpr>),
    Void,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TypedBinaryOperation {
    Sum,
    Concat,
    Difference,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessEqualThan,
    GreaterEqualThan,
}

#[derive(Debug, PartialEq)]
pub struct TypedBindExpr {
    pub sym: String,
    pub expr: TypedExpression,
}

#[derive(Debug, PartialEq)]
pub struct TypedGroupExpr {
    pub expr: TypedExpression,
}

#[derive(Debug, PartialEq)]
pub struct TypedFunctionCallExpr {
    pub expr: TypedExpression,
    pub arguments: Vec<TypedExpression>,
}

#[derive(Debug, PartialEq)]
pub struct TypedConditionalExpr {
    pub condition: TypedExpression,
    pub true_branch: TypedExpression,
    pub false_branch: Option<TypedExpression>,
}

#[derive(Debug, PartialEq)]
pub struct TypedBinaryOperationExpr {
    pub operation: TypedBinaryOperation,
    pub left: TypedExpression,
    pub right: TypedExpression,
}

#[derive(Debug, PartialEq)]
pub struct TypedFunctionExpr {
    pub sym: Option<String>,
    pub parameters: Vec<(String, ResolvedType)>,
    pub expr: TypedExpression,
}

#[derive(Debug, PartialEq)]
pub struct TypedBlockExpr {
    pub list: Vec<TypedExpression>,
}

pub struct TypedNativeFunctionExpr {
    pub function: fn(e: &TypedExpression) -> Result<TypedExpression, String>,
    pub call_by_value: bool,
}

impl PartialEq for TypedNativeFunctionExpr {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl fmt::Debug for TypedNativeFunctionExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Native function")
    }
}

