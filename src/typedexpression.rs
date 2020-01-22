use std::fmt;
use std::rc::Rc;

use crate::resolvedtype::{ResolvedFunctionType, ResolvedType};
use crate::symboltable::SymbolTable;
use crate::typedexpressionnode::*;

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpression {
    pub resolved_type: ResolvedType,
    pub node: TypedExpressionNode,
}

impl fmt::Display for TypedExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} (:{})", self.node, self.resolved_type)
    }
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
    pub fn r#break(expr: TypedExpression) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Break(Box::new(expr.resolved_type.clone())),
            node: TypedExpressionNode::Break(Rc::new(TypedBreakExpr { expr })),
        }
    }
    pub fn r#loop(list: Vec<TypedExpression>, resolved_type: ResolvedType) -> TypedExpression {
        TypedExpression {
            resolved_type,
            node: TypedExpressionNode::Loop(Rc::new(TypedBlockExpr { list })),
        }
    }
    pub fn program(list: Vec<TypedExpression>) -> TypedExpression {
        let block_resolved_type = list
            .last()
            .map(|last| last.resolved_type.clone())
            .unwrap_or_else(|| ResolvedType::None);
        TypedExpression {
            resolved_type: block_resolved_type,
            node: TypedExpressionNode::Program(Rc::new(TypedBlockExpr { list })),
        }
    }
    pub fn bind(symbol: String, expr: TypedExpression) -> TypedExpression {
        TypedExpression {
            resolved_type: expr.resolved_type.clone(),
            node: TypedExpressionNode::Bind(Rc::new(TypedBindExpr { sym: symbol, expr })),
        }
    }

    pub fn array(array_type: ResolvedType, v: Vec<i32>) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Array(Box::new(array_type)),
            node: TypedExpressionNode::IntArray(Rc::new(TypedIntArrayExpr { array: v })),
        }
    }

    pub fn function(
        sym: Option<String>,
        id: u32,
        return_type: ResolvedType,
        parameters: Vec<(String, ResolvedType)>,
        expr: TypedExpression,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type: return_type,
            node: TypedExpressionNode::Function(Rc::new(TypedFunctionExpr {
                sym,
                id,
                parameters,
                expr,
            })),
        }
    }

    pub fn call(
        expr: TypedExpression,
        return_type: ResolvedType,
        arguments: Vec<TypedExpression>,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type: return_type,
            node: TypedExpressionNode::FunctionCall(Rc::new(TypedFunctionCallExpr {
                arguments,
                expr,
            })),
        }
    }

    pub fn native_function(
        f: fn(
            env: &SymbolTable,
            arguments: &Vec<TypedExpression>,
            type_arguments: &Option<Vec<ResolvedType>>,
        ) -> Result<TypedExpression, String>,
        return_type: ResolvedType,
        parameters: Vec<ResolvedType>,
        call_by_value: bool,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Function(Rc::new(ResolvedFunctionType {
                return_type,
                parameters,
            })),
            node: TypedExpressionNode::NativeFunction(Rc::new(TypedNativeFunctionExpr {
                function: f,
                call_by_value,
                type_arguments: None,
            })),
        }
    }

    pub fn type_qualified_expression(
        expr: TypedExpression,
        resolved_type: ResolvedType,
        type_arguments: Vec<ResolvedType>,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type,
            node: TypedExpressionNode::TypedTypeQualifiedExpression(Rc::new(
                TypedTypeQualifiedExpressionExpr {
                    expr,
                    type_arguments,
                },
            )),
        }
    }

    pub fn r#struct() -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Struct(0),
            node: TypedExpressionNode::Struct(Rc::new(StructExpr {
                list: vec![StructEntry {
                    sym: "len".to_string(),
                    expr: TypedExpression::integer(123),
                }],
            })),
        }
    }
}
