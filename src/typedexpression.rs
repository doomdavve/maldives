use std::collections::HashMap;
use std::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::resolvedtype::{ResolvedFunctionType, ResolvedType};
use crate::symboltable::SymbolTable;
use crate::typedexpressionnode::*;
use sdl2::{render::Canvas, video::Window, EventPump, Sdl, VideoSubsystem};

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpression {
    pub resolved_type: ResolvedType,
    pub node: TypedExpressionNode,
}

impl fmt::Display for TypedExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl TypedExpression {
    pub fn resolved_type(resolved_type: ResolvedType) -> TypedExpression {
        TypedExpression {
            resolved_type,
            node: TypedExpressionNode::Void,
        }
    }
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

    pub fn array(array_type: ResolvedType, v: Vec<TypedExpression>) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Array(Box::new(array_type)),
            node: TypedExpressionNode::Array(Rc::new(TypedArrayExpr { array: v })),
        }
    }

    pub fn function(
        sym: Option<String>,
        id: u32,
        function_type: ResolvedType,
        parameters: Vec<(String, ResolvedType)>,
        expr: TypedExpression,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type: function_type,
            node: TypedExpressionNode::Function(Rc::new(TypedFunctionExpr {
                sym,
                id,
                parameters,
                expr,
            })),
        }
    }

    pub fn binding(instance: TypedExpression, origin: TypedExpression) -> TypedExpression {
        TypedExpression {
            resolved_type: origin.resolved_type.clone(),
            node: TypedExpressionNode::Binding(Rc::new(BindingExpr { instance, origin })),
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
            env: &mut SymbolTable,
            arguments: &Vec<TypedExpression>,
            type_arguments: &Option<Vec<ResolvedType>>,
        ) -> Result<TypedExpression, String>,
        parameters: Vec<ResolvedType>,
        return_type: ResolvedType,
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

    pub fn r#struct(id: u32, data: Vec<StructEntry>) -> TypedExpression {
        let mut members: HashMap<String, TypedExpression> = HashMap::new();
        for entry in data {
            members.insert(entry.sym, entry.expr);
        }
        TypedExpression {
            resolved_type: ResolvedType::Struct(id),
            node: TypedExpressionNode::Struct(Rc::new(StructExpr { members })),
        }
    }

    pub fn access(
        expr: TypedExpression,
        sym: String,
        resolved_type: ResolvedType,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type,
            node: TypedExpressionNode::Access(Rc::new(TypedAccessExpr { expr, sym })),
        }
    }

    pub fn sdl(
        sdl_context: Sdl,
        video_subsystem: VideoSubsystem,
        canvas: Canvas<Window>,
        event_pump: EventPump,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Sdl,
            node: TypedExpressionNode::Sdl(Rc::new(RefCell::new(SdlWrapper {
                sdl_context,
                video_subsystem,
                canvas,
                event_pump,
            }))),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StructEntry {
    pub sym: String,
    pub expr: TypedExpression,
}

impl StructEntry {
    pub fn new(sym: &str, expr: TypedExpression) -> StructEntry {
        StructEntry {
            sym: String::from(sym),
            expr,
        }
    }
}

impl fmt::Display for StructEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}={}", self.sym, self.expr)
    }
}
