use std::fmt;
use std::rc::Rc;

use crate::resolvedtype::{ResolvedFunctionType, ResolvedType};

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpression {
    pub resolved_type: ResolvedType,
    pub node: TypedExpressionNode,
}

impl fmt::Display for TypedExpression {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        write!(f, "{}: {}", self.node, self.resolved_type)
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

    pub fn array(array_type: ResolvedType, v: Vec<TypedExpression>) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Array(Box::new(array_type)),
            node: TypedExpressionNode::Array(Rc::new(TypedBlockExpr { list: v })),
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
        f: fn(e: &TypedExpression) -> Result<TypedExpression, String>,
        return_type: ResolvedType,
        arg_type: ResolvedType,
        call_by_value: bool,
    ) -> TypedExpression {
        TypedExpression {
            resolved_type: ResolvedType::Function(Rc::new(ResolvedFunctionType {
                return_type,
                parameters: vec![arg_type],
            })),
            node: TypedExpressionNode::NativeFunction(Rc::new(TypedNativeFunctionExpr {
                function: f,
                call_by_value,
            })),
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
    Program(Rc<TypedBlockExpr>),
    Group(Rc<TypedGroupExpr>),
    String(String),
    Symbol(String),
    Conditional(Rc<TypedConditionalExpr>),
    Break(Rc<TypedBreakExpr>),
    Loop(Rc<TypedBlockExpr>),
    Array(Rc<TypedBlockExpr>),
    Void,
}

impl fmt::Display for TypedExpressionNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            TypedExpressionNode::Integer(v) => write!(f, "{}", v),
            TypedExpressionNode::Bool(v) => write!(f, "{}", v),
            TypedExpressionNode::Function(function) => write!(f, "{}", function),
            TypedExpressionNode::NativeFunction(native_function) => {
                write!(f, "{}", native_function)
            }
            TypedExpressionNode::BinaryOperation(binary_operation) => {
                write!(f, "{}", binary_operation)
            }
            TypedExpressionNode::FunctionCall(call) => write!(f, "{}", call),
            TypedExpressionNode::Bind(bind) => write!(f, "{}", bind),
            TypedExpressionNode::Block(block) => write!(f, "{}", block),
            TypedExpressionNode::Program(program) => write!(f, "{}", program),
            TypedExpressionNode::Group(group) => write!(f, "{}", group),
            TypedExpressionNode::String(string) => write!(f, "\"{}\"", string),
            TypedExpressionNode::Symbol(symbol) => write!(f, "{}", symbol),
            TypedExpressionNode::Conditional(conditional) => write!(f, "{}", conditional),
            TypedExpressionNode::Void => write!(f, "void"),
            TypedExpressionNode::Break(b) => write!(f, "{}", b),
            TypedExpressionNode::Loop(b) => write!(f, "{}", b),
            TypedExpressionNode::Array(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TypedBinaryOperation {
    Sum,
    Concat,
    Difference,
    Multiply,
    ToThePowerOf,
    Divide,
    LessThan,
    GreaterThan,
    LessEqualThan,
    GreaterEqualThan,
    Equal,
    Assign,
}

impl TypedBinaryOperation {
    pub fn left_hand_by_name(&self) -> bool {
        match &self {
            TypedBinaryOperation::Sum
            | TypedBinaryOperation::Concat
            | TypedBinaryOperation::Difference
            | TypedBinaryOperation::Multiply
            | TypedBinaryOperation::ToThePowerOf
            | TypedBinaryOperation::Divide
            | TypedBinaryOperation::LessThan
            | TypedBinaryOperation::GreaterThan
            | TypedBinaryOperation::LessEqualThan
            | TypedBinaryOperation::GreaterEqualThan
            | TypedBinaryOperation::Equal => false,
            TypedBinaryOperation::Assign => true,
        }
    }
}

impl fmt::Display for TypedBinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            TypedBinaryOperation::Sum => write!(f, "+"),
            TypedBinaryOperation::Concat => write!(f, "+"),
            TypedBinaryOperation::Difference => write!(f, "-"),
            TypedBinaryOperation::Multiply => write!(f, "*"),
            TypedBinaryOperation::ToThePowerOf => write!(f, "**"),
            TypedBinaryOperation::Divide => write!(f, "/"),
            TypedBinaryOperation::LessThan => write!(f, "<"),
            TypedBinaryOperation::GreaterThan => write!(f, ">"),
            TypedBinaryOperation::LessEqualThan => write!(f, "<="),
            TypedBinaryOperation::GreaterEqualThan => write!(f, ">="),
            TypedBinaryOperation::Equal => write!(f, "=="),
            TypedBinaryOperation::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedBindExpr {
    pub sym: String,
    pub expr: TypedExpression,
}

impl fmt::Display for TypedBindExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {}", self.sym)
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedGroupExpr {
    pub expr: TypedExpression,
}

impl fmt::Display for TypedGroupExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "()")
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedFunctionCallExpr {
    pub expr: TypedExpression,
    pub arguments: Vec<TypedExpression>,
}

impl fmt::Display for TypedFunctionCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}()", self.expr)
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedConditionalExpr {
    pub condition: TypedExpression,
    pub true_branch: TypedExpression,
    pub false_branch: Option<TypedExpression>,
}

impl fmt::Display for TypedConditionalExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {{}} else {{}}")
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedBinaryOperationExpr {
    pub operation: TypedBinaryOperation,
    pub left: TypedExpression,
    pub right: TypedExpression,
}

impl fmt::Display for TypedBinaryOperationExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}()", self.operation)
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedFunctionExpr {
    pub sym: Option<String>,
    pub id: u32,
    pub parameters: Vec<(String, ResolvedType)>,
    pub expr: TypedExpression,
}

impl fmt::Display for TypedFunctionExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fn {}() <{}>",
            self.sym.clone().unwrap_or("".to_string()),
            self.id
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedBlockExpr {
    pub list: Vec<TypedExpression>,
}

impl fmt::Display for TypedBlockExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{}}")
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedBreakExpr {
    pub expr: TypedExpression,
}

impl fmt::Display for TypedBreakExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "break")
    }
}

pub struct TypedNativeFunctionExpr {
    pub function: fn(e: &TypedExpression) -> Result<TypedExpression, String>,
    pub call_by_value: bool,
}

impl fmt::Display for TypedNativeFunctionExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native function>")
    }
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
