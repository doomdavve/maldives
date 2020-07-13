use std::collections::HashMap;
use std::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::resolvedtype::ResolvedType;
use crate::symboltable::SymbolTable;
use crate::typedexpression::TypedExpression;
use fmt::Debug;
use sdl2::{render::Canvas, video::Window, EventPump, Sdl, VideoSubsystem};

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpressionNode {
    Integer(i32),
    Bool(bool),
    Function(Rc<TypedFunctionExpr>),
    Binding(Rc<BindingExpr>),
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
    Array(Rc<TypedArrayExpr>),
    Access(Rc<TypedAccessExpr>),
    TypedTypeQualifiedExpression(Rc<TypedTypeQualifiedExpressionExpr>),
    Struct(Rc<StructExpr>),
    Sdl(Rc<RefCell<SdlWrapper>>),
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
            TypedExpressionNode::Array(a) => write!(f, "{}", a),
            TypedExpressionNode::TypedTypeQualifiedExpression(qf) => write!(f, "{}", qf),
            TypedExpressionNode::Struct(s) => write!(f, "{}", s),
            TypedExpressionNode::Access(a) => write!(f, "{}", a),
            TypedExpressionNode::Binding(b) => write!(f, "{}", b),
            TypedExpressionNode::Sdl(_sdl) => write!(f, "sdl"),
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
pub struct TypedIntArrayExpr {
    pub array: Vec<i32>,
}

impl fmt::Display for TypedIntArrayExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strings = self
            .array
            .iter()
            .map(|a| format!("{}", a))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "[{}]", strings)
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedArrayExpr {
    pub array: Vec<TypedExpression>,
}

impl fmt::Display for TypedArrayExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strings = self
            .array
            .iter()
            .map(|a| format!("{}", a))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "[{}]", strings)
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

#[derive(Clone)]
pub struct TypedNativeFunctionExpr {
    pub function: fn(
        env: &mut SymbolTable,
        e: &Vec<TypedExpression>,
        t: &Option<Vec<ResolvedType>>,
    ) -> Result<TypedExpression, String>,
    pub call_by_value: bool,
    pub type_arguments: Option<Vec<ResolvedType>>,
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

#[derive(Debug, PartialEq)]
pub struct TypedTypeQualifiedExpressionExpr {
    pub expr: TypedExpression,
    pub type_arguments: Vec<ResolvedType>,
}

impl fmt::Display for TypedTypeQualifiedExpressionExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let strings = self
            .type_arguments
            .iter()
            .map(|a| format!("{}", a))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}<{}>", self.expr, strings)
    }
}

#[derive(Debug, PartialEq)]
pub struct StructExpr {
    pub members: HashMap<String, TypedExpression>,
}

impl fmt::Display for StructExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let strings = self
            .members
            .iter()
            .map(|i| format!("{}: {}", i.0, i.1))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}", strings)
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedAccessExpr {
    pub expr: TypedExpression,
    pub sym: String,
}

impl fmt::Display for TypedAccessExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.expr, self.sym)
    }
}

#[derive(Debug, PartialEq)]
pub struct BindingExpr {
    pub instance: TypedExpression,
    pub origin: TypedExpression,
}

impl fmt::Display for BindingExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} <-> {}", self.origin, self.instance)
    }
}

pub struct SdlWrapper {
    pub sdl_context: Sdl,
    pub video_subsystem: VideoSubsystem,
    pub canvas: Canvas<Window>,
    pub event_pump: EventPump,
}

impl PartialEq for SdlWrapper {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Debug for SdlWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Sdl")
    }
}

impl fmt::Display for SdlWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Sdl")
    }
}
