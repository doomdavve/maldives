use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum PrimitiveType {
    Integer,
    Bool,
    String,
    Function,
    Any,
    None
}

impl FromStr for PrimitiveType {
    type Err = ();

    fn from_str(s: &str) -> Result<PrimitiveType, ()> {
        match s {
            "int" => Ok(PrimitiveType::Integer),
            "bool" => Ok(PrimitiveType::Bool),
            "string" => Ok(PrimitiveType::String),
            "fn" => Ok(PrimitiveType::Function),
            "any" => Ok(PrimitiveType::Any),
            "none" => Ok(PrimitiveType::None),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Integer(i32),
    Bool(bool),
    Function(Rc<FunctionExpr>),
    NativeFunction(Rc<NativeFunctionExpr>),
    Binary(Rc<BinaryExpr>),
    FunctionCall(Rc<FunctionCallExpr>),
    Bind(Rc<BindExpr>),
    Block(Rc<BlockExpr>),
    Group(Rc<GroupExpr>),
    Symbol(String),
    String(String),
    Conditional(Rc<ConditionalExpr>),
    Void,
}

impl Expression {
    // Should we pass in the symbol table here? Which one? :)
    pub fn resolve_type(&self) -> PrimitiveType {
        match self {
            Expression::Integer(_) => PrimitiveType::Integer,
            Expression::String(_) => PrimitiveType::String,
            Expression::Bool(_) => PrimitiveType::Bool,
            Expression::Function(_) => PrimitiveType::Function,
            Expression::Symbol(_) => PrimitiveType::Any, // Need the symbol table to tell
            Expression::Bind(bind) => bind.expr.resolve_type(),
            Expression::Block(block) => block.list.last().map(|e| e.resolve_type()).unwrap_or(PrimitiveType::None),
            Expression::FunctionCall(fc) => fc.expr.resolve_type(), // TODO: here we have work to do.
            Expression::Binary(binary) => binary.operation.resolve_type(&binary.left, &binary.right),
            Expression::Group(group) => group.expr.resolve_type(),
            Expression::Conditional(_) => PrimitiveType::Any, // Check both braches and assert they are the same
            Expression::NativeFunction(_) => PrimitiveType::Any, // One more thing left to do...
            Expression::Void => PrimitiveType::None
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperation {
    Sum,
    Difference,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessEqualThan,
    GreaterEqualThan,
}

impl BinaryOperation {
    // Should we pass in the symbol table here? Which one? :)
    pub fn resolve_type(&self, left: &Expression, right: &Expression) -> PrimitiveType {
        match self {
            BinaryOperation::Sum => if left.resolve_type() == right.resolve_type() {
                left.resolve_type()
            } else {
                PrimitiveType::Any
            }
            BinaryOperation::Difference | BinaryOperation::Multiply | BinaryOperation::Divide => PrimitiveType::Integer,
            BinaryOperation::LessThan | BinaryOperation::GreaterThan | BinaryOperation::LessEqualThan | BinaryOperation::GreaterEqualThan => PrimitiveType::Bool
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BindExpr {
    pub sym: String,
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct GroupExpr {
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallExpr {
    pub expr: Expression,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct ConditionalExpr {
    pub condition: Expression,
    pub true_branch: Expression,
    pub false_branch: Option<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub operation: BinaryOperation,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionExpr {
    pub sym: Option<String>,
    pub parameters: Vec<String>,
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub list: Vec<Expression>,
}

pub struct NativeFunctionExpr {
    pub function: fn(e: &Expression) -> Result<Expression, String>
}

impl PartialEq for NativeFunctionExpr {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl fmt::Debug for NativeFunctionExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Native function")
    }
}

