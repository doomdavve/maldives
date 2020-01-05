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
            Expression::Void => PrimitiveType::None
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Operation { // call it BinaryOperation
    Sum,
    Difference,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessEqualThan,
    GreaterEqualThan,
}

impl Operation {
    // Should we pass in the symbol table here? Which one? :)
    pub fn resolve_type(&self, left: &Expression, right: &Expression) -> PrimitiveType {
        match self {
            Operation::Sum => if left.resolve_type() == right.resolve_type() {
                left.resolve_type()
            } else {
                PrimitiveType::Any
            }
            Operation::Difference | Operation::Multiply | Operation::Divide => PrimitiveType::Integer,
            Operation::LessThan | Operation::GreaterThan | Operation::LessEqualThan | Operation::GreaterEqualThan => PrimitiveType::Bool
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
    pub operation: Operation,
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
