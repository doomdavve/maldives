use std::fmt;
use std::rc::Rc;

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

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub return_type: Rc<TypeDeclaration>,
    pub parameters: Vec<TypeDeclaration>,
}

#[derive(Debug, PartialEq)]
pub enum TypeDeclaration {
    Symbol(String),
    Function(FunctionDeclaration),
}

#[derive(Debug, PartialEq)]
pub struct BindExpr {
    pub sym: String,
    pub sym_type: Option<TypeDeclaration>,
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
    pub return_type: TypeDeclaration,
    pub parameters: Vec<(String, TypeDeclaration)>,
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub list: Vec<Expression>,
}

pub struct NativeFunctionExpr {
    pub function: fn(e: &Expression) -> Result<Expression, String>,
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
