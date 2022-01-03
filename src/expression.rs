use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Integer(i32),
    Bool(bool),
    Function(Rc<FunctionExpr>),
    Binary(Rc<BinaryExpr>),
    FunctionCall(Rc<FunctionCallExpr>),
    Bind(Rc<BindExpr>),
    Block(Rc<BlockExpr>),
    Loop(Rc<LoopExpr>),
    Program(Rc<BlockExpr>),
    Group(Rc<GroupExpr>),
    Symbol(String),
    String(String),
    Conditional(Rc<ConditionalExpr>),
    Break(Rc<BreakExpr>),
    TypeQualified(Rc<TypeQualifiedExpressionExpr>),
    Access(Rc<AccessExpr>),
    Void,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operator {
    Sum,
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
    Call,
    TypeArguments,
    Member,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub return_type: Rc<TypeDeclaration>,
    pub parameters: Vec<TypeDeclaration>,
}

#[derive(Debug, PartialEq, Clone)]
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
    pub operator: Operator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionExpr {
    pub sym: Option<String>,
    pub return_type: Option<TypeDeclaration>,
    pub parameters: Vec<(String, TypeDeclaration)>,
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct BlockExpr {
    pub list: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct LoopExpr {
    pub list: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct BreakExpr {
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct TypeQualifiedExpressionExpr {
    pub expr: Expression,
    pub type_arguments: Vec<TypeDeclaration>,
}

#[derive(Debug, PartialEq)]
pub struct AccessExpr {
    pub expr: Expression,
    pub sym: String,
}
