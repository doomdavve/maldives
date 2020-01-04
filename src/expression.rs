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
    Group(Rc<GroupExpr>),
    Symbol(String),
    String(String),
    Conditional(Rc<ConditionalExpr>),
    Void,
}

#[derive(Debug, PartialEq)]
pub enum Operation {
    Sum,
    Difference,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessEqualThan,
    GreaterEqualThan
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
