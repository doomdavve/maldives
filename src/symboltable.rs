use std::collections::HashMap;

use crate::expression::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub expr: Expression,
    pub env: Option<SymbolTable>,
}

impl Closure {
    pub fn simple(expr: Expression) -> Closure {
        Closure { expr, env: None }
    }
    pub fn complete(expr: Expression, env: Option<SymbolTable>) -> Closure {
        Closure { expr, env: env }
    }
}

pub type SymbolTable = HashMap<String, Closure>;

