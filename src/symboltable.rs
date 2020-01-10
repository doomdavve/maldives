use std::collections::HashMap;
use crate::typedexpression::TypedExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub expr: TypedExpression,
    pub env: Option<SymbolTable>,
}

impl Closure {
    pub fn simple(expr: TypedExpression) -> Closure {
        Closure { expr, env: None }
    }
    pub fn complete(expr: TypedExpression, env: Option<SymbolTable>) -> Closure {
        Closure { expr, env }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub map: HashMap<String, Closure>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            map: HashMap::new(),
        }
    }

    pub fn bind(&mut self, symbol: String, closure: Closure) {
        self.map.insert(symbol, closure);
    }

    pub fn lookup(&self, symbol: &String) -> Option<&Closure> {
        self.map.get(symbol)
    }
}
