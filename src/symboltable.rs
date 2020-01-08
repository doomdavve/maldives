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
        Closure { expr, env }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    map: HashMap<String, Closure>,
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
