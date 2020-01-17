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
    next_function_id: u32,
    function_map: HashMap<u32, Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            map: HashMap::new(),
            next_function_id: 0,
            function_map: HashMap::new(),
        }
    }

    pub fn bind(&mut self, symbol: String, closure: Closure) {
        self.map.insert(symbol, closure);
    }

    pub fn update(&mut self, symbol: String, closure: Closure) -> Option<Closure> {
        if self.map.contains_key(&symbol) {
            self.map.insert(symbol, closure)
        } else {
            None
        }
    }

    pub fn lookup(&self, symbol: &String) -> Option<&Closure> {
        self.map.get(symbol)
    }

    pub fn set_function_id(&mut self, function_id: u32) {
        self.next_function_id = function_id
    }

    pub fn get_next_function_id(&mut self) -> u32 {
        self.next_function_id
    }
}
