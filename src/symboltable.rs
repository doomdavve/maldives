use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure<T> {
    pub expr: T,
    pub env: Option<SymbolTable<T>>,
}

impl<T> Closure<T> {
    pub fn simple(expr: T) -> Closure<T> {
        Closure { expr, env: None }
    }
    pub fn complete(expr: T, env: Option<SymbolTable<T>>) -> Closure<T> {
        Closure { expr, env }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable<T> {
    map: HashMap<String, Closure<T>>,
}

impl<T> SymbolTable<T> {
    pub fn new() -> SymbolTable<T> {
        SymbolTable::<T> {
            map: HashMap::new(),
        }
    }

    pub fn bind(&mut self, symbol: String, closure: Closure<T>) {
        self.map.insert(symbol, closure);
    }

    pub fn lookup(&self, symbol: &String) -> Option<&Closure<T>> {
        self.map.get(symbol)
    }
}
