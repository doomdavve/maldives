use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::convert::From;
use std::fmt;
use std::rc::Rc;

use crate::resolvedtype::ResolvedType;
use crate::typedexpression::TypedExpression;
use crate::typedexpressionnode::StructExpr;
use crate::typedexpressionnode::TypedExpressionNode;

#[derive(Debug)]
pub struct Scope {
    pub map: HashMap<String, TypedExpression>,
}

#[derive(Debug, Copy, Clone)]
pub struct NodeId(usize);

impl From<usize> for NodeId {
    fn from(item: usize) -> Self {
        NodeId(item)
    }
}

impl From<NodeId> for usize {
    fn from(item: NodeId) -> usize {
        item.0
    }
}

#[derive(Debug)]
struct Node {
    parent: Option<NodeId>,
    next: Option<NodeId>,
    prev: Option<NodeId>,
    first_child: Option<NodeId>,
    last_child: Option<NodeId>,

    scope: Scope,
}

impl Node {
    fn new(scope: Scope) -> Node {
        Node {
            parent: None,
            next: None,
            prev: None,
            first_child: None,
            last_child: None,

            scope,
        }
    }

    fn append_child(pool: &mut Pool, node_id: NodeId, child_node_id: NodeId) {
        let old_last_child: Option<NodeId>;

        {
            let node = pool.get_mut(node_id).unwrap();
            match node.first_child {
                None => node.first_child = Some(child_node_id),
                Some(_) => (),
            }

            old_last_child = node.last_child;
            node.last_child = Some(child_node_id);
        }

        {
            let child = pool.get_mut(child_node_id).unwrap();
            child.parent = Some(node_id);
            if let Some(id) = old_last_child {
                child.prev = Some(id)
            }
        }

        if let Some(id) = old_last_child {
            let prev_child = pool.get_mut(id).unwrap();
            prev_child.next = Some(child_node_id)
        }
    }
}

#[derive(Debug)]
struct Pool {
    items: Vec<Node>,
}

impl Pool {
    fn new() -> Pool {
        Pool {
            items: Vec::with_capacity(1),
        }
    }

    fn push(&mut self, node: Node) -> NodeId {
        let next_id = NodeId::from(self.items.len());
        self.items.push(node);
        next_id
    }

    fn get_mut(&mut self, node_id: NodeId) -> Option<&mut Node> {
        self.items.get_mut(usize::from(node_id))
    }

    fn get(&self, node_id: NodeId) -> Option<&Node> {
        self.items.get(usize::from(node_id))
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pool: Pool,
    scope_id: NodeId,
    next_function_id: u32,
    function_map: HashMap<u32, NodeId>,
    struct_map: HashMap<u32, Rc<StructExpr>>,
    next_struct_id: u32,
}

pub struct NotFound;

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let mut pool: Pool = Pool::new();
        let scope_id = pool.push(Node::new(Scope {
            map: HashMap::new(),
        }));
        SymbolTable {
            pool,
            scope_id,
            next_function_id: 0,
            next_struct_id: 1024, // The first ones are reserved for native types.
            function_map: HashMap::new(),
            struct_map: HashMap::new(),
        }
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        let node = self.pool.get_mut(self.scope_id).unwrap(); // We might as well panic here
        &mut node.scope
    }

    pub fn bind(&mut self, symbol: String, expr: TypedExpression) {
        if let (ResolvedType::Struct(i), TypedExpressionNode::Struct(s)) =
            (&expr.resolved_type, &expr.node)
        {
            self.struct_map.insert(*i, s.clone());
        }

        let scope = self.current_scope_mut();
        scope.map.insert(symbol, expr);
    }

    pub fn bind_function(&mut self, expr: TypedExpression) {
        if let TypedExpressionNode::Function(f) = &expr.node {
            self.function_map.insert(f.id, self.scope_id);
        }
    }

    fn update_recursive(
        &mut self,
        symbol: String,
        expr: TypedExpression,
        scope_id: NodeId,
    ) -> Result<(), NotFound> {
        let node = self.pool.get_mut(scope_id).unwrap(); // We might as well panic here
        let scope = &mut node.scope;

        match scope.map.entry(symbol.clone()) {
            Entry::Occupied(mut v) => {
                v.insert(expr);
                Ok(())
            }
            Entry::Vacant(_) => match node.parent {
                Some(parent_scope_id) => self.update_recursive(symbol, expr, parent_scope_id),

                None => Err(NotFound),
            },
        }
    }

    pub fn update(&mut self, symbol: String, expr: TypedExpression) -> Result<(), NotFound> {
        self.update_recursive(symbol, expr, self.scope_id)
    }

    fn lookup_recursive(&self, symbol: &str, scope_id: NodeId) -> Option<&TypedExpression> {
        let node = self.pool.get(scope_id).unwrap(); // We might as well panic here
        let scope = &node.scope;
        if scope.map.contains_key(symbol) {
            scope.map.get(symbol)
        } else {
            match node.parent {
                Some(parent_scope_id) => self.lookup_recursive(symbol, parent_scope_id),
                None => None,
            }
        }
    }

    pub fn lookup(&self, symbol: &str) -> Option<&TypedExpression> {
        self.lookup_recursive(symbol, self.scope_id)
    }

    pub fn enter_function(&mut self, function_id: u32) -> NodeId {
        let old_scope_id = self.scope_id;
        self.scope_id = *self.function_map.get(&function_id).unwrap();
        old_scope_id
    }

    pub fn leave_function(&mut self, scope_id: NodeId) {
        self.scope_id = scope_id;
    }

    pub fn function_id(&mut self) -> u32 {
        let function_id = self.next_function_id;
        self.next_function_id += 1;
        function_id
    }

    #[allow(dead_code)]
    pub fn struct_id(&mut self) -> u32 {
        let struct_id = self.next_struct_id;
        self.next_struct_id += 1;
        struct_id
    }

    pub fn enter_scope(&mut self) {
        let new_scope_id = self.pool.push(Node::new(Scope {
            map: HashMap::new(),
        }));
        Node::append_child(&mut self.pool, self.scope_id, new_scope_id);
        self.scope_id = new_scope_id;
    }

    pub fn leave_scope(&mut self) {
        let node = self.pool.get_mut(self.scope_id).unwrap();
        self.scope_id = node.parent.unwrap();
    }

    pub fn lookup_struct(&self, id: u32) -> Option<&Rc<StructExpr>> {
        self.struct_map.get(&id)
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "SymbolTable")?;

        let node = self.pool.get(self.scope_id).unwrap(); // We might as well panic here
        let scope = &node.scope;

        for (key, value) in &scope.map {
            println!("{}: {}", key, value);
        }

        Ok(())
    }
}
