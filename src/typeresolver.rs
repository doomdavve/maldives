use std::error;
use std::fmt;
use std::rc::Rc;

use crate::expression::{BinaryOperation, Expression, LoopExpr};
use crate::resolvedtype::ResolvedType;
use crate::symboltable::SymbolTable;
use crate::typedexpression::{TypedBinaryOperation, TypedExpression};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeResolverError {
    pub message: String,
}

impl fmt::Display for TypeResolverError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type resolve error: {}", self.message)
    }
}

impl error::Error for TypeResolverError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl TypeResolverError {
    fn new(message: String) -> TypeResolverError {
        TypeResolverError { message }
    }
}

pub struct TypeResolver;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeTable {
    map: HashMap<String, ResolvedType>,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        TypeTable {
            map: HashMap::new(),
        }
    }

    pub fn bind(&mut self, symbol: String, resolve_type: ResolvedType) {
        self.map.insert(symbol, resolve_type);
    }

    pub fn lookup(&self, symbol: &String) -> Option<&ResolvedType> {
        self.map.get(symbol)
    }
}

struct ResolveContext {
    loop_expr: Option<Rc<LoopExpr>>,
    parent: Option<Rc<ResolveContext>>,
}

impl ResolveContext {
    fn new() -> ResolveContext {
        ResolveContext {
            loop_expr: None,
            parent: None,
        }
    }
    fn r#loop(ctx: Rc<ResolveContext>, loop_expr: &Rc<LoopExpr>) -> Rc<ResolveContext> {
        Rc::new(ResolveContext {
            loop_expr: Some(loop_expr.clone()),
            parent: Some(ctx.clone()),
        })
    }
}

impl TypeResolver {
    pub fn resolve_in_env(
        expression: &Expression,
        env: &SymbolTable,
    ) -> Result<TypedExpression, TypeResolverError> {
        let mut type_table = TypeTable::new();
        for (key, value) in &env.map {
            type_table.bind(key.clone(), value.expr.resolved_type.clone())
        }
        TypeResolver::resolve(expression, &mut type_table, &Rc::new(ResolveContext::new()))
    }

    fn resolve(
        expression: &Expression,
        env: &mut TypeTable,
        ctx: &Rc<ResolveContext>,
    ) -> Result<TypedExpression, TypeResolverError> {
        match expression {
            Expression::Void => Ok(TypedExpression::void()),
            Expression::Integer(i) => Ok(TypedExpression::integer(*i)),
            Expression::Bool(b) => Ok(TypedExpression::bool(*b)),
            Expression::String(s) => Ok(TypedExpression::string(s.to_string())),
            Expression::Group(group) => {
                let typed_group = TypeResolver::resolve(&group.expr, env, ctx)?;
                Ok(TypedExpression::group(typed_group))
            }
            Expression::Symbol(s) => {
                let resolved_type = env.lookup(s).ok_or(TypeResolverError::new(format!(
                    "Unable to resolve type of symbol '{}'",
                    s
                )))?;
                Ok(TypedExpression::symbol(
                    String::from(s),
                    resolved_type.clone(),
                ))
            }
            Expression::Binary(b) => {
                let left = TypeResolver::resolve(&b.left, env, ctx)?;
                let right = TypeResolver::resolve(&b.right, env, ctx)?;
                match (
                    b.operation,
                    left.resolved_type.clone(),
                    right.resolved_type.clone(),
                ) {
                    (BinaryOperation::Sum, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Sum,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }
                    (BinaryOperation::Sum, ResolvedType::String, ResolvedType::String) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Concat,
                            ResolvedType::String,
                            left,
                            right,
                        ))
                    }
                    (BinaryOperation::Difference, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Difference,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }
                    (BinaryOperation::Multiply, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Multiply,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }
                    (
                        BinaryOperation::ToThePowerOf,
                        ResolvedType::Integer,
                        ResolvedType::Integer,
                    ) => Ok(TypedExpression::binary_operation(
                        TypedBinaryOperation::ToThePowerOf,
                        ResolvedType::Integer,
                        left,
                        right,
                    )),
                    (BinaryOperation::Divide, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Divide,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }

                    (BinaryOperation::LessThan, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::LessThan,
                            ResolvedType::Bool,
                            left,
                            right,
                        ))
                    }
                    (
                        BinaryOperation::LessEqualThan,
                        ResolvedType::Integer,
                        ResolvedType::Integer,
                    ) => Ok(TypedExpression::binary_operation(
                        TypedBinaryOperation::LessEqualThan,
                        ResolvedType::Bool,
                        left,
                        right,
                    )),
                    (
                        BinaryOperation::GreaterThan,
                        ResolvedType::Integer,
                        ResolvedType::Integer,
                    ) => Ok(TypedExpression::binary_operation(
                        TypedBinaryOperation::GreaterThan,
                        ResolvedType::Bool,
                        left,
                        right,
                    )),
                    (
                        BinaryOperation::GreaterEqualThan,
                        ResolvedType::Integer,
                        ResolvedType::Integer,
                    ) => Ok(TypedExpression::binary_operation(
                        TypedBinaryOperation::GreaterEqualThan,
                        ResolvedType::Bool,
                        left,
                        right,
                    )),
                    (BinaryOperation::Equal, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Equal,
                            ResolvedType::Bool,
                            left,
                            right,
                        ))
                    }
                    (BinaryOperation::Assign, _, _) => {
                        if left.resolved_type == right.resolved_type
                            || left.resolved_type == ResolvedType::Any
                        {
                            Ok(TypedExpression::binary_operation(
                                TypedBinaryOperation::Assign,
                                right.resolved_type.clone(),
                                left,
                                right,
                            ))
                        } else {
                            Err(TypeResolverError::new(format!(
                                "type mismatch in assignment",
                            )))
                        }
                    }
                    _ => Err(TypeResolverError::new(format!("dbg: {:?}", expression))),
                }
            }
            Expression::Conditional(c) => {
                let condition = TypeResolver::resolve(&c.condition, env, ctx)?;
                let true_branch = TypeResolver::resolve(&c.true_branch, env, ctx)?;
                let false_branch = match c.false_branch.clone() {
                    Some(false_branch) => Some(TypeResolver::resolve(&false_branch, env, ctx)?),
                    None => None,
                };
                let true_branch_resolved_type = true_branch.resolved_type.clone();
                let false_branch_resolved_type = false_branch
                    .clone()
                    .and_then(|expr| Some(expr.resolved_type))
                    .unwrap_or(true_branch.resolved_type.clone());
                if condition.resolved_type == ResolvedType::Bool
                    && true_branch_resolved_type == false_branch_resolved_type
                {
                    Ok(TypedExpression::conditional(
                        condition,
                        true_branch_resolved_type,
                        true_branch.clone(),
                        false_branch.clone(),
                    ))
                } else {
                    Err(TypeResolverError::new(
                        "Conditional must operate on boolean and both branches must return the same type".to_string(),
                    ))
                }
            }
            Expression::Block(b) => {
                let mut list = Vec::new();
                for expr in &b.list {
                    list.push(TypeResolver::resolve(&expr, env, ctx)?);
                }
                Ok(TypedExpression::block(list))
            }
            Expression::Break(b) => match &ctx.loop_expr {
                Some(_) => {
                    let parent_ctx = ctx
                        .parent
                        .clone()
                        .ok_or(TypeResolverError::new("internal error".to_string()))?;
                    let typed_break = TypeResolver::resolve(&b.expr, env, &parent_ctx)?;
                    Ok(TypedExpression::r#break(typed_break))
                }
                None => Err(TypeResolverError::new("missing loop context".to_string())),
            },
            Expression::Loop(b) => {
                let resolve_context = ResolveContext::r#loop(ctx.clone(), b);

                let mut loop_break_type = ResolvedType::Never;
                let mut list = Vec::new();
                for expr in &b.list {
                    let expr = TypeResolver::resolve(&expr, env, &resolve_context)?;
                    match &expr.resolved_type {
                        ResolvedType::Break(break_type) => {
                            if loop_break_type == ResolvedType::Never {
                                loop_break_type = *break_type.clone();
                                Ok(())
                            } else {
                                Err(TypeResolverError::new(
                                    "mismatched break expression types".to_string(),
                                ))
                            }
                        }
                        _ => Ok(()),
                    }?;
                    list.push(expr);
                }
                Ok(TypedExpression::r#loop(list, loop_break_type))
            }
            Expression::Program(program) => {
                let mut list = Vec::new();
                for expr in &program.list {
                    list.push(TypeResolver::resolve(&expr, env, ctx)?);
                }
                Ok(TypedExpression::program(list))
            }
            Expression::Bind(bind) => {
                let expr = TypeResolver::resolve(&bind.expr, env, ctx)?;
                let resolved_sym_type: Option<ResolvedType> = match bind.sym_type.clone() {
                    Some(decl) => ResolvedType::from_decl(&decl),
                    None => Some(expr.resolved_type.clone()),
                };
                if resolved_sym_type == Some(expr.resolved_type.clone()) {
                    env.bind(String::from(&bind.sym), expr.resolved_type.clone());
                    Ok(TypedExpression::bind(String::from(&bind.sym), expr))
                } else {
                    Err(TypeResolverError::new(
                        format!(
                            "Type mismatch: Can't bind symbol '{}' of type '{:?}' to expression of type {:?}",
                            &bind.sym, resolved_sym_type, expr.resolved_type.clone()
                        ),
                    ))
                }
            }
            Expression::Function(f) => {
                let mut parameters: Vec<(String, ResolvedType)> = Vec::new();
                let mut types: Vec<ResolvedType> = Vec::new();

                let mut function_env = env.clone();
                for parameter in &f.parameters {
                    let resolved_parameter_type = ResolvedType::from_decl(&parameter.1)
                        .ok_or_else(|| {
                            TypeResolverError::new(format!(
                                "Type mismatch: Can't bind resolve type"
                            ))
                        })?;
                    parameters.push((parameter.0.clone(), resolved_parameter_type.clone()));
                    function_env.bind(String::from(&parameter.0), resolved_parameter_type.clone());
                    types.push(resolved_parameter_type)
                }

                let expr = TypeResolver::resolve(&f.expr, &mut function_env, ctx)?;
                let return_type = &expr.resolved_type;

                let specified_return_type: ResolvedType = match f.return_type.as_ref() {
                    Some(decl) => ResolvedType::from_decl(decl).ok_or(TypeResolverError::new(
                        format!("Can't resolve specified return type '{:?}'", decl),
                    )),
                    None => Ok(ResolvedType::Any),
                }?;

                let type_check: bool = specified_return_type == ResolvedType::Any
                    || &specified_return_type == return_type;

                if type_check {
                    let resolved_type = ResolvedType::function(return_type.clone(), types);
                    let function = TypedExpression::function(
                        f.sym.clone(),
                        resolved_type.clone(),
                        parameters,
                        expr.clone(),
                    );
                    if let Some(sym) = &f.sym {
                        env.bind(String::from(sym), resolved_type.clone());
                    }
                    Ok(function)
                } else {
                    Err(TypeResolverError::new(format!(
                        "Type mismatch: Return types does not match"
                    )))
                }
            }

            Expression::FunctionCall(fc) => {
                let expr = TypeResolver::resolve(&fc.expr, env, ctx)?;
                let return_type = match &expr.resolved_type {
                    ResolvedType::Function(f) => {
                        let mut typed_arguments = Vec::<TypedExpression>::new();
                        for arg in &fc.arguments {
                            let typed_arg = TypeResolver::resolve(&arg, env, ctx)?;
                            typed_arguments.push(typed_arg)
                        }

                        let mut mismatch = typed_arguments.len() != f.parameters.len();
                        if !mismatch {
                            for (arg, param) in typed_arguments.iter().zip(&f.parameters) {
                                if param != &ResolvedType::Any && &arg.resolved_type != param {
                                    mismatch = true;
                                    break;
                                }
                            }
                        }

                        if !mismatch {
                            Ok(f.return_type.clone())
                        } else {
                            Err(TypeResolverError::new(format!(
                                "Type mismatch: argument mismatch"
                            )))
                        }
                    }
                    _ => Err(TypeResolverError::new(format!(
                        "Type mismatch: Attempt to call something not a function: {:?}",
                        expr.node
                    ))),
                }?;

                let mut arguments: Vec<TypedExpression> = Vec::new();
                for argument in &fc.arguments {
                    let expr = TypeResolver::resolve(argument, env, ctx)?;
                    arguments.push(expr)
                }

                Ok(TypedExpression::call(
                    expr.clone(),
                    return_type.clone(),
                    arguments,
                ))
            }
        }
    }
}
