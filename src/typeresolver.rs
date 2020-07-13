use std::error;
use std::fmt;
use std::rc::Rc;

use crate::expression::{Expression, LoopExpr, Operator};
use crate::resolvedtype::ResolvedType;
use crate::symboltable::SymbolTable;
use crate::typedexpression::TypedExpression;
use crate::typedexpressionnode::TypedBinaryOperation;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type resolve error: {}", self.message)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl Error {
    fn new(message: String) -> Error {
        Error { message }
    }
}

pub struct TypeResolver;

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
        env: &mut SymbolTable,
    ) -> Result<TypedExpression, Error> {
        let root = TypeResolver::resolve(expression, env, &Rc::new(ResolveContext::new()))?;
        Ok(root)
    }

    fn resolve(
        expression: &Expression,
        env: &mut SymbolTable,
        ctx: &Rc<ResolveContext>,
    ) -> Result<TypedExpression, Error> {
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
                let expr = env.lookup(s).ok_or(Error::new(format!(
                    "Unable to resolve type of symbol '{}'",
                    s
                )))?;
                Ok(TypedExpression::symbol(
                    String::from(s),
                    expr.resolved_type.clone(),
                ))
            }
            Expression::Access(a) => {
                let struct_expr = TypeResolver::resolve(&a.expr, env, ctx)?;
                let struct_id = struct_expr
                    .resolved_type
                    .struct_id()
                    .map_err(|e| Error::new(e))?;
                let s = env
                    .lookup_struct(struct_id)
                    .ok_or(Error::new(format!("unexpected error3")))?;
                match &s.members.get(&a.sym) {
                    Some(m) => Ok(TypedExpression::access(
                        struct_expr.clone(),
                        a.sym.clone(),
                        m.resolved_type.clone(),
                    )),
                    _ => Err(Error::new(format!(
                        "unexpected {}, expected structure",
                        struct_expr.resolved_type
                    ))),
                }
            }
            Expression::Binary(b) => {
                let left = TypeResolver::resolve(&b.left, env, ctx)?;
                let right = TypeResolver::resolve(&b.right, env, ctx)?;
                match (
                    b.operator,
                    left.resolved_type.clone(),
                    right.resolved_type.clone(),
                ) {
                    (Operator::Sum, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Sum,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }
                    (Operator::Sum, ResolvedType::String, ResolvedType::String) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Concat,
                            ResolvedType::String,
                            left,
                            right,
                        ))
                    }
                    (Operator::Difference, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Difference,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }
                    (Operator::Multiply, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Multiply,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }
                    (Operator::ToThePowerOf, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::ToThePowerOf,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }
                    (Operator::Divide, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Divide,
                            ResolvedType::Integer,
                            left,
                            right,
                        ))
                    }

                    (Operator::LessThan, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::LessThan,
                            ResolvedType::Bool,
                            left,
                            right,
                        ))
                    }
                    (Operator::LessEqualThan, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::LessEqualThan,
                            ResolvedType::Bool,
                            left,
                            right,
                        ))
                    }
                    (Operator::GreaterThan, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::GreaterThan,
                            ResolvedType::Bool,
                            left,
                            right,
                        ))
                    }
                    (Operator::GreaterEqualThan, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::GreaterEqualThan,
                            ResolvedType::Bool,
                            left,
                            right,
                        ))
                    }
                    (Operator::Equal, ResolvedType::Integer, ResolvedType::Integer) => {
                        Ok(TypedExpression::binary_operation(
                            TypedBinaryOperation::Equal,
                            ResolvedType::Bool,
                            left,
                            right,
                        ))
                    }
                    (Operator::Assign, _, _) => {
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
                            Err(Error::new(format!("type mismatch in assignment",)))
                        }
                    }
                    _ => Err(Error::new(format!(
                        "operator {} can't be applied to type '{}' and type '{}'",
                        b.operator,
                        left.resolved_type.clone(),
                        right.resolved_type.clone()
                    ))),
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
                    Err(Error::new(
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
                        .ok_or(Error::new("internal error".to_string()))?;
                    let typed_break = TypeResolver::resolve(&b.expr, env, &parent_ctx)?;
                    Ok(TypedExpression::r#break(typed_break))
                }
                None => Err(Error::new("missing loop context".to_string())),
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
                                Err(Error::new("mismatched break expression types".to_string()))
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
                    env.bind(String::from(&bind.sym), expr.clone());
                    Ok(TypedExpression::bind(String::from(&bind.sym), expr))
                } else {
                    Err(Error::new(
                        format!(
                            "Type mismatch: Can't bind symbol '{}' of type '{:?}' to expression of type {:?}",
                            &bind.sym, resolved_sym_type, expr.resolved_type.clone()
                        ),
                    ))
                }
            }
            Expression::TypeQualifiedExpression(qf) => {
                let expr = TypeResolver::resolve(&qf.expr, env, ctx)?;
                let type_arguments = &qf.type_arguments;

                let mut typed_type_arguments: Vec<ResolvedType> =
                    Vec::with_capacity(qf.type_arguments.len());
                for type_argument in &qf.type_arguments {
                    let a = ResolvedType::from_decl(type_argument).ok_or(Error::new(format!(
                        "can't resolve type: {:?}",
                        type_argument
                    )))?;
                    let b = ResolvedType::complete_type(&a, type_arguments)
                        .ok_or(Error::new(format!("can't resolve type: {:?}", a)))?;
                    typed_type_arguments.push(b)
                }
                let resolved_return_type =
                    ResolvedType::complete_type(&expr.resolved_type, type_arguments).ok_or(
                        Error::new(format!(
                            "can't resolve return type: {:?}",
                            expr.resolved_type
                        )),
                    )?;

                Ok(TypedExpression::type_qualified_expression(
                    expr,
                    resolved_return_type,
                    typed_type_arguments,
                ))
            }
            Expression::Function(f) => {
                let mut parameters: Vec<(String, ResolvedType)> = Vec::new();
                let mut types: Vec<ResolvedType> = Vec::new();

                env.enter_scope();
                for parameter in &f.parameters {
                    let resolved_parameter_type = ResolvedType::from_decl(&parameter.1)
                        .ok_or_else(|| {
                            Error::new(format!("Type mismatch: Can't bind resolve type"))
                        })?;
                    parameters.push((parameter.0.clone(), resolved_parameter_type.clone()));
                    env.bind(
                        String::from(&parameter.0),
                        TypedExpression::resolved_type(resolved_parameter_type.clone()),
                    );
                    types.push(resolved_parameter_type)
                }

                let expr = TypeResolver::resolve(&f.expr, env, ctx)?;
                // FIXME: Do this leave through Drop in order to not stay in entered scope.
                env.leave_scope();

                let return_type = &expr.resolved_type;

                let specified_return_type: ResolvedType = match f.return_type.as_ref() {
                    Some(decl) => ResolvedType::from_decl(decl).ok_or(Error::new(format!(
                        "Can't resolve specified return type '{:?}'",
                        decl
                    ))),
                    None => Ok(ResolvedType::Any),
                }?;

                let type_match: bool = specified_return_type == ResolvedType::Any
                    || &specified_return_type == return_type;

                if type_match {
                    let resolved_type = ResolvedType::function(return_type.clone(), types);
                    let function_id = env.function_id();
                    let function = TypedExpression::function(
                        f.sym.clone(),
                        function_id,
                        resolved_type.clone(),
                        parameters,
                        expr.clone(),
                    );
                    if let Some(sym) = &f.sym {
                        env.bind(
                            String::from(sym),
                            TypedExpression::resolved_type(resolved_type.clone()),
                        );
                    }
                    Ok(function)
                } else {
                    Err(Error::new(format!(
                        "Type mismatch: Return types does not match"
                    )))
                }
            }

            Expression::FunctionCall(fc) => {
                let expr = TypeResolver::resolve(&fc.expr, env, ctx)?;
                let return_type = match &expr.resolved_type {
                    ResolvedType::Function(f) => {
                        let is_var_args_function = match f.parameters.last() {
                            Some(param) => param == &ResolvedType::VarArgs,
                            _ => false,
                        };
                        let mut typed_arguments = Vec::<TypedExpression>::new();
                        for arg in &fc.arguments {
                            let typed_arg = TypeResolver::resolve(&arg, env, ctx)?;
                            typed_arguments.push(typed_arg)
                        }

                        let mut error_message: Option<String> = None;
                        if !is_var_args_function && typed_arguments.len() != f.parameters.len() {
                            error_message = Some(format!(
                                "expected {} arguments, got {}",
                                f.parameters.len(),
                                typed_arguments.len()
                            ));
                        }
                        if error_message.is_none() {
                            for (arg, param) in typed_arguments.iter().zip(&f.parameters) {
                                if param == &ResolvedType::VarArgs {
                                    break;
                                }
                                match (&param, &arg.resolved_type) {
                                    (ResolvedType::Array(t1), ResolvedType::Array(t2)) => {
                                        if t1.as_ref() != &ResolvedType::Any && t1 != t2 {
                                            error_message =
                                                Some(format!("Array: {} != {}", t1, t1));
                                            break;
                                        }
                                    }
                                    _ => {
                                        if param != &ResolvedType::Any
                                            && &arg.resolved_type != param
                                        {
                                            error_message = Some(format!(
                                                "expected {}, got {}",
                                                param, arg.resolved_type
                                            ));
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                        match error_message {
                            None => Ok(f.return_type.clone()),
                            Some(message) => Err(Error::new(format!(
                                "Type mismatch: argument mismatch: {:?}",
                                message
                            ))),
                        }
                    }
                    _ => Err(Error::new(format!(
                        "Type mismatch: Attempt to call something not a function: {:?}",
                        expr.resolved_type
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
