use std::convert::TryInto;
use std::error;
use std::fmt;
use std::rc::Rc;

use crate::resolvedtype::AllocatedStructIds;
use crate::symboltable::SymbolTable;
use crate::typedexpression::TypedExpression;
use crate::typedexpressionnode::TypedBinaryOperation;
use crate::typedexpressionnode::TypedExpressionNode;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Interpreter error: {}", self.message)
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

pub struct Interpreter;

impl Interpreter {
    pub fn eval(expr: &TypedExpression, env: &mut SymbolTable) -> Result<TypedExpression, Error> {
        debug!("Evaluating {:?} with vars: {:?}", expr, env);
        match &expr.node {
            TypedExpressionNode::Void => Ok(TypedExpression::void()),
            TypedExpressionNode::BinaryOperation(b) => {
                let lhs = if b.operation.left_hand_by_name() {
                    b.left.clone()
                } else {
                    Interpreter::eval(&b.left, env)?
                };
                let rhs = Interpreter::eval(&b.right, env)?;
                match b.operation {
                    TypedBinaryOperation::Sum => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::integer(li + ri))
                        }
                        _ => Err(Error::new("Unexpected terms in sum operator".to_string())),
                    },
                    TypedBinaryOperation::Difference => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::integer(li - ri))
                        }
                        _ => Err(Error::new(
                            "One or more non-integer terms to difference operator".to_string(),
                        )),
                    },
                    TypedBinaryOperation::Multiply => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::integer(li * ri))
                        }
                        _ => Err(Error::new(
                            "One or more non-integer terms to multiply operator".to_string(),
                        )),
                    },
                    TypedBinaryOperation::ToThePowerOf => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::integer(li.pow(ri.try_into().unwrap())))
                        }
                        _ => Err(Error::new("Unexpected terms in power operator".to_string())),
                    },

                    TypedBinaryOperation::Divide => {
                        // TODO: handle division by zero.
                        match (lhs.node, rhs.node) {
                            (
                                TypedExpressionNode::Integer(li),
                                TypedExpressionNode::Integer(ri),
                            ) => Ok(TypedExpression::integer(li / ri)),
                            _ => Err(Error::new(
                                "One or more non-integer terms to divide operator".to_string(),
                            )),
                        }
                    }
                    TypedBinaryOperation::LessThan => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li < ri))
                        }
                        _ => Err(Error::new(
                            "One or more non-boolean terms to divide operator".to_string(),
                        )),
                    },
                    TypedBinaryOperation::GreaterThan => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li > ri))
                        }
                        _ => Err(Error::new(
                            "One or more non-boolean terms to divide operator".to_string(),
                        )),
                    },
                    TypedBinaryOperation::LessEqualThan => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li <= ri))
                        }
                        _ => Err(Error::new(
                            "One or more non-boolean terms to divide operator".to_string(),
                        )),
                    },
                    TypedBinaryOperation::GreaterEqualThan => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li >= ri))
                        }
                        _ => Err(Error::new(
                            "One or more non-boolean terms to divide operator".to_string(),
                        )),
                    },
                    TypedBinaryOperation::Concat => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::String(li), TypedExpressionNode::String(ri)) => {
                            Ok(TypedExpression::string(li + &ri))
                        }
                        _ => Err(Error::new(
                            "One or more non-string terms to concatenation operator".to_string(),
                        )),
                    },
                    TypedBinaryOperation::Equal => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li == ri))
                        }
                        _ => Err(Error::new(
                            "One or more non-integer terms to equal operator".to_string(),
                        )),
                    },
                    TypedBinaryOperation::Assign => match lhs.node {
                        TypedExpressionNode::Symbol(sym) => {
                            let expr = Interpreter::eval(&rhs, env)?;
                            env.update(String::from(&sym), expr.clone())
                                .ok_or_else(|| {
                                    Error::new(format!("{} not found in this scope", sym))
                                })?;
                            Ok(expr)
                        }
                        _ => Err(Error::new(
                            "unexpected left hand side of assignment".to_string(),
                        )),
                    },
                }
            }
            TypedExpressionNode::Conditional(c) => {
                let premise = Interpreter::eval(&c.condition, env)?;
                match (premise.node, c.false_branch.as_ref()) {
                    (TypedExpressionNode::Bool(true), _) => {
                        Ok(Interpreter::eval(&c.true_branch, env)?)
                    }
                    (TypedExpressionNode::Bool(false), Some(false_branch)) => {
                        Ok(Interpreter::eval(false_branch, env)?)
                    }
                    (TypedExpressionNode::Bool(false), _) => Ok(TypedExpression::void()),
                    _ => Err(Error::new("Unexpected result of conditional".to_string())),
                }
            }
            TypedExpressionNode::Group(g) => Interpreter::eval(&g.expr, env),
            TypedExpressionNode::Bool(b) => Ok(TypedExpression::bool(*b)),
            TypedExpressionNode::Integer(i) => Ok(TypedExpression::integer(*i)),
            TypedExpressionNode::String(s) => Ok(TypedExpression::string(s.to_string())),
            TypedExpressionNode::Array(_) => Ok(expr.clone()),
            TypedExpressionNode::Binding(_) => Ok(expr.clone()),
            TypedExpressionNode::Symbol(s) => {
                let value = env
                    .lookup(s)
                    .ok_or_else(|| Error::new(format!("unknown symbol '{}'", s)))?;
                Ok(value.clone())
            }
            TypedExpressionNode::Access(a) => {
                let val = Interpreter::eval(&a.expr, env)?;
                match &val.node {
                    TypedExpressionNode::Struct(s) => match s.members.get(&a.sym) {
                        Some(m) => Ok(m.clone()),
                        _ => Err(Error::new("stuff3".to_string())),
                    },
                    TypedExpressionNode::Array(_) => {
                        match env.lookup_struct(AllocatedStructIds::Array as u32) {
                            Some(struct_expr) => match struct_expr.members.get(&a.sym) {
                                Some(member) => Ok(TypedExpression::binding(val, member.clone())),
                                _ => Err(Error::new("stuff5".to_string())),
                            },
                            _ => Err(Error::new("stuff4".to_string())),
                        }
                    }
                    _ => Err(Error::new("stuff2".to_string())),
                }
            }
            TypedExpressionNode::Bind(b) => {
                let val = Interpreter::eval(&b.expr, env)?;
                env.bind(String::from(&b.sym), val.clone());
                Ok(val)
            }
            TypedExpressionNode::Block(b) => {
                let mut last = TypedExpression::void();
                env.enter_scope();
                for expr in &b.list {
                    last = Interpreter::eval(expr, env)?;
                    if let TypedExpressionNode::Break(_) = &last.node {
                        break;
                    }
                }
                env.leave_scope();
                Ok(last)
            }
            TypedExpressionNode::Loop(b) => {
                let mut last: Option<TypedExpression> = None;
                env.enter_scope();
                while last.is_none() {
                    for expr in &b.list {
                        let res = Interpreter::eval(expr, env)?;
                        last = match res.node {
                            TypedExpressionNode::Break(break_expr) => Some(break_expr.expr.clone()),
                            _ => None,
                        }
                    }
                }
                env.leave_scope();
                last.ok_or_else(|| Error::new("internal error".to_string()))
            }
            TypedExpressionNode::Break(b) => {
                Ok(TypedExpression::r#break(Interpreter::eval(&b.expr, env)?))
            }
            TypedExpressionNode::Program(program) => {
                let mut last = Ok(TypedExpression::void());
                for expr in &program.list {
                    last = Interpreter::eval(expr, env);
                }
                last
            }
            TypedExpressionNode::Function(f) => {
                if let Some(sym) = &f.sym {
                    env.bind(String::from(sym), expr.clone());
                }
                env.bind_function(expr.clone());
                Ok(expr.clone())
            }

            TypedExpressionNode::NativeFunction(_) => Ok(expr.clone()),
            TypedExpressionNode::FunctionCall(fc) => {
                let value = Interpreter::eval(&fc.expr, env)?;
                Interpreter::call(&value, &fc.arguments, env)
            }
            TypedExpressionNode::TypedTypeQualifiedExpression(qf) => {
                let value = Interpreter::eval(&qf.expr, env)?;
                let a = match value.node {
                    TypedExpressionNode::NativeFunction(f) => {
                        let mut a = (*f).clone();
                        a.type_arguments = Some(qf.type_arguments.clone());
                        TypedExpression {
                            resolved_type: value.resolved_type,
                            node: TypedExpressionNode::NativeFunction(Rc::new(a)),
                        }
                    }
                    _ => unimplemented!(),
                };

                Ok(a)
            }
            TypedExpressionNode::Struct(_s) => Ok(expr.clone()),
            TypedExpressionNode::Sdl(_s) => Ok(expr.clone()),
        }
    }

    pub fn call(
        value: &TypedExpression,
        args: &[TypedExpression],
        env: &mut SymbolTable,
    ) -> Result<TypedExpression, Error> {
        match &value.node {
            TypedExpressionNode::Function(f) => {
                let scope_id = env.enter_function(f.id);
                for (idx, parameter) in f.parameters.iter().enumerate() {
                    let void = TypedExpression::void();
                    let argument = args.get(idx).unwrap_or(&void);
                    let val = Interpreter::eval(argument, env)?;
                    env.bind(String::from(&parameter.0), val.clone());
                }
                let result = Interpreter::eval(&f.expr, env)?;
                env.leave_function(scope_id);
                Ok(result)
            }
            TypedExpressionNode::NativeFunction(f) => {
                let native_function = f.function;
                if f.call_by_value {
                    let mut evaluated_arguments: Vec<TypedExpression> =
                        Vec::with_capacity(args.len());
                    for argument in args {
                        evaluated_arguments.push(Interpreter::eval(argument, env)?)
                    }
                    let res = native_function(env, &evaluated_arguments, &f.type_arguments)
                        .map_err(Error::new)?;
                    Ok(res)
                } else {
                    let res = native_function(env, args, &f.type_arguments).map_err(Error::new)?;
                    Ok(res)
                }
            }
            TypedExpressionNode::Binding(b) => {
                let mut bound_args = args.to_vec();
                bound_args.insert(0, b.instance.clone());
                Interpreter::call(&b.origin, &bound_args, env)
            }
            _ => Err(Error::new(format!(
                "unexpected {}, expected function",
                value.node
            ))),
        }
    }
}
