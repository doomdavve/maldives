use std::convert::TryInto;
use std::error;
use std::fmt;

use crate::symboltable::SymbolTable;
use crate::typedexpression::TypedBinaryOperation;
use crate::typedexpression::TypedExpression;
use crate::typedexpression::TypedExpressionNode;

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError {
    pub message: String,
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Interpreter error: {}", self.message)
    }
}

impl error::Error for InterpreterError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl InterpreterError {
    fn new(message: String) -> InterpreterError {
        InterpreterError { message }
    }
}

pub struct Interpreter;

impl Interpreter {
    pub fn eval(
        expr: &TypedExpression,
        mut env: &mut SymbolTable,
    ) -> Result<TypedExpression, InterpreterError> {
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
                        _ => Err(InterpreterError::new(format!(
                            "Unexpected terms in sum operator"
                        ))),
                    },
                    TypedBinaryOperation::Difference => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::integer(li - ri))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-integer terms to difference operator"
                        ))),
                    },
                    TypedBinaryOperation::Multiply => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::integer(li * ri))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-integer terms to multiply operator"
                        ))),
                    },
                    TypedBinaryOperation::ToThePowerOf => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::integer(li.pow(ri.try_into().unwrap())))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "Unexpected terms in power operator"
                        ))),
                    },

                    TypedBinaryOperation::Divide => {
                        // TODO: handle division by zero.
                        match (lhs.node, rhs.node) {
                            (
                                TypedExpressionNode::Integer(li),
                                TypedExpressionNode::Integer(ri),
                            ) => Ok(TypedExpression::integer(li / ri)),
                            _ => Err(InterpreterError::new(format!(
                                "One or more non-integer terms to divide operator"
                            ))),
                        }
                    }
                    TypedBinaryOperation::LessThan => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li < ri))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-boolean terms to divide operator"
                        ))),
                    },
                    TypedBinaryOperation::GreaterThan => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li > ri))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-boolean terms to divide operator"
                        ))),
                    },
                    TypedBinaryOperation::LessEqualThan => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li <= ri))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-boolean terms to divide operator"
                        ))),
                    },
                    TypedBinaryOperation::GreaterEqualThan => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li >= ri))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-boolean terms to divide operator"
                        ))),
                    },
                    TypedBinaryOperation::Concat => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::String(li), TypedExpressionNode::String(ri)) => {
                            Ok(TypedExpression::string(li + &ri))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-string terms to concatenation operator"
                        ))),
                    },
                    TypedBinaryOperation::Equal => match (lhs.node, rhs.node) {
                        (TypedExpressionNode::Integer(li), TypedExpressionNode::Integer(ri)) => {
                            Ok(TypedExpression::bool(li == ri))
                        }
                        _ => Err(InterpreterError::new(format!(
                            "One or more non-integer terms to equal operator"
                        ))),
                    },
                    TypedBinaryOperation::Assign => match lhs.node {
                        TypedExpressionNode::Symbol(sym) => {
                            let expr = Interpreter::eval(&rhs, env)?;
                            env.update(String::from(&sym), expr.clone()).ok_or(
                                InterpreterError::new(format!("{} not found in this scope", sym)),
                            )?;
                            Ok(expr)
                        }
                        _ => Err(InterpreterError::new(format!(
                            "unexpected left hand side of assignment"
                        ))),
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
                        Ok(Interpreter::eval(&false_branch, env)?)
                    }
                    (TypedExpressionNode::Bool(false), _) => Ok(TypedExpression::void()),
                    _ => Err(InterpreterError::new(format!(
                        "Unexpected result of conditional"
                    ))),
                }
            }
            TypedExpressionNode::Group(g) => Interpreter::eval(&g.expr, env),
            TypedExpressionNode::Bool(b) => Ok(TypedExpression::bool(*b)),
            TypedExpressionNode::Integer(i) => Ok(TypedExpression::integer(*i)),
            TypedExpressionNode::String(s) => Ok(TypedExpression::string(s.to_string())),
            TypedExpressionNode::Array(_) => Ok(expr.clone()),
            TypedExpressionNode::Symbol(s) => {
                let value = env
                    .lookup(s)
                    .ok_or_else(|| InterpreterError::new(format!("unknown symbol '{}'", s)))?;
                Ok(value.clone())
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
                    last = Interpreter::eval(&expr, env)?;
                    match &last.node {
                        TypedExpressionNode::Break(break_expr) => {
                            Some(break_expr.expr.clone());
                            break;
                        }
                        _ => (),
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
                        let res = Interpreter::eval(&expr, env)?;
                        last = match res.node {
                            TypedExpressionNode::Break(break_expr) => Some(break_expr.expr.clone()),
                            _ => None,
                        }
                    }
                }
                env.leave_scope();
                last.ok_or(InterpreterError::new("internal error".to_string()))
            }
            TypedExpressionNode::Break(b) => Ok(TypedExpression::r#break(Interpreter::eval(
                &b.expr, &mut env,
            )?)),
            TypedExpressionNode::Program(program) => {
                let mut last = Ok(TypedExpression::void());
                for expr in &program.list {
                    last = Interpreter::eval(&expr, &mut env);
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
                match value.node {
                    TypedExpressionNode::Function(f) => {
                        let scope_id = env.enter_function(f.id);
                        for (idx, parameter) in f.parameters.iter().enumerate() {
                            let void = TypedExpression::void();
                            let argument = fc.arguments.get(idx).unwrap_or(&void);
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
                                Vec::with_capacity(fc.arguments.len());
                            for argument in &fc.arguments {
                                evaluated_arguments.push(Interpreter::eval(argument, env)?)
                            }
                            let res = native_function(env, &evaluated_arguments)
                                .map_err(|message| InterpreterError::new(message))?;
                            Ok(res)
                        } else {
                            let res = native_function(env, &fc.arguments)
                                .map_err(|message| InterpreterError::new(message))?;
                            Ok(res)
                        }
                    }
                    _ => Err(InterpreterError::new(format!(
                        "unexpected {:?}, expected function",
                        fc.expr
                    ))),
                }
            }
        }
    }
}
