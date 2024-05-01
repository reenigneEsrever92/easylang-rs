use std::collections::HashMap;
use thiserror::Error;

use crate::{ast::Expression, error::EvalXError, evalx};

pub type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug, PartialEq)]
pub enum EvalXValue {
    None,
    Number(f64),
    Boolean(bool),
    String(String),
}

#[derive(Default)]
pub struct EvalXContext {
    variables: HashMap<String, String>,
    funcs: HashMap<String, EvalXFunc>,
}

impl EvalXContext {
    fn new(variables: HashMap<String, String>, funcs: HashMap<String, EvalXFunc>) -> Self {
        Self { variables, funcs }
    }
}

pub enum EvalXFunc {
    Function(fn() -> EvalXValue),
    Function1(fn(EvalXValue) -> EvalXValue),
}

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("")]
    Custom { msg: String },
    #[error("")]
    EasylangError(#[from] Box<EvalXError>),
}

pub fn eval(ctx: &EvalXContext, expression: &Expression) -> EvalResult<EvalXValue> {
    let result = eval_expression(ctx, expression);

    result
}

fn eval_expression(ctx: &EvalXContext, expression: &Expression) -> EvalResult<EvalXValue> {
    match expression {
        Expression::BinaryExpression { left, op, right } => match op {
            crate::ast::BinaryOp::Add => {
                let (left, right) = eval_numbers(ctx, left, right)?;
                Ok(EvalXValue::Number(left + right))
            }
            crate::ast::BinaryOp::Subtract => {
                let (left, right) = eval_numbers(ctx, left, right)?;
                Ok(EvalXValue::Number(left - right))
            }
            crate::ast::BinaryOp::Multiply => {
                let (left, right) = eval_numbers(ctx, left, right)?;
                Ok(EvalXValue::Number(left * right))
            }
            crate::ast::BinaryOp::Divide => {
                let (left, right) = eval_numbers(ctx, left, right)?;
                Ok(EvalXValue::Number(left / right))
            }
            crate::ast::BinaryOp::Modulo => {
                let (left, right) = eval_numbers(ctx, left, right)?;
                Ok(EvalXValue::Number(left % right))
            }
            crate::ast::BinaryOp::Equals => {
                let (left, right) = eval_values(ctx, left, right)?;
                Ok(EvalXValue::Boolean(left == right))
            }
            crate::ast::BinaryOp::LessThan => todo!(),
            crate::ast::BinaryOp::GreaterThan => todo!(),
            crate::ast::BinaryOp::LessThanEquals => todo!(),
            crate::ast::BinaryOp::GreaterThanEquals => todo!(),
            crate::ast::BinaryOp::NotEquals => todo!(),
            crate::ast::BinaryOp::Or => todo!(),
            crate::ast::BinaryOp::And => todo!(),
        },
        Expression::UnaryExpression { op, expression } => match op {
            crate::ast::UnaryOp::Not => todo!(),
            crate::ast::UnaryOp::Negate => {
                let value = eval_expression(ctx, expression)?;
                match value {
                    EvalXValue::Number(value) => Ok(EvalXValue::Number(value * -1f64)),
                    _ => Err(EvalError::Custom {
                        msg: "! operator can only be used on numbers.".to_string(),
                    }),
                }
            }
        },
        Expression::Operand { operand } => match operand {
            crate::ast::Operand::Variable { name } => {
                match ctx.variables.get(name) {
                    // evaluate variables as if they were evalx, too.
                    Some(var) => Ok(evalx(ctx, var).map_err(Box::new)?),
                    None => Err(EvalError::Custom {
                        msg: format!("Expected a variable with name: {name}"),
                    }),
                }
            }
            crate::ast::Operand::Literal { lit } => match lit {
                crate::ast::Literal::Number { value } => Ok(EvalXValue::Number(*value)),
                crate::ast::Literal::String { value } => Ok(EvalXValue::String(value.to_string())),
                crate::ast::Literal::Boolean { value } => Ok(EvalXValue::Boolean(*value)),
            },
        },
    }
}

fn eval_values<'a>(
    ctx: &EvalXContext,
    left: &Expression,
    right: &Expression,
) -> EvalResult<(EvalXValue, EvalXValue)> {
    let (left_value, right_value) = (eval_expression(ctx, left)?, eval_expression(ctx, right)?);
    match (&left_value , &right_value) {
        (EvalXValue::Number(_), EvalXValue::Number(_)) => Ok((left_value, right_value)),
        (EvalXValue::Boolean(_), EvalXValue::Boolean(_)) => Ok((left_value, right_value)),
        (EvalXValue::String(_), EvalXValue::String(_)) => Ok((left_value, right_value)),
        (_left, _right) => Err(EvalError::Custom { msg: format!("Expected left and right to be the same type. But they are: left: {left_value:?}, right: {right_value:?}") }),
    }
}

fn eval_numbers(
    ctx: &EvalXContext,
    left: &Expression,
    right: &Expression,
) -> EvalResult<(f64, f64)> {
    match (eval_expression(ctx, left)?, eval_expression(ctx, right)?) {
        (EvalXValue::Number(value1), EvalXValue::Number(value2)) => Ok((value1, value2)),
        _ => Err(EvalError::Custom {
            msg: "Expected left and right expression to evaluate to a number.".to_string(),
        }),
    }
}

#[cfg(test)]
mod test {
    use pest::Parser;
    use std::collections::HashMap;

    use crate::{
        ast::parse_ast,
        cst::EasylangParser,
        eval::{eval, EvalXContext, EvalXValue},
    };

    macro_rules! eval_math {
        ($($name:ident, $expr:literal, $result:expr)*) => {
        $(
            #[test]
            fn $name() {
                let cst = EasylangParser::parse(crate::cst::Rule::evalx, $expr).unwrap();
                let ast = parse_ast(cst);
                let ctx = EvalXContext::default();
                let eval = eval(&ctx, &ast);

                assert_eq!(eval.unwrap(), EvalXValue::Number($result));
            }
        )*
        }
    }

    macro_rules! eval_boolean {
        ($($name:ident, $expr:literal, $result:expr)*) => {
        $(
            #[test]
            fn $name() {
                let cst = EasylangParser::parse(crate::cst::Rule::evalx, $expr).unwrap();
                let ast = parse_ast(cst);
                let ctx = EvalXContext::default();
                let eval = eval(&ctx, &ast);

                assert_eq!(eval.unwrap(), EvalXValue::Boolean($result));
            }
        )*
        }
    }

    eval_math!(test_math, "2 * 4 + 6 / 3", 10f64);
    eval_math!(test_math_1, "3 * (4 + 6) / 3", 10f64);
    eval_math!(test_math_2, "3 * (6 - 4) / 3", 2f64);
    eval_math!(test_math_3, "3%2", 1f64);
    eval_math!(test_math_unary, "-2", -2f64);
    eval_math!(test_math_unary_1, "5 * -(2 + 4.0)", -30f64);

    eval_boolean!(test_boolean, "true == true", true);
    eval_boolean!(test_boolean_1, "true == false", false);

    eval_boolean!(test_string_comparison, "\"test\" == \"test\"", true);
    eval_boolean!(test_string_comparison_1, "\'test\' == \"test\"", true);
    eval_boolean!(test_string_comparison_2, "\"test\" == \"test2\"", false);

    #[test]
    fn test_variable() {
        let cst = EasylangParser::parse(crate::cst::Rule::evalx, "\"test\" == [var]").unwrap();
        let ast = parse_ast(cst);
        let ctx = EvalXContext {
            variables: HashMap::from([("var".to_string(), "\"test\"".to_string())]),
            ..Default::default()
        };
        let eval = eval(&ctx, &ast);

        assert_eq!(eval.unwrap(), EvalXValue::Boolean(true));
    }
}
