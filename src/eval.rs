use std::{cell::RefCell, collections::HashMap};

use crate::{
    ast::{Declaration, Expression, Stmt},
    error::{EasylangResult, EazylangError},
    eval_eazylang,
};

#[derive(Debug, PartialEq)]
pub enum EasylangValue {
    None,
    Number(f64),
    Boolean(bool),
    String(String),
}

pub struct EvalContext {
    stmts: RefCell<Vec<Stmt>>,
    variables: RefCell<HashMap<String, String>>,
}

impl EvalContext {
    pub fn new(stmts: Vec<Stmt>, variables: HashMap<String, String>) -> Self {
        Self {
            stmts: RefCell::new(stmts),
            variables: RefCell::new(variables),
        }
    }

    pub fn eval(&self) -> EasylangResult<EasylangValue> {
        let result = self
            .stmts
            .borrow()
            .iter()
            .map(|stmt| self.eval_stmt(stmt))
            .last()
            .unwrap_or(Ok(EasylangValue::None));

        result
    }

    fn eval_stmt(&self, stmt: &Stmt) -> EasylangResult<EasylangValue> {
        match stmt {
            Stmt::Declaration(decl) => self.parse_declaration(&decl),
            Stmt::Loop(_) => todo!(),
            Stmt::If(_) => todo!(),
            Stmt::Return(_) => todo!(),
            Stmt::Expression(e) => self.eval_expression(&e),
        }
    }

    fn parse_declaration(&self, decl: &Declaration) -> Result<EasylangValue, EazylangError> {
        match decl {
            Declaration::Function(_func) => Ok(EasylangValue::None),
            Declaration::Variable(_var) => todo!(),
        }
    }

    fn eval_expression(&self, expression: &Expression) -> EasylangResult<EasylangValue> {
        match expression {
            Expression::BinaryExpression { left, op, right } => match op {
                crate::ast::BinaryOp::Add => {
                    let (left, right) = self.eval_numbers(left, right)?;
                    Ok(EasylangValue::Number(left + right))
                }
                crate::ast::BinaryOp::Subtract => {
                    let (left, right) = self.eval_numbers(left, right)?;
                    Ok(EasylangValue::Number(left - right))
                }
                crate::ast::BinaryOp::Multiply => {
                    let (left, right) = self.eval_numbers(left, right)?;
                    Ok(EasylangValue::Number(left * right))
                }
                crate::ast::BinaryOp::Divide => {
                    let (left, right) = self.eval_numbers(left, right)?;
                    Ok(EasylangValue::Number(left / right))
                }
                crate::ast::BinaryOp::Modulo => {
                    let (left, right) = self.eval_numbers(left, right)?;
                    Ok(EasylangValue::Number(left % right))
                }
                crate::ast::BinaryOp::Equals => {
                    let (left, right) = self.eval_values(left, right)?;
                    Ok(EasylangValue::Boolean(left == right))
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
                    let value = self.eval_expression(expression)?;
                    match value {
                        EasylangValue::Number(value) => Ok(EasylangValue::Number(value * -1f64)),
                        _ => Err(EazylangError::EvaluationError {
                            msg: "! operator can only be used on numbers.".to_string(),
                        }),
                    }
                }
            },
            Expression::Operand { operand } => match operand {
                crate::ast::Operand::Variable { name } => {
                    match self.variables.borrow().get(name) {
                        // evaluate variables as if they where eazylang, too.
                        Some(var) => eval_eazylang(var, HashMap::new()),
                        None => {
                            return Err(EazylangError::EvaluationError {
                                msg: format!("Expected a variable with name: {name}"),
                            })
                        }
                    }
                }
                crate::ast::Operand::Literal { lit } => match lit {
                    crate::ast::Literal::Number { value } => Ok(EasylangValue::Number(*value)),
                    crate::ast::Literal::String { value } => {
                        Ok(EasylangValue::String(value.to_string()))
                    }
                    crate::ast::Literal::Boolean { value } => Ok(EasylangValue::Boolean(*value)),
                },
            },
            Expression::FuncCall {
                name: _,
                arguments: _,
            } => todo!(),
        }
    }

    fn eval_values<'a>(
        &self,
        left: &Expression,
        right: &Expression,
    ) -> EasylangResult<(EasylangValue, EasylangValue)> {
        let (left_value, right_value) = (self.eval_expression(left)?, self.eval_expression(right)?);
        match (&left_value , &right_value) {
        (EasylangValue::Number(_), EasylangValue::Number(_)) => Ok((left_value, right_value)),
        (EasylangValue::Boolean(_), EasylangValue::Boolean(_)) => Ok((left_value, right_value)),
        (EasylangValue::String(_), EasylangValue::String(_)) => Ok((left_value, right_value)),
        (_left, _right) => Err(EazylangError::EvaluationError { msg: format!("Expected left and right to be the same type. But they are: left: {left_value:?}, right: {right_value:?}") }),
    }
    }

    fn eval_numbers(&self, left: &Expression, right: &Expression) -> EasylangResult<(f64, f64)> {
        match (self.eval_expression(left)?, self.eval_expression(right)?) {
            (EasylangValue::Number(value1), EasylangValue::Number(value2)) => Ok((value1, value2)),
            _ => Err(EazylangError::EvaluationError {
                msg: "Expected left and right expression to evaluate to a number.".to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use pest::Parser;
    use std::collections::HashMap;

    use crate::{
        ast::parse_ast,
        cst::EasylangParser,
        eval::{EasylangValue, EvalContext},
    };

    macro_rules! eval_math {
        ($($name:ident, $expr:literal, $result:expr)*) => {
        $(
            #[test]
            fn $name() {
                let cst = EasylangParser::parse(crate::cst::Rule::easylang, $expr).unwrap();
                let ast = parse_ast(cst);
                let eval = EvalContext::new(ast, HashMap::new()).eval().unwrap();

                assert_eq!(eval, EasylangValue::Number($result));
            }
        )*
        }
    }

    macro_rules! eval_boolean {
        ($($name:ident, $expr:literal, $result:expr)*) => {
        $(
            #[test]
            fn $name() {
                let cst = EasylangParser::parse(crate::cst::Rule::easylang, $expr).unwrap();
                let ast = parse_ast(cst);
                let eval = EvalContext::new(ast, HashMap::new()).eval().unwrap();

                assert_eq!(eval, EasylangValue::Boolean($result));
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

    // TODO
    // eval_math!(
    //     test_function_call,
    //     r#"
    //         func give_five() { return 5 }
    //         give_five()
    //     "#,
    //     5.0
    // );

    #[test]
    fn test_variable() {
        let cst = EasylangParser::parse(crate::cst::Rule::easylang, "\"test\" == [var]").unwrap();
        let ast = parse_ast(cst);
        let eval = EvalContext::new(
            ast,
            HashMap::from([("var".to_string(), "\"test\"".to_string())]),
        )
        .eval()
        .unwrap();

        assert_eq!(eval, EasylangValue::Boolean(true));
    }
}
