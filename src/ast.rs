use pest::{iterators::Pairs, pratt_parser::PrattParser};
use tracing::debug;

use crate::cst::Rule;

#[derive(Debug, PartialEq)]
pub struct Evalx {
    pub expression: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    BinaryExpression {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    UnaryExpression {
        op: UnaryOp,
        expression: Box<Expression>,
    },
    Operand {
        operand: Operand,
    },
}

#[derive(Debug, PartialEq)]
pub enum Operand {
    Variable { name: String },
    Literal { lit: Literal },
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Or,
    And,
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number { value: f64 },
    String { value: String },
    Boolean { value: bool },
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

    PrattParser::new()
        .op(Op::prefix(NOT) | Op::prefix(NEGATE))
        .op(Op::infix(EQUALS, Left)
            | Op::infix(NOT_EQUALS, Left)
            | Op::infix(GREATER_THAN, Left)
            | Op::infix(GREATER_THAN_EQUALS, Left)
            | Op::infix(LESS_THAN, Left)
            | Op::infix(LESS_THAN_EQUALS, Left))
        .op(Op::infix(ADD, Left) | Op::infix(SUBTRACT, Left))
        .op(Op::infix(MULTIPLY, Left)
            | Op::infix(DIVIDE, Left)
            | Op::infix(MODULO, Left))
        .op(Op::infix(OR, Left))
        .op(Op::infix(AND, Left))
    };
}

pub fn parse_ast(mut input: Pairs<Rule>) -> Expression {
    parse_expression(input.next().unwrap().into_inner(), &PRATT_PARSER)
}

fn parse_expression(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Expression {
    debug!(?pairs, "parse_expression");
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::expression => parse_expression(primary.into_inner(), pratt),
            Rule::REAL => Expression::Operand {
                operand: Operand::Literal {
                    lit: Literal::Number {
                        value: primary.as_str().parse().unwrap(),
                    },
                },
            },
            Rule::INT => Expression::Operand {
                operand: Operand::Literal {
                    lit: Literal::Number {
                        value: primary.as_str().parse().unwrap(),
                    },
                },
            },
            Rule::BOOLEAN => Expression::Operand {
                operand: Operand::Literal {
                    lit: Literal::Boolean {
                        value: primary.as_str().parse().unwrap(),
                    },
                },
            },
            Rule::STRING => Expression::Operand {
                operand: Operand::Literal {
                    lit: Literal::String {
                        value: primary.as_str()[1..primary.as_str().len() - 1].to_string(),
                    },
                },
            },
            Rule::VARIABLE => Expression::Operand {
                operand: Operand::Variable {
                    name: primary.as_str()[1..primary.as_str().len() - 1].to_string(),
                },
            },
            rule => unreachable!(
                "parse_expression expected terminal or expression, found {:?}",
                rule
            ),
        })
        .map_infix(|left, op, right| {
            let ast_op = match op.as_rule() {
                Rule::MULTIPLY => BinaryOp::Multiply,
                Rule::DIVIDE => BinaryOp::Divide,
                Rule::ADD => BinaryOp::Add,
                Rule::SUBTRACT => BinaryOp::Subtract,
                Rule::OR => BinaryOp::Or,
                Rule::AND => BinaryOp::And,
                Rule::EQUALS => BinaryOp::Equals,
                Rule::NOT_EQUALS => BinaryOp::NotEquals,
                Rule::GREATER_THAN => BinaryOp::GreaterThan,
                Rule::GREATER_THAN_EQUALS => BinaryOp::GreaterThanEquals,
                Rule::LESS_THAN => BinaryOp::LessThan,
                Rule::LESS_THAN_EQUALS => BinaryOp::LessThanEquals,
                Rule::MODULO => BinaryOp::Modulo,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };

            Expression::BinaryExpression {
                left: Box::new(left),
                op: ast_op,
                right: Box::new(right),
            }
        })
        .map_prefix(|op, expression| {
            let op = match op.as_rule() {
                Rule::NOT => UnaryOp::Not,
                Rule::NEGATE => UnaryOp::Negate,
                rule => unreachable!("Expr::parse expected prefix operation, found {:?}", rule),
            };

            Expression::UnaryExpression {
                op,
                expression: Box::new(expression),
            }
        })
        .parse(pairs)
}

#[cfg(test)]
mod test {

    use crate::{
        ast::{parse_ast, BinaryOp, Expression, Literal, Operand},
        cst::parse_cst,
    };

    #[test]
    fn test_parse_simple_arithmetic() {
        let cst = parse_cst("7 * 4 + 2 / 2").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast,
            Expression::BinaryExpression {
                left: Box::new(Expression::BinaryExpression {
                    left: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Number { value: 7f64 }
                        }
                    }),
                    op: BinaryOp::Multiply,
                    right: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Number { value: 4f64 }
                        }
                    })
                }),
                op: BinaryOp::Add,
                right: Box::new(Expression::BinaryExpression {
                    left: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Number { value: 2f64 }
                        }
                    }),
                    op: BinaryOp::Divide,
                    right: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Number { value: 2f64 }
                        }
                    })
                })
            }
        )
    }

    #[test]
    fn test_arithmetic_with_parenthesis() {
        let cst = parse_cst("7 * (4 + 2)").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast,
            Expression::BinaryExpression {
                left: Box::new(Expression::Operand {
                    operand: Operand::Literal {
                        lit: Literal::Number { value: 7f64 }
                    }
                }),
                op: BinaryOp::Multiply,
                right: Box::new(Expression::BinaryExpression {
                    left: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Number { value: 4f64 }
                        }
                    }),
                    op: BinaryOp::Add,
                    right: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Number { value: 2f64 }
                        }
                    })
                })
            }
        )
    }

    #[test]
    fn test_boolean() {
        let cst = parse_cst("true and false or false and true").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast,
            Expression::BinaryExpression {
                left: Box::new(Expression::BinaryExpression {
                    left: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Boolean { value: true }
                        }
                    }),
                    op: BinaryOp::And,
                    right: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Boolean { value: false }
                        }
                    })
                }),
                op: BinaryOp::Or,
                right: Box::new(Expression::BinaryExpression {
                    left: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Boolean { value: false }
                        }
                    }),
                    op: BinaryOp::And,
                    right: Box::new(Expression::Operand {
                        operand: Operand::Literal {
                            lit: Literal::Boolean { value: true }
                        }
                    })
                })
            }
        )
    }

    #[test]
    fn test_string_comparison() {
        let cst = parse_cst("\"test\" == \"test2\"").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast,
            Expression::BinaryExpression {
                left: Box::new(Expression::Operand {
                    operand: Operand::Literal {
                        lit: Literal::String {
                            value: "test".to_string()
                        }
                    }
                }),
                op: BinaryOp::Equals,
                right: Box::new(Expression::Operand {
                    operand: Operand::Literal {
                        lit: Literal::String {
                            value: "test2".to_string()
                        }
                    }
                })
            }
        )
    }

    #[test]
    fn test_variable() {
        let cst = parse_cst("[var] == \"test2\"").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast,
            Expression::BinaryExpression {
                left: Box::new(Expression::Operand {
                    operand: Operand::Variable {
                        name: "var".to_string()
                    }
                }),
                op: BinaryOp::Equals,
                right: Box::new(Expression::Operand {
                    operand: Operand::Literal {
                        lit: Literal::String {
                            value: "test2".to_string()
                        }
                    }
                })
            }
        )
    }
}
