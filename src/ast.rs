use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
};
use tracing::debug;

use crate::cst::Rule;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Declaration(Declaration),
    Loop(Loop),
    If(If),
    Return(Return),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function(Function),
    Variable(Variable),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: CodeBlock,
}

#[derive(Debug, PartialEq)]
pub struct CodeBlock {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Loop {
    pub var_name: String,
    pub range_start: Expression,
    pub range_end: Expression,
    pub body: CodeBlock,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub expression: Expression,
    pub body: CodeBlock,
}

#[derive(Debug, PartialEq)]
pub struct Return {
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
    FuncCall {
        name: String,
        arguments: Vec<Expression>,
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

pub fn parse_ast(input: Pairs<Rule>) -> Vec<Stmt> {
    let pratt = PrattParser::new()
        .op(Op::prefix(Rule::NOT) | Op::prefix(Rule::NEGATE))
        .op(Op::infix(Rule::EQUALS, Assoc::Left)
            | Op::infix(Rule::NOT_EQUALS, Assoc::Left)
            | Op::infix(Rule::GREATER_THAN, Assoc::Left)
            | Op::infix(Rule::GREATER_THAN_EQUALS, Assoc::Left)
            | Op::infix(Rule::LESS_THAN, Assoc::Left)
            | Op::infix(Rule::LESS_THAN_EQUALS, Assoc::Left))
        .op(Op::infix(Rule::ADD, Assoc::Left) | Op::infix(Rule::SUBTRACT, Assoc::Left))
        .op(Op::infix(Rule::MULTIPLY, Assoc::Left)
            | Op::infix(Rule::DIVIDE, Assoc::Left)
            | Op::infix(Rule::MODULO, Assoc::Left))
        .op(Op::infix(Rule::OR, Assoc::Left))
        .op(Op::infix(Rule::AND, Assoc::Left));

    input
        .flat_map(|pair| {
            if let Rule::EOI = pair.as_rule() {
                None
            } else {
                Some(parse_stmt(pair, &pratt))
            }
        })
        .collect()
}

fn parse_stmt(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> Stmt {
    debug!(?pair, "Pair");
    match pair.as_rule() {
        Rule::declaration => parse_declaration(pair.into_inner(), pratt),
        Rule::r#return => parse_return(pair.into_inner(), pratt),
        Rule::r#loop => todo!(),
        Rule::r#if => todo!(),
        Rule::expression => Stmt::Expression(parse_expression(pair.into_inner(), pratt)),
        rule => unreachable!("parse_stmt expected stmt, found {:?}", rule),
    }
}

fn parse_return(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Stmt {
    debug!(?pairs, "parse_return");
    Stmt::Return(Return {
        expression: parse_expression(pairs, pratt),
    })
}

fn parse_expression(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Expression {
    debug!(?pairs, "parse_expression");
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::expression => parse_expression(primary.into_inner(), pratt),
            Rule::call => parse_call(primary.into_inner(), pratt),
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

fn parse_call(mut pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Expression {
    debug!(?pairs, "parse_call");
    Expression::FuncCall {
        name: pairs.next().unwrap().as_str().to_string(),
        arguments: parse_arguments(pairs.next().unwrap(), pratt),
    }
}

fn parse_arguments(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> Vec<Expression> {
    debug!(?pair, "parse_arguments");
    pair.into_inner()
        .map(|pair| parse_expression(pair.into_inner(), pratt))
        .collect()
}

fn parse_declaration(mut pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Stmt {
    debug!(?pairs, "parse_declaration");
    let pair = pairs.next().unwrap();
    match pair.as_rule() {
        Rule::function => parse_function(pair, pratt),
        Rule::variable => todo!(),
        rule => unreachable!("parse_declaration expected declaration, found {:?}", rule),
    }
}

fn parse_function(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> Stmt {
    debug!(?pair, "parse_function");
    let mut inner = pair.into_inner();
    Stmt::Declaration(Declaration::Function(Function {
        name: inner.next().unwrap().as_str().to_string(),
        args: parse_parameters(inner.next().unwrap()),
        body: parse_code_block(inner.next().unwrap(), pratt),
    }))
}

fn parse_code_block(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> CodeBlock {
    debug!(?pair, "parse_code_block");
    let inner = pair.into_inner();
    CodeBlock {
        stmts: inner.map(|pair| parse_stmt(pair, pratt)).collect(),
    }
}

fn parse_parameters(pair: Pair<Rule>) -> Vec<String> {
    debug!(?pair, "parse_parameters");
    pair.into_inner()
        .map(|pair| pair.as_str().to_string())
        .collect()
}

#[cfg(test)]
mod test {
    use tracing::Level;

    use crate::{
        ast::{
            parse_ast, BinaryOp, CodeBlock, Declaration, Expression, Function, Literal, Operand,
            Stmt,
        },
        cst::parse_cst,
    };

    #[test]
    fn test_func() {
        tracing_subscriber::fmt()
            .with_max_level(Level::DEBUG)
            .init();

        let cst = parse_cst(
            r#"
                func give_five() { return 5 }
                give_five()
            "#,
        )
        .unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast,
            vec![
                Stmt::Declaration(Declaration::Function(Function {
                    name: "give_five".to_string(),
                    args: Vec::new(),
                    body: CodeBlock {
                        stmts: vec![Stmt::Return(crate::ast::Return {
                            expression: Expression::Operand {
                                operand: Operand::Literal {
                                    lit: Literal::Number { value: 5.0 }
                                }
                            }
                        })]
                    }
                })),
                Stmt::Expression(Expression::FuncCall {
                    name: "give_five".to_string(),
                    arguments: Vec::new()
                })
            ]
        )
    }

    #[test]
    fn test_parse_simple_arithmetic() {
        let cst = parse_cst("7 * 4 + 2 / 2").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast.get(0).unwrap(),
            &Stmt::Expression(Expression::BinaryExpression {
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
            })
        )
    }

    #[test]
    fn test_arithmetic_with_parenthesis() {
        let cst = parse_cst("7 * (4 + 2)").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast.get(0).unwrap(),
            &Stmt::Expression(Expression::BinaryExpression {
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
            })
        )
    }

    #[test]
    fn test_boolean() {
        let cst = parse_cst("true and false or false and true").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast.get(0).unwrap(),
            &Stmt::Expression(Expression::BinaryExpression {
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
            })
        )
    }

    #[test]
    fn test_string_comparison() {
        let cst = parse_cst("\"test\" == \"test2\"").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast.get(0).unwrap(),
            &Stmt::Expression(Expression::BinaryExpression {
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
            })
        )
    }

    #[test]
    fn test_variable() {
        let cst = parse_cst("[var] == \"test2\"").unwrap();
        let ast = parse_ast(cst);

        assert_eq!(
            ast.get(0).unwrap(),
            &Stmt::Expression(Expression::BinaryExpression {
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
            })
        )
    }
}
