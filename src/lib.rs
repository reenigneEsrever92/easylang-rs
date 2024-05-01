use ast::parse_ast;
use cst::parse_cst;
use error::EvalXResult;
use eval::{eval, EvalXContext, EvalXValue};

pub mod ast;
pub mod cst;
pub mod error;
pub mod eval;

pub fn evalx(ctx: &EvalXContext, expr: &str) -> EvalXResult<EvalXValue> {
    let cst = parse_cst(expr)?;
    let ast = parse_ast(cst);

    Ok(eval(ctx, &ast)?)
}
