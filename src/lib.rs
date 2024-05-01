use ast::parse_ast;
use cst::parse_cst;
use error::EasylangResult;
use eval::{eval, EvalXContext, EvalXValue};

pub mod ast;
pub mod cst;
pub mod error;
pub mod eval;

pub fn evalx(ctx: &EvalXContext, expr: &str) -> EasylangResult<EvalXValue> {
    let cst = parse_cst(expr)?;
    let ast = parse_ast(cst);

    Ok(eval(ctx, &ast)?)
}
