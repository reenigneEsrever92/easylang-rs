use std::collections::HashMap;

use ast::parse_ast;
use cst::parse_cst;
use error::EasylangResult;
use eval::{EasylangValue, EvalContext};

pub mod ast;
pub mod cst;
pub mod error;
pub mod eval;

pub fn eval_eazylang(
    expr: &str,
    variables: HashMap<String, String>,
) -> EasylangResult<EasylangValue> {
    let cst = parse_cst(expr)?;
    let ast = parse_ast(cst);

    EvalContext::new(ast, variables).eval()
}
