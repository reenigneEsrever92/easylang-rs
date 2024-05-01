use crate::eval::EvalError;
use thiserror::Error;

pub type EvalXResult<T> = Result<T, EvalXError>;

#[derive(Debug, Error)]
pub enum EvalXError {
    #[error("Error during parsing")]
    CstError(#[from] Box<pest::error::Error<crate::cst::Rule>>),
    #[error("Error during expression evaluation")]
    EvaluationError(#[from] EvalError),
}
