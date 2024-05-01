use crate::eval::EvalError;
use thiserror::Error;

pub type EasylangResult<T> = Result<T, EazylangError>;

#[derive(Debug, Error)]
pub enum EazylangError {
    #[error("Error during parsing")]
    CstError(#[from] pest::error::Error<crate::cst::Rule>),
    #[error("Error during expression evaluation")]
    EvaluationError(#[from] EvalError),
}
