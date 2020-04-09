use thiserror::Error;

#[derive(Error, Debug)]
#[error("At [line {line:?}] {location:?}:\n{message}")]
pub(crate) struct LoxError {
    pub line: usize,
    pub location: String,
    pub message: String,
}
