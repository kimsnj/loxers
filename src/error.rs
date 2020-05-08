use crate::token::Token;
use crate::value::Value;
use thiserror::Error;

#[derive(Error, Debug, Default)]
#[error("At [line {line:?}] {location:?}:\n{message}")]
pub(crate) struct LoxError {
    pub line: usize,
    pub location: String,
    pub message: String,
}

impl LoxError {
    pub fn new(message: String, t: &Token) -> Self {
        Self {
            message,
            line: t.line,
            location: t.lexeme.clone(),
        }
    }

    pub fn msg(message: String) -> Self {
        Self {
            message,
            ..Default::default()
        }
    }

    pub fn enrich(mut self, t: &Token) -> Self {
        self.line = t.line;
        self.location = t.lexeme.clone();
        self
    }
}

#[derive(Error, Debug)]
pub(crate) enum ControlFlow {
    #[error("runtime error: {0}")]
    Error(LoxError),

    #[error("returning: {1}")]
    Return(Token, Value),
}

impl From<LoxError> for ControlFlow {
    fn from(e: LoxError) -> ControlFlow {
        ControlFlow::Error(e)
    }
}
