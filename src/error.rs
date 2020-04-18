use crate::token::Token;
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

    pub fn enrich(mut self, t: Token) -> Self {
        self.line = t.line;
        self.location = t.lexeme;
        self
    }
}
