use crate::ast::Expr;
use crate::error::LoxError;
use std::convert::{From, TryFrom};

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::Boolean(b) => b.fmt(f),
            Value::Number(n) => {
                let mut s = format!("{}", n);
                if s.ends_with(".0") {
                    s.truncate(s.len() - 2);
                }
                f.write_str(&s)
            }
            Value::String(s) => f.write_str(s),
        }
    }
}

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
        use Value::Number;
        if let (Number(ns), Number(no)) = (self, other) {
            ns.partial_cmp(no)
        } else {
            None
        }
    }
}

impl From<Expr> for Value {
    /// Converts literal to values
    /// Panics if called with non-literals
    fn from(literal: Expr) -> Self {
        match literal {
            Expr::StringLit(s) => Value::String(s),
            Expr::NumberLit(n) => Value::Number(n),
            Expr::BoolLit(b) => Value::Boolean(b),
            Expr::Nil => Value::Nil,
            _ => panic!("Conversion limited to literal values"),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = LoxError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Number(n) = value {
            Ok(n)
        } else {
            Err(LoxError::msg(format!("Expected number found: {:?}", value)))
        }
    }
}

impl TryFrom<Value> for String {
    type Error = LoxError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::String(s) = value {
            Ok(s)
        } else {
            Err(LoxError::msg(format!("Expected string found: {:?}", value)))
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = LoxError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(b) => Ok(b),
            Value::Nil => Ok(false),
            _ => Err(LoxError::msg(format!(
                "Expected boolean found: {:?}",
                value
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eq() {
        assert_eq!(Value::Nil, Value::Nil);
        assert_eq!(Value::Boolean(true), Value::Boolean(true));
        assert_ne!(Value::Boolean(false), Value::Boolean(true));
        assert_ne!(Value::String("3".into()), Value::Number(3.0));
    }

    #[test]
    fn test_partial_ord() {
        use std::cmp::Ordering::*;
        use Value::*;

        assert!(Nil.partial_cmp(&Nil) == None);
        assert!(Number(3.2).partial_cmp(&Number(3.0)) == Some(Greater));
        assert!(Number(3.2).partial_cmp(&Number(3.2)) == Some(Equal));
        assert!(Number(-3.2).partial_cmp(&Number(3.2)) == Some(Less));
        assert!(String("a".into()).partial_cmp(&String("b".into())) == None);
    }
    #[test]
    fn test_partial_display() {
        use Value::*;
        assert_eq!(Nil.to_string(), "nil".to_string());
        assert_eq!(Number(2.0).to_string(), "2".to_string());
        assert_eq!(Number(2.54).to_string(), "2.54".to_string());
        assert_eq!(String("abc".into()).to_string(), "abc".to_string());
        assert_eq!(Boolean(false).to_string(), "false".to_string());
    }
}
