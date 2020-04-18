use crate::ast::{self, Expr};
use crate::error::LoxError;
use crate::token::TokenKind;
use crate::value::Value;
use std::convert::TryInto;
use std::ops::{Add, Div, Mul, Sub};

type RuntimeRes = Result<Value, crate::error::LoxError>;
pub(crate) struct Interpreter {}

impl Interpreter {
    pub fn evaluate(expr: Expr) -> RuntimeRes {
        match expr {
            Expr::StringLit(_) | Expr::NumberLit(_) | Expr::BoolLit(_) | Expr::Nil => {
                Ok(expr.into())
            }
            Expr::Grouping(e) => Self::evaluate(*e),
            Expr::Unary(unary) => Self::evaluate_unary(*unary),
            Expr::Binary(binary) => Self::evaluate_binary(*binary),
        }
    }

    fn evaluate_unary(unary: ast::Unary) -> RuntimeRes {
        let operator = unary.operator;
        let right = Self::evaluate(unary.right)?;
        match operator.kind {
            TokenKind::Minus => {
                let right: f64 = right.try_into().map_err(|e: LoxError| e.enrich(operator))?;
                Ok(Value::Number(-right))
            }
            TokenKind::Bang => {
                let right: bool = right.try_into().map_err(|e: LoxError| e.enrich(operator))?;
                Ok(Value::Boolean(!right))
            }
            _ => unreachable!(),
        }
    }

    fn evaluate_binary(binary: ast::Binary) -> RuntimeRes {
        use Value::*;

        let operator = binary.operator;
        let left = Self::evaluate(binary.left)?;
        let right = Self::evaluate(binary.right)?;
        match operator.kind {
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::BangEqual
            | TokenKind::Equal
            | TokenKind::EqualEqual => {
                let ordering = left.partial_cmp(&right).ok_or(LoxError::new(
                    format!("Cannot compare {} and {}", left, right),
                    &operator,
                ))?;
                Ok(Boolean(to_bool(operator.kind, ordering)))
            }

            TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                let left: f64 = left.try_into()?;
                let right: f64 = right.try_into()?;
                Ok(Number(num_op(operator.kind)(left, right)))
            }
            TokenKind::Plus => match (left, right) {
                (Number(nl), Number(nr)) => Ok(Number(nl + nr)),
                (String(mut sl), String(sr)) => {
                    sl += &sr;
                    Ok(String(sl))
                }
                _ => Err(LoxError::new(
                    "+ requires both operands to be number or string".into(),
                    &operator,
                )),
            },
            _ => unreachable!(),
        }
    }
}

fn to_bool(kind: TokenKind, ordering: std::cmp::Ordering) -> bool {
    use std::cmp::Ordering::*;
    match kind {
        TokenKind::Greater => ordering == Greater,
        TokenKind::GreaterEqual => ordering == Greater || ordering == Equal,
        TokenKind::Less => ordering == Less,
        TokenKind::LessEqual => ordering == Less || ordering == Equal,
        TokenKind::BangEqual => ordering != Equal,
        TokenKind::EqualEqual => ordering == Equal,
        _ => unreachable!(),
    }
}

fn num_op(kind: TokenKind) -> impl Fn(f64, f64) -> f64 {
    match kind {
        TokenKind::Minus => f64::sub,
        TokenKind::Plus => f64::add,
        TokenKind::Slash => f64::div,
        TokenKind::Star => f64::mul,
        _ => unreachable!(),
    }
}
