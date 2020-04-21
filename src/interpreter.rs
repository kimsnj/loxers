use crate::ast::{self, Expr, Stmt};
use crate::error::LoxError;
use crate::token::TokenKind;
use crate::value::Value;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::{Add, Div, Mul, Sub};

type EvaluationRes = Result<Value, crate::error::LoxError>;
type RuntimeRes = Result<(), crate::error::LoxError>;
#[derive(Default)]
pub(crate) struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn execute(&mut self, stmts: Vec<Stmt>) -> RuntimeRes {
        for s in stmts.into_iter() {
            match s {
                Stmt::Expression(e) => {
                    self.evaluate(e)?;
                }
                Stmt::Print(e) => {
                    let value = self.evaluate(e)?;
                    println!("{}", value);
                }
                Stmt::Var(v) => {
                    let value = self.evaluate(v.init.unwrap_or(Expr::Nil))?;
                    self.env.declare(v.name.lexeme, value);
                }
                Stmt::Block(stmts) => {
                    self.env.enter_scope();
                    self.execute(stmts)?;
                    self.env.exit_scope();
                }
            }
        }
        Ok(())
    }

    pub fn evaluate(&mut self, expr: Expr) -> EvaluationRes {
        match expr {
            Expr::StringLit(_) | Expr::NumberLit(_) | Expr::BoolLit(_) | Expr::Nil => {
                Ok(expr.into())
            }
            Expr::Grouping(e) => self.evaluate(*e),
            Expr::Unary(unary) => self.evaluate_unary(*unary),
            Expr::Binary(binary) => self.evaluate_binary(*binary),
            Expr::Variable(t) => self
                .env
                .get(&t.lexeme)
                .ok_or_else(|| LoxError::new("unknown variable".into(), &t)),
            Expr::Assign(a) => {
                let name = a.name;
                let value = self.evaluate(a.expr)?;
                self.env
                    .assign(&name.lexeme, value)
                    .ok_or_else(|| LoxError::new("Unknown variable".into(), &name))
            }
        }
    }

    fn evaluate_unary(&mut self, unary: ast::Unary) -> EvaluationRes {
        let operator = unary.operator;
        let right = self.evaluate(unary.right)?;
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

    fn evaluate_binary(&mut self, binary: ast::Binary) -> EvaluationRes {
        use Value::*;

        let operator = binary.operator;
        let left = self.evaluate(binary.left)?;
        let right = self.evaluate(binary.right)?;
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

#[derive(Debug)]
struct Environment {
    scopes: Vec<HashMap<String, Value>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }
}

impl Environment {
    fn declare(&mut self, name: String, value: Value) {
        self.scopes
            .iter_mut()
            .last()
            .expect("no scope found")
            .insert(name, value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|m| m.get(name).cloned())
            .next()
    }

    fn assign(&mut self, name: &str, value: Value) -> Option<Value> {
        let v = self
            .scopes
            .iter_mut()
            .rev()
            .flat_map(|m| m.get_mut(name))
            .next()?;
        *v = value.clone();
        Some(value)
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}
