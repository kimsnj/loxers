use crate::ast::{self, Expr, Stmt};
use crate::callable;
use crate::error::{ControlFlow, LoxError};
use crate::token::{Token, TokenKind};
use crate::value::Value;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::{Add, Div, Mul, Sub};

type RuntimeRes = Result<(), LoxError>;
type ControlFlowRes = Result<(), ControlFlow>;
type EvaluationRes = Result<Value, ControlFlow>;

#[derive(Default)]
pub(crate) struct Interpreter {
    pub env: Environment,
}

impl Interpreter {
    fn new_with_globals(&self) -> Self {
        Self {
            env: Environment {
                scopes: vec![self.env.scopes[0].clone()],
            },
        }
    }

    pub fn execute(&mut self, stmts: &[Stmt]) -> RuntimeRes {
        self.execute_stmts(stmts).map_err(|e| match e {
            ControlFlow::Error(e) => e,
            ControlFlow::Return(t, v) => {
                LoxError::new(format!("Returning {} outside of function", v), &t)
            }
        })
    }

    pub fn execute_stmts(&mut self, stmts: &[Stmt]) -> ControlFlowRes {
        stmts.into_iter().map(|s| self.execute_single(s)).collect()
    }

    fn execute_single(&mut self, stmt: &Stmt) -> ControlFlowRes {
        match stmt {
            Stmt::Expression(e) => {
                self.evaluate(e)?;
            }
            Stmt::Print(e) => {
                let value = self.evaluate(e)?;
                println!("{}", value);
            }
            Stmt::Var(v) => {
                let value = self.evaluate(v.init.as_ref().unwrap_or(&Expr::Nil))?;
                self.env.declare(&v.name.lexeme, value);
            }
            Stmt::Block(stmts) => {
                self.env.enter_scope();
                self.execute(stmts)?;
                self.env.exit_scope();
            }
            Stmt::If(if_) => {
                if self.evaluate(&if_.condition)?.try_into()? {
                    self.execute_single(&if_.then_branch)?;
                } else if let Some(else_stmt) = &if_.else_branch {
                    self.execute_single(&else_stmt)?;
                }
            }
            Stmt::While(while_) => {
                while self.evaluate(&while_.condition)?.try_into()? {
                    self.execute_single(&while_.body)?;
                }
            }
            Stmt::Function(f) => {
                self.env.declare(&f.name, Value::from(f.as_ref()));
            }
            Stmt::Return(r) => {
                return Err(ControlFlow::Return(
                    r.keyword.clone(),
                    self.evaluate(&r.value)?,
                ))
            }
        }
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> EvaluationRes {
        match expr {
            Expr::StringLit(_) | Expr::NumberLit(_) | Expr::BoolLit(_) | Expr::Nil => {
                Ok(expr.into())
            }
            Expr::Grouping(e) => self.evaluate(&e),
            Expr::Unary(unary) => self.evaluate_unary(&unary),
            Expr::Binary(binary) => self.evaluate_binary(&binary),
            Expr::Logical(binary) => self.evaluate_logical(&binary),
            Expr::Variable(t) => self.read_var(&t),
            Expr::Assign(a) => self.evaluate_assignment(&a.name, &a.expr),
            Expr::Call(c) => self.evaluate_call(&c),
        }
    }

    fn evaluate_call(&mut self, call: &ast::Call) -> EvaluationRes {
        let func = self.evaluate(&call.callee)?;
        let func: &dyn crate::callable::Callable = (&func).try_into()?;
        if func.arity() != call.args.len() {
            Err(LoxError::new(
                format!("Invalid number of parameters to {:?}", func.name()),
                &call.paren,
            )
            .into())
        } else {
            let args = call
                .args
                .iter()
                .map(|e| self.evaluate(&e))
                .collect::<Result<_, _>>()?;

            let mut interpreter = self.new_with_globals();
            self.env.enter_scope();
            let res = func.call(&mut interpreter, args);
            self.env.exit_scope();
            res.map_err(|e| e.into())
        }
    }

    fn evaluate_unary(&mut self, unary: &ast::Unary) -> EvaluationRes {
        let operator = &unary.operator;
        let right = self.evaluate(&unary.right)?;
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
    fn evaluate_logical(&mut self, binary: &ast::Binary) -> EvaluationRes {
        let operator = &binary.operator;
        assert!(operator.kind == TokenKind::Or || operator.kind == TokenKind::And);

        let left = self.evaluate(&binary.left)?;
        let is_truthy: bool = left.clone().try_into()?;

        if operator.kind == TokenKind::Or && is_truthy {
            Ok(left)
        } else {
            self.evaluate(&binary.right)
        }
    }

    fn evaluate_binary(&mut self, binary: &ast::Binary) -> EvaluationRes {
        use Value::*;

        let operator = &binary.operator;
        let left = self.evaluate(&binary.left)?;
        let right = self.evaluate(&binary.right)?;
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
                Ok(Boolean(to_bool(&operator.kind, ordering)))
            }

            TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                let left: f64 = left.try_into()?;
                let right: f64 = right.try_into()?;
                Ok(Number(num_op(&operator.kind)(left, right)))
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
                )
                .into()),
            },
            _ => unreachable!(),
        }
    }

    fn read_var(&self, t: &Token) -> EvaluationRes {
        self.env
            .get(&t.lexeme)
            .ok_or_else(|| LoxError::new("unknown variable".into(), &t).into())
    }

    fn evaluate_assignment(&mut self, name: &Token, expr: &Expr) -> EvaluationRes {
        let value = self.evaluate(expr)?;
        self.env
            .assign(&name.lexeme, value)
            .ok_or_else(|| LoxError::new("Unknown variable".into(), &name).into())
    }
}

fn to_bool(kind: &TokenKind, ordering: std::cmp::Ordering) -> bool {
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

fn num_op(kind: &TokenKind) -> impl Fn(f64, f64) -> f64 {
    match kind {
        TokenKind::Minus => f64::sub,
        TokenKind::Plus => f64::add,
        TokenKind::Slash => f64::div,
        TokenKind::Star => f64::mul,
        _ => unreachable!(),
    }
}

type Assignments = HashMap<String, Value>;

#[derive(Debug)]
pub(crate) struct Environment {
    scopes: Vec<Assignments>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scopes: vec![Self::init_globals()],
        }
    }
}

impl Environment {
    fn init_globals() -> Assignments {
        let mut m = HashMap::new();
        m.insert("clock".to_string(), callable::Clock::value());
        m
    }

    pub fn declare(&mut self, name: &str, value: Value) {
        self.scopes
            .iter_mut()
            .last()
            .expect("no scope found")
            .insert(name.into(), value);
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
