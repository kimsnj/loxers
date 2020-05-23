use crate::{
    ast::{self, Expr, Stmt},
    error::LoxError,
    token::Token,
};
use std::collections::HashSet;

type AnalysisRes = Result<(), LoxError>;
type Bindings = Vec<(*const Token, usize)>;
type BindingsRes = Result<Bindings, LoxError>;

#[derive(Default)]
pub(crate) struct Resolver {
    env: Environment,
    bindings: Bindings,
}

impl Resolver {
    pub fn resolve(&mut self, stmts: &[Stmt]) -> BindingsRes {
        self.resolve_stmts(stmts)?;
        let mut res = Vec::new();
        std::mem::swap(&mut res, &mut self.bindings);
        Ok(res)
    }

    fn resolve_stmts(&mut self, stmts: &[Stmt]) -> AnalysisRes {
        stmts.into_iter().map(|s| self.resolve_stmt(s)).collect()
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> AnalysisRes {
        match stmt {
            Stmt::Expression(e) | Stmt::Print(e) => self.resolve_expr(e)?,
            Stmt::Var(v) => {
                if let Some(ref init) = v.init {
                    self.resolve_expr(init)?;
                }
                self.env.declare(&v.name.lexeme);
            }
            Stmt::Block(stmts) => {
                self.env.enter_scope();
                self.resolve_stmts(stmts)?;
                self.env.exit_scope();
            }
            Stmt::If(if_) => {
                self.resolve_expr(&if_.condition)?;
                self.resolve_stmt(&if_.then_branch)?;
                if let Some(ref else_stmt) = &if_.else_branch {
                    self.resolve_stmt(else_stmt)?;
                }
            }
            Stmt::While(while_) => {
                self.resolve_expr(&while_.condition)?;
                self.resolve_stmt(&while_.body)?;
            }
            Stmt::Function(f) => {
                self.env.declare(&f.name);
                self.env.enter_scope();
                f.params.iter().for_each(|p| self.env.declare(&p.lexeme));
                self.resolve_stmts(&f.body)?;
                self.env.exit_scope();
            }
            Stmt::Return(r) => self.resolve_expr(&r.value)?,
            Stmt::Class(c) => {
                self.env.declare(&c.name.lexeme);
                self.env.enter_scope();
                self.env.declare("this");
                for method in &c.methods {
                    self.resolve_fn(&method)?;
                }
                self.env.exit_scope();
            }
        }
        Ok(())
    }

    fn resolve_fn(&mut self, f: &ast::Function) -> AnalysisRes {
        self.env.declare(&f.name);
        self.env.enter_scope();
        f.params.iter().for_each(|p| self.env.declare(&p.lexeme));
        self.resolve_stmts(&f.body)?;
        self.env.exit_scope();
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> AnalysisRes {
        match expr {
            Expr::StringLit(_) | Expr::NumberLit(_) | Expr::BoolLit(_) | Expr::Nil => {}
            Expr::Grouping(e) => self.resolve_expr(&e)?,
            Expr::Unary(u) => self.resolve_expr(&u.as_ref().right)?,
            Expr::Binary(b) | Expr::Logical(b) => {
                self.resolve_expr(&b.left)?;
                self.resolve_expr(&b.right)?;
            }
            Expr::Variable(v) => self.resolve_var(v)?,
            Expr::Assign(a) => {
                self.resolve_expr(&a.expr)?;
                self.resolve_var(&a.name)?;
            }
            Expr::Call(c) => {
                self.resolve_expr(&c.callee)?;
                for arg in c.args.iter() {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Get(g) => self.resolve_expr(&g.object)?,
            Expr::Set(s) => {
                self.resolve_expr(&s.object)?;
                self.resolve_expr(&s.value)?
            }
            Expr::This(t) => self.resolve_var(t)?,
        }
        Ok(())
    }

    fn resolve_var(&mut self, t: &Token) -> AnalysisRes {
        let idx = self
            .env
            .resolve(&t.lexeme)
            .ok_or_else(|| LoxError::new("undefined variable".into(), t))?;
        self.bindings.push((t, idx));
        Ok(())
    }
}

struct Environment {
    scopes: Vec<HashSet<String>>,
}

impl Default for Environment {
    fn default() -> Self {
        let mut globals = HashSet::new();
        globals.insert("clock".into());
        Self {
            scopes: vec![globals],
        }
    }
}

impl Environment {
    pub fn declare(&mut self, name: &str) {
        self.scopes
            .iter_mut()
            .last()
            .expect("no scope found")
            .insert(name.into());
    }

    fn resolve(&self, name: &str) -> Option<usize> {
        self.scopes
            .iter()
            .enumerate()
            .rfind(|(_, s)| s.contains(name))
            .map(|(i, _)| i)
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}
