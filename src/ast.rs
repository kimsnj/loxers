use crate::token::Token;
use std::fmt::{self, Debug};
use std::rc::Rc;

#[derive(Clone)]
pub(crate) enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(VarDeclaration),
    Block(Vec<Stmt>),
    If(Box<If>),
    While(Box<While>),
    Function(Rc<Function>),
    Return(Box<Return>),
    Class(Box<Class>),
}

#[derive(Clone)]
pub(crate) struct VarDeclaration {
    pub name: Token,
    pub init: Option<Expr>,
}

#[derive(Clone)]
pub(crate) struct If {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}
#[derive(Clone)]
pub(crate) struct While {
    pub condition: Expr,
    pub body: Stmt,
}
#[derive(Clone)]
pub(crate) struct Function {
    pub name: String,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}
#[derive(Clone)]
pub(crate) struct Return {
    pub keyword: Token,
    pub value: Expr,
}

#[derive(Clone)]
pub(crate) struct Class {
    pub name: Token,
    pub methods: Vec<Rc<Function>>,
}

#[derive(Clone)]
pub(crate) enum Expr {
    StringLit(String),
    NumberLit(f64),
    BoolLit(bool),
    Nil,
    Grouping(Box<Expr>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Logical(Box<Binary>),
    Variable(Token),
    Assign(Box<Assignment>),
    Call(Box<Call>),
    Get(Box<Get>),
    Set(Box<Set>),
    This(Token),
}

#[derive(Clone)]
pub(crate) struct Unary {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Clone)]
pub(crate) struct Binary {
    pub operator: Token,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Clone)]
pub(crate) struct Assignment {
    pub name: Token,
    pub expr: Expr,
}

#[derive(Clone)]
pub(crate) struct Call {
    pub callee: Expr,
    pub paren: Token,
    pub args: Vec<Expr>,
}

#[derive(Clone)]
pub(crate) struct Get {
    pub object: Expr,
    pub name: Token,
}

#[derive(Clone)]
pub(crate) struct Set {
    pub object: Expr,
    pub name: Token,
    pub value: Expr,
}

impl Debug for Unary {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_tuple(&self.operator.lexeme)
            .field(&self.right)
            .finish()
    }
}

impl Debug for Binary {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_tuple(&self.operator.lexeme)
            .field(&self.left)
            .field(&self.right)
            .finish()
    }
}

impl Debug for Assignment {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_tuple("=")
            .field(&self.name.lexeme)
            .field(&self.expr)
            .finish()
    }
}

impl Debug for Call {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if self.args.is_empty() {
            formatter.write_fmt(format_args!("{:?}()", self.callee))
        } else {
            let mut tuple = formatter.debug_tuple(&format!("{:?}", self.callee));
            for arg in &self.args {
                tuple.field(&arg);
            }
            tuple.finish()
        }
    }
}

impl Debug for Get {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_tuple(".")
            .field(&self.object)
            .field(&self.name)
            .finish()
    }
}

impl Debug for Set {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_tuple(".=")
            .field(&self.object)
            .field(&self.name)
            .field(&self.value)
            .finish()
    }
}

impl Debug for Expr {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;

        match self {
            Grouping(e) => e.fmt(formatter),
            StringLit(s) => s.fmt(formatter),
            NumberLit(n) => formatter.write_fmt(format_args!("{}", n)),
            Unary(u) => u.fmt(formatter),
            Binary(b) => b.fmt(formatter),
            Logical(b) => b.fmt(formatter),
            BoolLit(b) => b.fmt(formatter),
            Nil => formatter.write_str("<nil>"),
            Variable(t) => formatter.write_str(&t.lexeme),
            Assign(a) => a.fmt(formatter),
            Call(c) => c.fmt(formatter),
            Get(g) => g.fmt(formatter),
            Set(s) => s.fmt(formatter),
            This(_) => formatter.write_str("this"),
        }
    }
}

impl Debug for VarDeclaration {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_tuple("var")
            .field(&self.name.lexeme)
            .field(&self.init)
            .finish()
    }
}

impl Debug for While {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_tuple("while")
            .field(&self.condition)
            .field(&self.body)
            .finish()
    }
}

impl Debug for If {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let mut debug = formatter.debug_tuple("if");
        debug.field(&self.condition).field(&self.then_branch);
        if let Some(else_stmt) = &self.else_branch {
            debug.field(else_stmt);
        }
        debug.finish()
    }
}

impl Debug for Function {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let mut debug = formatter.debug_tuple(&self.name);
        for param in self.params.iter() {
            debug.field(&param.lexeme);
        }
        debug.field(&self.body);
        debug.finish()
    }
}

impl Debug for Return {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.debug_tuple("return").field(&self.value).finish()
    }
}

impl Debug for Class {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_tuple("class")
            .field(&self.name)
            .field(&self.methods)
            .finish()
    }
}

impl Debug for Stmt {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use Stmt::*;

        match self {
            Expression(e) => e.fmt(formatter),
            Print(e) => formatter.debug_tuple("print").field(e).finish(),
            Var(v) => v.fmt(formatter),
            Block(stmts) => stmts.fmt(formatter),
            If(if_stmt) => if_stmt.fmt(formatter),
            While(while_stmt) => while_stmt.fmt(formatter),
            Function(f) => f.fmt(formatter),
            Return(r) => r.fmt(formatter),
            Class(c) => c.fmt(formatter),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    #[test]
    fn test_debug() {
        let expr = Expr::Binary(Box::new(Binary {
            operator: Token {
                kind: TokenKind::Plus,
                lexeme: "+".into(),
                line: 0,
            },
            left: Expr::NumberLit(12.0),
            right: Expr::Unary(Box::new(Unary {
                operator: Token {
                    kind: TokenKind::Minus,
                    lexeme: "-".into(),
                    line: 0,
                },
                right: Expr::NumberLit(45.5),
            })),
        }));
        assert_eq!(format!("{:?}", expr), "+(12, -(45.5))");
        assert_eq!(
            format!("{:#?}", expr),
            "+(\n    12,\n    -(\n        45.5,\n    ),\n)"
        );
    }
}
