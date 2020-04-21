use crate::token::Token;
use std::fmt::{self, Debug};

pub(crate) enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(VarDeclaration),
    Block(Vec<Stmt>),
}

pub(crate) struct VarDeclaration {
    pub name: Token,
    pub init: Option<Expr>,
}

pub(crate) enum Expr {
    StringLit(String),
    NumberLit(f64),
    BoolLit(bool),
    Nil,
    Grouping(Box<Expr>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Variable(Token),
    Assign(Box<Assignment>),
}

pub(crate) struct Unary {
    pub operator: Token,
    pub right: Expr,
}

pub(crate) struct Binary {
    pub operator: Token,
    pub left: Expr,
    pub right: Expr,
}

pub(crate) struct Assignment {
    pub name: Token,
    pub expr: Expr,
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

impl Debug for Expr {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;

        match self {
            Grouping(e) => e.fmt(formatter),
            StringLit(s) => s.fmt(formatter),
            NumberLit(n) => formatter.write_fmt(format_args!("{}", n)),
            Unary(u) => u.fmt(formatter),
            Binary(b) => b.fmt(formatter),
            BoolLit(b) => b.fmt(formatter),
            Nil => formatter.write_str("<nil>"),
            Variable(t) => formatter.write_str(&t.lexeme),
            Assign(a) => a.fmt(formatter),
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

impl Debug for Stmt {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use Stmt::*;

        match self {
            Expression(e) => e.fmt(formatter),
            Print(e) => formatter.debug_tuple("print").field(e).finish(),
            Var(v) => v.fmt(formatter),
            Block(stmts) => stmts.fmt(formatter),
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
