use crate::token::Token;
use std::fmt::{self, Debug};

pub(crate) enum Stmt {
    Expression(Expr),
    Print(Expr),
}

pub(crate) enum Expr {
    StringLit(String),
    NumberLit(f64),
    BoolLit(bool),
    Nil,
    Grouping(Box<Expr>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
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

impl Debug for Expr {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;

        match self {
            Grouping(e) => e.fmt(formatter),
            StringLit(s) => formatter.write_str(s),
            NumberLit(n) => formatter.write_fmt(format_args!("{}", n)),
            Unary(u) => u.fmt(formatter),
            Binary(b) => b.fmt(formatter),
            BoolLit(b) => b.fmt(formatter),
            Nil => formatter.write_str("<nil>"),
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use Stmt::*;

        match self {
            Expression(e) => e.fmt(formatter),
            Print(e) => formatter.debug_tuple("print").field(e).finish(),
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
