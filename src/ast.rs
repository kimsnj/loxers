use crate::token::Token;
use std::fmt::{self, Debug};

enum Expr {
    Grouping(Box<Expr>),
    StringLit(String),
    NumberLit(f64),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
}

struct Unary {
    operator: Token,
    right: Expr,
}

struct Binary {
    operator: Token,
    left: Expr,
    right: Expr,
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
            Grouping(e) => formatter.debug_tuple("").field(e).finish(),
            StringLit(s) => formatter.write_str(s),
            NumberLit(n) => formatter.write_fmt(format_args!("{}", n)),
            Unary(u) => u.fmt(formatter),
            Binary(b) => b.fmt(formatter),
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
