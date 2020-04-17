use crate::ast::{self, Expr};
use crate::error::LoxError;
use crate::token::Token;
use crate::token::TokenKind as tk;

pub(crate) struct Parser {
    tokens: Vec<Token>,
    last_line: usize,
}

type ParseRes = Result<Expr, LoxError>;

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        let last_line = tokens.last().map_or(0, |t| t.line);
        tokens.reverse();
        Self { tokens, last_line }
    }

    pub fn run(&mut self) -> ParseRes {
        self.expression()
    }

    fn expression(&mut self) -> ParseRes {
        self.equality()
    }

    fn binary_op<F>(&mut self, operands: F, operators: &[tk]) -> ParseRes
    where
        F: Fn(&mut Self) -> ParseRes,
    {
        let mut expr = operands(self)?;
        while let Some(operator) = self.matches(operators) {
            expr = Expr::Binary(Box::new(ast::Binary {
                left: expr,
                operator,
                right: operands(self)?,
            }))
        }
        Ok(expr)
    }

    fn equality(&mut self) -> ParseRes {
        self.binary_op(Self::comparison, &[tk::BangEqual, tk::EqualEqual])
    }

    fn comparison(&mut self) -> ParseRes {
        self.binary_op(
            Self::addition,
            &[tk::Greater, tk::GreaterEqual, tk::Less, tk::LessEqual],
        )
    }

    fn addition(&mut self) -> ParseRes {
        self.binary_op(Self::multiplication, &[tk::Plus, tk::Minus])
    }

    fn multiplication(&mut self) -> ParseRes {
        self.binary_op(Self::unary, &[tk::Star, tk::Slash])
    }

    fn unary(&mut self) -> ParseRes {
        if let Some(operator) = self.matches(&[tk::Bang, tk::Minus]) {
            Ok(Expr::Unary(Box::new(ast::Unary {
                operator,
                right: self.unary()?,
            })))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ParseRes {
        let next = self.advance();
        match next.kind {
            tk::Number(n) => Ok(Expr::NumberLit(n)),
            tk::StringLiteral(s) => Ok(Expr::StringLit(s)),
            tk::False => Ok(Expr::BoolLit(false)),
            tk::True => Ok(Expr::BoolLit(true)),
            tk::Nil => Ok(Expr::Nil),
            tk::LeftParen => {
                let expr = Expr::Grouping(Box::new(self.expression()?));
                self.expect(tk::RightParen)?;
                Ok(expr)
            }
            _ => Err(LoxError {
                message: "Unexpected token".into(),
                line: next.line,
                location: next.lexeme,
            }),
        }
    }

    fn matches(&mut self, kinds: &[tk]) -> Option<Token> {
        let next = self.peek();
        for kind in kinds {
            if next == *kind {
                return Some(self.advance());
            }
        }
        None
    }

    fn peek(&self) -> tk {
        self.tokens.last().map_or(tk::EOF, |t| t.kind.clone())
    }

    fn advance(&mut self) -> Token {
        self.tokens.pop().unwrap_or_else(|| Token {
            line: self.last_line,
            lexeme: "<EOF>".into(),
            kind: tk::EOF,
        })
    }

    fn expect(&mut self, kind: tk) -> Result<(), LoxError> {
        if self.advance().kind == kind {
            Ok(())
        } else {
            Err(LoxError {
                message: "Expected ')'".into(),
                line: 0,
                location: "".into(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_parsed(expr: &str) -> Result<String, LoxError> {
        let tokens = crate::scanner::Scanner::new(expr.into()).scan_tokens()?;
        let ast = Parser::new(tokens).run()?;
        Ok(format!("{:?}", ast))
    }

    #[test]
    fn parse_expr() {
        assert_eq!("+(1, 2)", &to_parsed("1 + 2").unwrap());
        assert_eq!("-(-(1, *(2, 3)), 2)", &to_parsed("1 - 2 * 3 - 2").unwrap());
        assert_eq!(
            "*(-(1, 2), -(3, 2))",
            &to_parsed("(1 - 2) * (3 - 2)").unwrap()
        );
        assert_eq!(
            "==(+(+(1, 2), 6), -(*(5, 2), 1))",
            &to_parsed("1 + 2 + 6 == 5 * 2 - 1").unwrap()
        );
    }

    #[test]
    fn parse_expr_err() {
        assert_eq!("Unexpected token", &to_parsed("1 +").unwrap_err().message);
        assert_eq!(
            "Unexpected token",
            &to_parsed("1 - * 3 - 2").unwrap_err().message
        );
        assert_eq!("Expected ')'", &to_parsed("(1 - 2 3").unwrap_err().message);
    }
}
