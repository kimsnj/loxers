use crate::ast::{self, Expr, Stmt};
use crate::error::LoxError;
use crate::token::Token;
use crate::token::TokenKind as tk;

pub(crate) struct Parser {
    tokens: Vec<Token>,
    last_line: usize,
}

type ParseRes = Result<Vec<Stmt>, LoxError>;
type StmtRes = Result<Stmt, LoxError>;
type ExprRes = Result<Expr, LoxError>;

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        let last_line = tokens.last().map_or(0, |t| t.line);
        tokens.reverse();
        Self { tokens, last_line }
    }

    pub fn run(&mut self) -> ParseRes {
        let mut res = Vec::new();
        while !self.tokens.is_empty() {
            res.push(self.declaration()?);
        }
        Ok(res)
    }

    fn declaration(&mut self) -> StmtRes {
        match self.peek() {
            tk::Var => self.var_declaration(),
            _ => self.statement(),
        }
    }

    fn statement(&mut self) -> StmtRes {
        match self.peek() {
            tk::Print => self.print_stmt(),
            tk::LeftBrace => self.block(),
            tk::If => self.if_stmt(),
            tk::While => self.while_stmt(),
            tk::For => self.for_stmt(),
            _ => self.expr_stmt(),
        }
    }

    fn block(&mut self) -> StmtRes {
        self.expect(tk::LeftBrace)?;
        let mut stmts = Vec::new();
        while self.peek() != tk::RightBrace && self.peek() != tk::EOF {
            stmts.push(self.declaration()?);
        }
        self.expect(tk::RightBrace)?;
        Ok(Stmt::Block(stmts))
    }

    fn var_declaration(&mut self) -> StmtRes {
        self.expect(tk::Var)?;
        let name = self.advance();
        match name.kind {
            tk::Identifier => {
                let init = if let Some(_) = self.matches(&[tk::Equal]) {
                    Some(self.expression()?)
                } else {
                    None
                };
                self.expect(tk::Semicolon)?;
                Ok(Stmt::Var(ast::VarDeclaration { name, init }))
            }
            _ => Err(LoxError::new("Expected variable name".into(), &name)),
        }
    }

    fn print_stmt(&mut self) -> StmtRes {
        self.expect(tk::Print)?;
        let value = self.expression()?;
        self.expect(tk::Semicolon)?;
        Ok(Stmt::Print(value))
    }

    fn if_stmt(&mut self) -> StmtRes {
        self.expect(tk::If)?;
        self.expect(tk::LeftParen)?;
        let condition = self.expression()?;
        self.expect(tk::RightParen)?;
        let then_branch = self.statement()?;
        let else_branch = if self.matches(&[tk::Else]).is_some() {
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Stmt::If(Box::new(ast::If {
            condition,
            then_branch,
            else_branch,
        })))
    }

    fn while_stmt(&mut self) -> StmtRes {
        self.expect(tk::While)?;
        self.expect(tk::LeftParen)?;
        let condition = self.expression()?;
        self.expect(tk::RightParen)?;
        let body = self.statement()?;
        Ok(Stmt::While(Box::new(ast::While { condition, body })))
    }

    fn for_stmt(&mut self) -> StmtRes {
        self.expect(tk::For)?;

        self.expect(tk::LeftParen)?;
        let initialization = match self.peek() {
            tk::Var => Some(self.var_declaration()?),
            tk::Semicolon => {
                self.advance();
                None
            }
            _ => Some(self.expr_stmt()?),
        };

        let condition = self.expression()?;
        self.expect(tk::Semicolon)?;

        let increment = if self.peek() == tk::Semicolon {
            self.advance();
            None
        } else {
            Some(Stmt::Expression(self.expression()?))
        };
        self.expect(tk::RightParen)?;

        let body = self.statement()?;
        let nil_statement = Stmt::Expression(Expr::Nil);
        Ok(Stmt::Block(vec![
            initialization.unwrap_or(nil_statement.clone()),
            Stmt::While(Box::new(ast::While {
                condition,
                body: Stmt::Block(vec![body, increment.unwrap_or(nil_statement)]),
            })),
        ]))
    }

    fn expr_stmt(&mut self) -> StmtRes {
        let expr = self.expression()?;
        self.expect(tk::Semicolon)?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> ExprRes {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprRes {
        let expr = self.or()?;
        if let Some(t) = self.matches(&[tk::Equal]) {
            let value = self.assignment()?;
            if let Expr::Variable(ident) = expr {
                Ok(Expr::Assign(Box::new(ast::Assignment {
                    name: ident,
                    expr: value,
                })))
            } else {
                Err(LoxError::new("Invalid assignment target".into(), &t))
            }
        } else {
            Ok(expr)
        }
    }

    fn binary_op<F>(&mut self, operands: F, operators: &[tk]) -> ExprRes
    where
        F: Fn(&mut Self) -> ExprRes,
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

    fn logical_op<F>(&mut self, operands: F, operators: &[tk]) -> ExprRes
    where
        F: Fn(&mut Self) -> ExprRes,
    {
        let mut expr = operands(self)?;
        while let Some(operator) = self.matches(operators) {
            expr = Expr::Logical(Box::new(ast::Binary {
                left: expr,
                operator,
                right: operands(self)?,
            }))
        }
        Ok(expr)
    }

    fn or(&mut self) -> ExprRes {
        self.logical_op(Self::and, &[tk::Or])
    }

    fn and(&mut self) -> ExprRes {
        self.logical_op(Self::equality, &[tk::And])
    }

    fn equality(&mut self) -> ExprRes {
        self.binary_op(Self::comparison, &[tk::BangEqual, tk::EqualEqual])
    }

    fn comparison(&mut self) -> ExprRes {
        self.binary_op(
            Self::addition,
            &[tk::Greater, tk::GreaterEqual, tk::Less, tk::LessEqual],
        )
    }

    fn addition(&mut self) -> ExprRes {
        self.binary_op(Self::multiplication, &[tk::Plus, tk::Minus])
    }

    fn multiplication(&mut self) -> ExprRes {
        self.binary_op(Self::unary, &[tk::Star, tk::Slash])
    }

    fn unary(&mut self) -> ExprRes {
        if let Some(operator) = self.matches(&[tk::Bang, tk::Minus]) {
            Ok(Expr::Unary(Box::new(ast::Unary {
                operator,
                right: self.unary()?,
            })))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ExprRes {
        let next = self.advance();
        match next.kind {
            tk::Number(n) => Ok(Expr::NumberLit(n)),
            tk::StringLiteral(s) => Ok(Expr::StringLit(s)),
            tk::False => Ok(Expr::BoolLit(false)),
            tk::True => Ok(Expr::BoolLit(true)),
            tk::Nil => Ok(Expr::Nil),
            tk::Identifier => Ok(Expr::Variable(next)),
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
                message: format!("Expected {:?}", kind),
                line: 0,
                location: "".into(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_parsed_expr(expr: &str) -> Result<String, LoxError> {
        let tokens = crate::scanner::Scanner::new(expr.into()).scan_tokens()?;
        let ast = Parser::new(tokens).expression()?;
        Ok(format!("{:?}", ast))
    }

    fn to_parsed_program(program: &str) -> Result<String, LoxError> {
        let tokens = crate::scanner::Scanner::new(program.into()).scan_tokens()?;
        let ast = Parser::new(tokens).run()?;
        Ok(format!("{:?}", ast))
    }

    #[test]
    fn parse_expr() {
        assert_eq!("+(1, 2)", &to_parsed_expr("1 + 2").unwrap());
        assert_eq!(
            "-(-(1, *(2, 3)), 2)",
            &to_parsed_expr("1 - 2 * 3 - 2").unwrap()
        );
        assert_eq!(
            "*(-(1, 2), -(3, 2))",
            &to_parsed_expr("(1 - 2) * (3 - 2)").unwrap()
        );
        assert_eq!(
            "==(+(+(1, 2), 6), -(*(5, 2), 1))",
            &to_parsed_expr("1 + 2 + 6 == 5 * 2 - 1").unwrap()
        );
        assert_eq!(
            "=(\"a\", or(true, and(false, <nil>)))",
            &to_parsed_expr("a = true or false and nil").unwrap()
        );
    }

    #[test]
    fn parse_expr_err() {
        assert_eq!(
            "Unexpected token",
            &to_parsed_expr("1 +").unwrap_err().message
        );
        assert_eq!(
            "Unexpected token",
            &to_parsed_expr("1 - * 3 - 2").unwrap_err().message
        );
        assert_eq!(
            "Expected RightParen",
            &to_parsed_expr("(1 - 2 3").unwrap_err().message
        );
    }

    #[test]
    fn parse() {
        assert_eq!(
            "[print(+(1, 2))]",
            &to_parsed_program("print 1 + 2;").unwrap()
        );

        assert_eq!(
            "[var(\"a\", None), var(\"b\", Some(3))]",
            &to_parsed_program("var a; var b = 3;").unwrap()
        );

        assert_eq!(
            "[[1, print(3)]]",
            &to_parsed_program("{1;print 3;}").unwrap()
        );

        assert_eq!(
            "[if(true, print(1), if(<nil>, print(2), print(3)))]",
            &to_parsed_program("if(true) print 1; else if (nil) print 2; else print 3;").unwrap()
        );
    }
}
