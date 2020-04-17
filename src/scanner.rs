use crate::error::LoxError;
use crate::token::{Token, TokenKind};
use phf::phf_map;
use std::str::FromStr;

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "and" => TokenKind::And,
    "class" => TokenKind::Class,
    "else" => TokenKind::Else,
    "false" => TokenKind::False,
    "for" => TokenKind::For,
    "fun" => TokenKind::Fun,
    "if" => TokenKind::If,
    "nil" => TokenKind::Nil,
    "or" => TokenKind::Or,
    "print" => TokenKind::Print,
    "return" => TokenKind::Return,
    "super" => TokenKind::Super,
    "this" => TokenKind::This,
    "true" => TokenKind::True,
    "var" => TokenKind::Var,
    "while" => TokenKind::While,
};

#[derive(Debug)]
pub(crate) struct Scanner {
    tokens: Vec<Token>,
    source: Vec<u8>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub(crate) fn new(source: Vec<u8>) -> Self {
        Self {
            tokens: Vec::new(),
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    fn at(&self, i: usize) -> Option<char> {
        self.source.get(i).map(|&b| Into::into(b))
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.at(self.current - 1)
    }

    fn peek(&self) -> Option<char> {
        self.at(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        self.at(self.current + 1)
    }

    fn matches(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn add_token(&mut self, kind: crate::token::TokenKind) {
        self.tokens.push(Token {
            kind,
            lexeme: self.lexeme(self.start, self.current),
            line: self.line,
        })
    }

    fn lexeme(&self, start: usize, end: usize) -> String {
        std::str::from_utf8(&self.source[start..end])
            .unwrap()
            .into()
    }

    fn err(&self, message: String) -> LoxError {
        LoxError {
            line: self.line,
            location: String::new(),
            message,
        }
    }

    fn advance_until(&mut self, c: char) -> bool {
        loop {
            let next = self.peek();
            if next == Some(c) {
                break true;
            }
            if next == Some('\n') {
                self.line += 1;
            }
            if next.is_none() {
                break false;
            }
            self.advance();
        }
    }

    fn string(&mut self) -> Result<TokenKind, LoxError> {
        if self.advance_until('"') {
            self.advance();
            Ok(TokenKind::StringLiteral(
                self.lexeme(self.start + 1, self.current - 1),
            ))
        } else {
            Err(self.err("Unterminated string".into()))
        }
    }

    fn number(&mut self) -> Result<TokenKind, LoxError> {
        let is_digit = |opt: Option<char>| opt.map_or(false, |c| c.is_ascii_digit());
        while is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == Some('.') && is_digit(self.peek_next()) {
            self.advance();
            while is_digit(self.peek()) {
                self.advance();
            }
        }

        Ok(TokenKind::Number(
            f64::from_str(&self.lexeme(self.start, self.current))
                .map_err(|_| self.err("Invalid number".into()))?,
        ))
    }

    fn identifier(&mut self) -> TokenKind {
        while self.peek().map_or(false, |c| c.is_ascii_alphanumeric()) {
            self.advance();
        }
        let value = self.lexeme(self.start, self.current);
        KEYWORDS
            .get(value.as_str())
            .cloned()
            .unwrap_or(TokenKind::Identifier(value))
    }

    fn advance_token(&mut self) -> Result<(), LoxError> {
        use TokenKind::*;
        if let Some(c) = self.advance() {
            let kind = match c {
                '(' => LeftParen,
                ')' => RightParen,
                '{' => LeftBrace,
                '}' => RightBrace,
                ',' => Comma,
                '.' => Dot,
                '-' => Minus,
                '+' => Plus,
                ';' => Semicolon,
                '*' => Star,
                '!' if self.matches('=') => BangEqual,
                '!' => Bang,
                '=' if self.matches('=') => EqualEqual,
                '=' => Equal,
                '<' if self.matches('=') => LessEqual,
                '<' => Less,
                '>' if self.matches('=') => GreaterEqual,
                '>' => Greater,
                ' ' | '\t' | '\r' => return Ok(()),
                '\n' => {
                    self.line += 1;
                    return Ok(());
                }
                '/' if self.matches('/') => {
                    self.advance_until('\n');
                    return Ok(());
                }
                '/' => Slash,
                '"' => self.string()?,
                '0'..='9' => self.number()?,
                'a'..='z' | 'A'..='Z' => self.identifier(),
                c => return Err(self.err(format!("unexpected character {}", c))),
            };
            self.add_token(kind);
        }
        Ok(())
    }

    pub(crate) fn scan_tokens(&mut self) -> Result<Vec<Token>, LoxError> {
        self.tokens.clear();

        while !self.is_at_end() {
            self.start = self.current;
            self.advance_token()?;
        }

        Ok(self.tokens.clone())
    }
}
