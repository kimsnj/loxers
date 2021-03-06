use crate::{
    ast::{self, Expr},
    callable::{self, Callable},
    error::LoxError,
    interpreter::Interpreter,
};
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::convert::{From, TryFrom};
use std::rc::Rc;

#[derive(Clone)]
pub(crate) enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Rc<dyn Callable>),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
}

impl Value {
    pub fn get(self, field: &str) -> Result<Value, LoxError> {
        if let Value::Instance(ref i) = self {
            i.borrow()
                .fields
                .get(field)
                .cloned()
                .or_else(|| {
                    i.borrow()
                        .class
                        .methods
                        .get(field)
                        .cloned()
                        .map(|function| {
                            Value::Callable(Rc::new(Method {
                                function,
                                instance: self.clone(),
                            }))
                        })
                })
                .ok_or_else(|| LoxError::msg(format!("no field {}", field)))
        } else {
            Err(LoxError::msg(format!("Expected instance found: {}", self)))
        }
    }
}

pub(crate) struct Class {
    pub name: String,
    pub methods: HashMap<String, Rc<callable::Function>>,
}

impl Class {
    pub fn new_value(declaration: &ast::Class, interpreter: &Interpreter) -> Value {
        let name = declaration.name.lexeme.clone();
        let methods: HashMap<_, _> = declaration
            .methods
            .iter()
            .cloned()
            .map(|declaration| {
                (
                    declaration.name.clone(),
                    Rc::new(callable::Function {
                        declaration,
                        interpreter: interpreter.clone(),
                    }),
                )
            })
            .collect();

        Value::Class(Rc::new(Class { name, methods }))
    }

    pub fn get_init(&self) -> Option<Rc<callable::Function>> {
        self.methods.get("init").cloned()
    }
}

#[derive(Clone)]
pub(crate) struct Instance {
    pub class: Rc<Class>,
    fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new_value(class: Rc<Class>) -> Value {
        Value::Instance(Rc::new(RefCell::new(Self {
            class,
            fields: HashMap::new(),
        })))
    }

    pub fn set(&mut self, field: String, value: Value) -> Value {
        self.fields.insert(field, value.clone());
        value
    }
}

pub(crate) struct Method {
    pub instance: Value,
    pub function: Rc<callable::Function>,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::Boolean(b) => b.fmt(f),
            Value::Number(n) => {
                let mut s = format!("{}", n);
                if s.ends_with(".0") {
                    s.truncate(s.len() - 2);
                }
                f.write_str(&s)
            }
            Value::String(s) => f.write_str(s),
            Value::Callable(c) => f.write_fmt(format_args!("<callable {}>", c.name())),
            Value::Class(c) => f.write_fmt(format_args!("<class {}>", &c.name)),
            Value::Instance(i) => {
                f.write_fmt(format_args!("<instance {}>", &i.borrow().class.name))
            }
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self))
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        use Value::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Boolean(b1), Boolean(b2)) => b1 == b2,
            (Number(n1), Number(n2)) => n1 == n2,
            (String(s1), String(s2)) => s1 == s2,
            (Callable(fp1), Callable(fp2)) => Rc::ptr_eq(fp1, fp2),
            (_, _) => false,
        }
    }
}

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
        use Value::Number;
        if let (Number(ns), Number(no)) = (self, other) {
            ns.partial_cmp(no)
        } else {
            None
        }
    }
}

impl From<&Expr> for Value {
    /// Converts literal to values
    /// Panics if called with non-literals
    fn from(literal: &Expr) -> Self {
        match literal {
            Expr::StringLit(s) => Value::String(s.clone()),
            Expr::NumberLit(n) => Value::Number(*n),
            Expr::BoolLit(b) => Value::Boolean(*b),
            Expr::Nil => Value::Nil,
            _ => panic!("Conversion limited to literal values"),
        }
    }
}

impl From<callable::Function> for Value {
    fn from(f: callable::Function) -> Self {
        Value::Callable(std::rc::Rc::new(f))
    }
}

impl TryFrom<Value> for f64 {
    type Error = LoxError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Number(n) = value {
            Ok(n)
        } else {
            Err(LoxError::msg(format!("Expected number found: {}", value)))
        }
    }
}

impl TryFrom<Value> for String {
    type Error = LoxError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::String(s) = value {
            Ok(s)
        } else {
            Err(LoxError::msg(format!("Expected string found: {}", value)))
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = LoxError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(b) => Ok(b),
            Value::Nil => Ok(false),
            _ => Err(LoxError::msg(format!("Expected boolean found: {}", value))),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a dyn Callable {
    type Error = LoxError;
    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Callable(c) => Ok(c.as_ref()),
            Value::Class(c) => Ok(c),
            _ => Err(LoxError::msg(format!("Expected callable found: {}", value))),
        }
    }
}
impl<'a> TryFrom<&'a mut Value> for RefMut<'a, Instance> {
    type Error = LoxError;
    fn try_from(value: &'a mut Value) -> Result<Self, Self::Error> {
        if let Value::Instance(i) = value {
            Ok(i.borrow_mut())
        } else {
            Err(LoxError::msg(format!("Expected instance found: {}", value)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eq() {
        assert_eq!(Value::Nil, Value::Nil);
        assert_eq!(Value::Boolean(true), Value::Boolean(true));
        assert_ne!(Value::Boolean(false), Value::Boolean(true));
        assert_ne!(Value::String("3".into()), Value::Number(3.0));
    }

    #[test]
    fn test_partial_ord() {
        use std::cmp::Ordering::*;
        use Value::*;

        assert!(Nil.partial_cmp(&Nil) == None);
        assert!(Number(3.2).partial_cmp(&Number(3.0)) == Some(Greater));
        assert!(Number(3.2).partial_cmp(&Number(3.2)) == Some(Equal));
        assert!(Number(-3.2).partial_cmp(&Number(3.2)) == Some(Less));
        assert!(String("a".into()).partial_cmp(&String("b".into())) == None);
    }
    #[test]
    fn test_partial_display() {
        use Value::*;
        assert_eq!(Nil.to_string(), "nil".to_string());
        assert_eq!(Number(2.0).to_string(), "2".to_string());
        assert_eq!(Number(2.54).to_string(), "2.54".to_string());
        assert_eq!(String("abc".into()).to_string(), "abc".to_string());
        assert_eq!(Boolean(false).to_string(), "false".to_string());
    }
}
