use crate::ast;
use crate::error::{ControlFlow, LoxError};
use crate::interpreter::Interpreter;
use crate::value::{Class, Instance, Method, Value};
use std::rc::Rc;
pub(crate) trait Callable {
    fn arity(&self) -> usize;
    fn name(&self) -> &str;
    fn call(&self, args: Vec<Value>) -> Result<Value, LoxError>;
}

pub(crate) struct Clock;

impl Clock {
    pub fn value() -> Value {
        Value::Callable(Rc::new(Clock {}))
    }
}

impl Callable for Clock {
    fn name(&self) -> &str {
        "clock"
    }

    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: Vec<Value>) -> Result<Value, LoxError> {
        use std::time::SystemTime;
        Ok(Value::Number(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .expect("failed to get time")
                .as_secs() as f64,
        ))
    }
}

pub(crate) struct Function {
    pub declaration: Rc<ast::Function>,
    pub interpreter: Interpreter,
}

impl Function {
    fn execute(&self, mut interpreter: Interpreter, args: Vec<Value>) -> Result<Value, LoxError> {
        for (name, value) in self.declaration.params.iter().zip(args.into_iter()) {
            interpreter.env.declare(&name.lexeme, value);
        }
        match interpreter.execute_stmts(&self.declaration.body) {
            Ok(()) => Ok(Value::Nil),
            Err(ControlFlow::Return(_, v)) => Ok(v),
            Err(ControlFlow::Error(e)) => Err(e),
        }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn name(&self) -> &str {
        &self.declaration.name
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, LoxError> {
        let mut interpreter = self.interpreter.clone();
        interpreter.env.enter_scope();
        self.execute(interpreter, args)
    }
}

impl Callable for Rc<Class> {
    fn arity(&self) -> usize {
        0
    }
    fn name(&self) -> &str {
        &self.name
    }
    fn call(&self, _: Vec<Value>) -> Result<Value, LoxError> {
        Ok(Instance::new(self.clone()))
    }
}

impl Callable for Method {
    fn arity(&self) -> usize {
        self.function.arity()
    }
    fn name(&self) -> &str {
        &self.function.name()
    }
    fn call(&self, args: Vec<Value>) -> Result<Value, LoxError> {
        let mut interpreter = self.function.interpreter.clone();
        interpreter.env.enter_scope();
        interpreter.env.declare("this", self.instance.clone());
        interpreter.env.enter_scope();
        self.function.execute(interpreter, args)
    }
}
