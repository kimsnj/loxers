use crate::ast;
use crate::error::{ControlFlow, LoxError};
use crate::interpreter::{Environment, Interpreter};
use crate::value::Value;
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
    pub declaration: ast::Function,
    pub environment: Environment,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn name(&self) -> &str {
        &self.declaration.name
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, LoxError> {
        let mut interpreter = Interpreter::with_environment(self.environment.clone());
        interpreter.env.enter_scope();

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
