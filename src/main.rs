use std::io::Write;

use anyhow::{Context, Result};

mod ast;
mod error;
mod scanner;
mod token;

#[derive(Default)]
struct Interpreter {}

impl Interpreter {
    fn run(&self, source: &str) -> Result<()> {
        for token in scanner::Scanner::new(source.into()).scan_tokens()?.iter() {
            println!("{:?}", token);
        }
        Ok(())
    }

    fn run_prompt(&self) -> Result<()> {
        loop {
            print!("> ");
            let mut s = String::new();
            std::io::stdout().flush()?;
            std::io::stdin().read_line(&mut s)?;
            if let Err(e) = self.run(&s) {
                eprintln!("{}", e);
            }
        }
    }

    fn run_file(&self, input: &str) -> Result<()> {
        self.run(
            &std::fs::read_to_string(input)
                .with_context(|| format!("failed to open script file {}", input))?,
        )
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let interpreter = Interpreter::default();
    match args.as_slice() {
        [_] => interpreter.run_prompt(),
        [_, input] => interpreter.run_file(input),
        _ => {
            eprintln!("usage: loxers [script]");
            std::process::exit(64)
        }
    }
}
