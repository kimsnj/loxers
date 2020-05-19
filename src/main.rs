use anyhow::{Context, Result};
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod ast;
mod callable;
mod error;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod token;
mod value;

use interpreter::Interpreter;
use resolver::Resolver;

fn run(interpreter: &mut Interpreter, resolver: &mut Resolver, source: &str) -> Result<()> {
    let tokens = scanner::Scanner::new(source.into()).scan_tokens()?;
    let stmts = parser::Parser::new(tokens).run()?;
    let bindings = resolver.resolve(&stmts)?;
    interpreter.execute(&stmts, bindings).map_err(|e| e.into())
}

fn run_prompt() -> Result<()> {
    let mut rl = Editor::<()>::new();
    let mut interpreter = Interpreter::default();
    let mut resolver = Resolver::default();
    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if let Err(e) = run(&mut interpreter, &mut resolver, line.as_str()) {
                    eprintln!("{}", e);
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break Ok(());
            }
            Err(e) => break Err(e.into()),
        }
    }
}

fn run_file(input: &str) -> Result<()> {
    run(
        &mut Interpreter::default(),
        &mut Resolver::default(),
        &std::fs::read_to_string(input)
            .with_context(|| format!("failed to open script file {}", input))?,
    )
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_] => run_prompt(),
        [_, input] => run_file(input),
        _ => {
            eprintln!("usage: loxers [script]");
            std::process::exit(64)
        }
    }
}
