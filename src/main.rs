use anyhow::{Context, Result};
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod ast;
mod error;
mod interpreter;
mod parser;
mod scanner;
mod token;
mod value;

use interpreter::Interpreter;

fn run(interpreter: &mut Interpreter, source: &str) -> Result<()> {
    let tokens = scanner::Scanner::new(source.into()).scan_tokens()?;
    let stmts = parser::Parser::new(tokens).run()?;
    interpreter.execute(stmts).map_err(|e| e.into())
}

fn run_prompt() -> Result<()> {
    let mut rl = Editor::<()>::new();
    let mut interpreter = Interpreter::default();
    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if let Err(e) = run(&mut interpreter, line.as_str()) {
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
