use std::io::Write;

use anyhow::{Context, Result};

mod ast;
mod error;
mod interpreter;
mod parser;
mod scanner;
mod token;
mod value;

fn run(source: &str) -> Result<()> {
    let tokens = scanner::Scanner::new(source.into()).scan_tokens()?;
    let stmts = parser::Parser::new(tokens).run()?;
    interpreter::Interpreter::execute(stmts).map_err(|e| e.into())
}

fn run_prompt() -> Result<()> {
    loop {
        print!("> ");
        let mut s = String::new();
        std::io::stdout().flush()?;
        std::io::stdin().read_line(&mut s)?;
        if let Err(e) = run(&s) {
            eprintln!("{}", e);
        }
    }
}

fn run_file(input: &str) -> Result<()> {
    run(&std::fs::read_to_string(input)
        .with_context(|| format!("failed to open script file {}", input))?)
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
