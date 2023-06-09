mod tokenizer;
mod tokens;
mod parser;
mod environment;
mod errors;
use std::env;

use std::fs;
use std::io;
use std::io::{BufRead, Write};
use std::println;
use std::process;
use parser::Parser;
use errors::Error;

use crate::errors::format_error;
use crate::errors::locate_error;

fn main() {
    println!("Hello, world!");
    let args: Vec<_> = env::args().skip(1).collect();
    println!("{:?}", args);
    match args.len() {
        0 => run_prompt(),
        1 => run_file(&args[0]),
        _ => {
            println!("Bad arguments! {:?}", args);
            process::exit(64);
        }
    }
}

fn run_file(file: &str) {
    let content = fs::read_to_string(file).unwrap();
    println!("Interpreting:\n{}\n", content);
    run(content);
}

fn run_prompt() {
    let stdin = io::stdin();

    print!("> ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let mut line = line.unwrap();
        if line.is_empty() { continue }
        if !line.ends_with(';') {
            line.push(';');
        }
        run(line);
        print!("\n> ");
        io::stdout().flush().unwrap();
    }
}

fn run(source: String) {
    let parser = Parser::new(source.clone());
    let program = parser.get_ast();
    if let Err((_statements, e)) = program {
        let err = locate_error(&source, e);
        println!("\n{}", format_error(err));
    } else {
        for stmt in program.unwrap().iter() {
            println!("{}", stmt);
            println!("{:?}", stmt);
            println!();
        }
    }
}



