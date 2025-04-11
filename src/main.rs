pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod parser;

use std::{collections::HashMap, env, fs::read_to_string};

use evaluator::environment::EnvironmentState;
use lexer::Lexer;

fn read_from_file(path: &str) -> String {
    read_to_string(path).unwrap()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let debug_mode = matches!(args.len(), 2);
    let input = read_from_file("./fib.shai");
    let state = EnvironmentState::new(HashMap::new());

    if debug_mode {
        println!("{}", input);
    }
    let tokens = Lexer::new(&input);
    if debug_mode {
        println!("{:?}", tokens);
    }
    let ast = match parser::parse(tokens) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Parse error! {:?}", e);
            return;
        }
    };

    if debug_mode {
        println!("AST: {:?}", ast);
    }

    let result = evaluator::evaluate(state, ast);
    match result {
        Ok((state, v)) => {
            if debug_mode {
                println!("{:?}", state);
                println!("Return value{:?}", v)
            }
        }

        Err(e) => {
            eprintln!("{:?}", e);
        }
    }
}
