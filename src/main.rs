pub mod condition_evaluation;
pub mod environment;
pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod math;
pub mod parser;
pub mod rust_bindings;

use std::{collections::HashMap, env, fs::read_to_string};

use environment::EnvironmentState;
use lexer::Lexer;

fn read_from_file(path: &str) -> String {
    read_to_string(path).unwrap()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let debug_mode = matches!(args.len(), 2);
    let input = read_from_file("./hello.shai");
    let state = EnvironmentState::new(HashMap::new());

    if debug_mode {
        println!("{}", input);
    }
    let tokens = Lexer::new(&input);
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

    let (new_state, result) = evaluator::evaluate(state, ast);
    if debug_mode {
        println!("{:?}", new_state);
    }
    match result {
        Err(e) => {
            eprintln!("{:?}", e);
        }
        Ok(v) => {
            if debug_mode {
                println!("Return value{:?}", v)
            }
        }
    }
}
