pub mod environment;
pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod parser;
pub mod std_lib;

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
    let ast = parser::parse(tokens).unwrap();
    if debug_mode {
        println!("AST: {:?}", ast);
    }
    let (new_state, result) = evaluator::evaluate(state, ast);
    if debug_mode {
        println!("{:?}", new_state);
    }
    if let Err(e) = result {
        eprintln!("{:?}", e);
    }
}
