pub mod environment;
pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod parser;
pub mod std_lib;

use std::{collections::HashMap, fs::read_to_string};

use environment::EnvironmentState;
use lexer::Lexer;

fn read_from_file(path: &str) -> String {
    read_to_string(path).unwrap()
}

fn main() {
    let input = read_from_file("./hello.shai");
    let state = EnvironmentState::new(HashMap::new());

    println!("{}", input);
    let tokens = Lexer::new(&input);
    let ast = parser::parse(tokens).unwrap();
    println!("AST: {:?}", ast);
    let (new_state, error, _) = evaluator::evaluate(state, ast);
    println!("{:?}", new_state);
    if let Some(e) = error {
        eprintln!("{:?}", e);
    }
}
