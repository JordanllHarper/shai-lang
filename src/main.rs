pub mod environment;
pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod parser;
pub mod std_lib;

use std::{collections::HashMap, fs::read_to_string};

use environment::EnvironmentState;
use lexer::Lexer;

fn read_from_file(path: &str) -> Vec<String> {
    read_to_string(path)
        .into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
}

fn main() {
    let input = read_from_file("./hello.shai");
    let mut state = EnvironmentState::new(HashMap::new());

    for line in input {
        println!("{}", line);
        let tokens = Lexer::new(&line);
        let ast = parser::parse(tokens).unwrap();
        println!("AST: {:?}", ast);
        let (new_state, error, _) = evaluator::evaluate(state, ast);
        if let Some(e) = error {
            eprintln!("{:?}", e);
            break;
        }
        state = new_state;
    }
}
