pub mod environment;
mod language;
pub mod lexer;
pub mod parser;
pub mod std_lib;

use std::collections::HashMap;

use environment::EnvironmentState;

use crate::lexer::*;

fn main() {
    let input = "print \"Hello, World!\"\n";
    let tokens = lexer::Lexer::new(input);
    let ast = parser::parse(tokens).unwrap();
    let state = EnvironmentState::new(HashMap::new());
    let (state, error) = environment::evaluate(state, ast);

    // TODO: Manage runtime state
}
