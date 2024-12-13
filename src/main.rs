mod language;
pub mod lexer;
pub mod parser;
pub mod evaluator;

use crate::lexer::*;

fn main() {
    let input = "x = 5";
    // let state = State::new_init(input);
    // TODO: Manage runtime state
}
