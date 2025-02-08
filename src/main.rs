pub mod call_stack;
pub mod evaluator;
mod language;
pub mod lexer;
pub mod parser;

use crate::lexer::*;

fn main() {
    let input = "x = 5";
    // let state = State::new_init(input);
    // TODO: Manage runtime state
}
