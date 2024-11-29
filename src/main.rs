mod language;
mod lexer;
mod parse_state;
mod parser_helpers;
mod runtime;
pub mod parser;


use crate::lexer::*;

fn main() {
    let input = "x = 5";
    // let state = State::new_init(input);
    // TODO: Manage runtime state
}
