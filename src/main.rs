mod language;
mod lexer;
mod parser_helpers;
pub mod parser;

use crate::lexer::*;

fn main() {
    let input = "x = 5";
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let parsed = parser::parse(&tokens);
}
