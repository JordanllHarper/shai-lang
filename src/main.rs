mod language;
mod lexer;
pub mod parser;

use crate::lexer::*;

fn main() {
    let input = "x = 5";
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let parsed = parser::parse(&tokens);
}
