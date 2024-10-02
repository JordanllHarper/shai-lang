mod lexer;
mod parser;

use parser::Parser;

use crate::lexer::Lexer;

fn main() {
    let input = "x = 5";
    let tokens = Lexer::new(input).collect();
    let parser = Parser::new(tokens);
}
