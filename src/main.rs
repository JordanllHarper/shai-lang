mod lexer;
mod parser;

use crate::lexer::Lexer;

fn main() {
    let input = "x = 5";
    let _lexer = Lexer::new(input);
}
