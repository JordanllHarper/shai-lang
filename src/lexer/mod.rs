use token::Token;

mod lexer_impl;
pub mod token;

/// Lexer component for transforming raw string slice into a [Token] stream.
/// Implements [Iterator], and will provide a token on each `.next()`.
///
/// Usage:
///
/// ```rust
///
/// let token_iterator = Lexer::new(input)
/// token_iterator.next();
/// token_iterator.advance(); // to advance the lexer without
/// token_iterator.step_back(); // to step back a token
/// token_iterator.peek(); // peek token
///
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Lexer {
    position: usize,
    input: Vec<char>,
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let allocated_chars = input.chars().collect::<Vec<char>>();
        Self {
            position: 0,
            input: allocated_chars,
        }
    }

    pub fn step_back(&mut self) {
        if self.position == 0 {
            return;
        }
        self.position -= 1;
    }

    pub fn peek(&mut self) -> Option<Token> {
        let t = self.next_token();
        self.step_back();
        t
    }
}
