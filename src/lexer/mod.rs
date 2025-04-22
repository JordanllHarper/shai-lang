mod lexer_impl;
pub mod token;

/// Lexer component for transforming raw string slice into a [Token] stream.
/// Implements [Iterator], and will provide a token on each `.next()`.
///
/// Usage:
///
/// ```
/// let token_iterator = Lexer::new(input)
/// // Get a Vec of Tokens.
/// let tokens = token_iterator.collect::<Vec<Token>>();
///
/// ```
#[derive(Clone, Debug)]
pub struct Lexer {
    position: usize,
    input: Vec<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let allocated_chars = input.chars().collect::<Vec<char>>();
        Self {
            position: 0,
            input: allocated_chars,
        }
    }
    /// Advances the lexer a single position, returning an optional char in the process.
    fn advance(&mut self) -> Option<char> {
        self.position += 1;
        self.input.get(self.position).copied()
    }

    /// Steps the lexer back one step
    fn step_back(&mut self) {
        if self.position == 0 {
            return;
        }
        self.position -= 1;
    }
}
