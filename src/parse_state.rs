use crate::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseState {
    tokens: Vec<Token>,
    position: usize,
}

impl ParseState {
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }
    pub fn advance(self) -> Self {
        ParseState::new(self.tokens, self.position + 1)
    }

    pub fn next(self) -> (Option<Token>, Self) {
        let next = self.tokens.get(self.position).cloned();
        let new_position = self.position + 1;
        let new_state = ParseState::new(self.tokens, new_position);

        (next, new_state)
    }

    pub fn new(tokens: Vec<Token>, position: usize) -> Self {
        Self { tokens, position }
    }
}
