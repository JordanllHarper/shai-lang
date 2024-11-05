use crate::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseState {
    tokens: Vec<Token>,
    position: usize,
}

impl ParseState {
    pub fn step_back(self) -> (Option<Token>, Self) {
        let new_position = self.position - 1;
        let previous = self.tokens.get(new_position).cloned();
        let new_state = ParseState::new(self.tokens, new_position);
        (previous, new_state)
    }

    pub fn has_next(self) -> bool {
        self.tokens.get(self.position).cloned().is_some()
    }
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
