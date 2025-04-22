mod parser_impl;

use parser_impl::parse_expression;

use crate::language::*;
use crate::lexer::token::*;

type ParseResult<T> = Result<T, ParseError>;

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
    pub fn step_back(self) -> Self {
        ParseState::new(self.tokens, self.position - 1)
    }
    pub fn new(tokens: Vec<Token>, position: usize) -> Self {
        Self { tokens, position }
    }
    pub fn is_last(&self) -> bool {
        self.peek().is_none()
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    InvalidSyntax {
        message: String,
        token_context: Token,
    },
    NoMoreTokens {
        context: String,
    },
    ExpectedLhs,
}

pub fn parse<I>(tokens: I) -> ParseResult<Expression>
where
    I: std::iter::IntoIterator<Item = Token>,
{
    let tokens: Vec<Token> = tokens
        .into_iter()
        .filter(|each| each != &Token::whitespace())
        .filter(|each| {
            !matches!(each, Token::Symbol(Symbol::Comment(_)))
                && !matches!(each, Token::Symbol(Symbol::MultilineComment(_)))
        })
        .collect();

    let tokens: Vec<Token> = tokens
        .into_iter()
        .rev()
        .skip_while(|t| t == &Token::Symbol(Symbol::Newline))
        .collect();

    let tokens = tokens.into_iter().rev().collect::<Vec<Token>>();

    let mut top_level_body: Body = vec![];
    let mut parse_state = ParseState::new(tokens, 0);
    while !parse_state.is_last() {
        let (expr, new_state) = parse_expression(parse_state, None, None)?;
        top_level_body.push(expr);
        parse_state = new_state;
    }

    Ok(Expression::Body(top_level_body))
}
