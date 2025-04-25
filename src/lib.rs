pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod parser;

use std::collections::HashMap;

use evaluator::environment::*;
use lexer::*;

#[derive(Debug)]
pub enum ShaiError {
    Parse(parser::ParseError),
    Evaluator(evaluator::EvaluatorError),
}

pub fn run(input: &str) -> Result<(EnvironmentState, Value), ShaiError> {
    let state = EnvironmentState::new(HashMap::new());

    dbg!("Input: {}", &input);
    let tokens = Lexer::new(input);

    dbg!("Token stream: {:?}", &tokens);

    let ast = parser::parse(tokens).map_err(ShaiError::Parse)?;

    dbg!("Generated AST: {:?}", &ast);

    let result = evaluator::evaluate(state, ast);
    result.map_err(ShaiError::Evaluator)
}
