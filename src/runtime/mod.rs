use std::collections::HashMap;

use crate::{
    language::{Assignment, Expression, OneSideOperation, TwoSideOperation},
    parser, Lexer, Token,
};

enum RuntimeReferenceValue {
    Void,
    Int(i32),
    String(String),
    Char(char),
    Array(Vec<RuntimeReferenceValue>),
    Dictionary(HashMap<RuntimeReferenceValue, RuntimeReferenceValue>),
    Bool(bool),
    Float(f32),
}

pub struct State {
    current_instruction: String,
    references: HashMap<String, RuntimeReferenceValue>,
}

impl State {
    pub fn new_init(current_instruction: &str) -> Self {
        Self {
            current_instruction: current_instruction.to_string(),
            references: HashMap::new(),
        }
    }
    pub fn new(next_instruction: &str, references: HashMap<String, RuntimeReferenceValue>) -> Self {
        Self {
            current_instruction: next_instruction.to_string(),
            references,
        }
    }
}

fn evaluate(expression: Expression, state: State) -> State {
    // TODO: Evaluate tree
    // Update references and perform operations
    match expression {
        Expression::Statement {
            expression,
            operation,
        } => evaluate_statement(expression, operation),
        Expression::Operation {
            lhs,
            rhs,
            operation,
        } => evaluate_operation(lhs, rhs, operation),
        Expression::Evaluation(_) => todo!(),
        Expression::Function(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::While { condition, body } => todo!(),
        Expression::For {
            scoped_variable,
            iterable,
            body,
        } => todo!(),
        Expression::Body(_) => todo!(),
        Expression::Range(_) => todo!(),
        _ => todo!(),
    };
    todo!()
}

fn evaluate_statement(expression: Option<Box<Expression>>, operation: OneSideOperation) -> State {
    match operation {
        OneSideOperation::Break => {
            assert!(expression.is_none());
            todo!()
        }
        OneSideOperation::Continue => todo!(),
        OneSideOperation::Return => todo!(),
        OneSideOperation::Include => todo!(),
    }
}
fn evaluate_operation(
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    operation: TwoSideOperation,
) -> State {
    match operation {
        TwoSideOperation::FunctionCall => todo!(),
        TwoSideOperation::Math(_) => todo!(),
        TwoSideOperation::Assignment(a) => evaluate_assignment(a),
    };
    todo!()
}

fn evaluate_assignment(assignment: Assignment) -> State {
    todo!()
}

pub fn run(state: State) -> State {
    let lexer = Lexer::new(&state.current_instruction);
    let tokens = lexer.collect::<Vec<Token>>();
    let expression = parser::parse(tokens);
    evaluate(expression, state)
}
