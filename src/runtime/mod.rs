use std::collections::HashMap;

use crate::{
    language::{Expression, MathOperation, Operation},
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
    next_instruction: String,
    references: HashMap<String, RuntimeReferenceValue>,
}

impl State {
    pub fn new_init(next_instruction: &str) -> Self {
        Self {
            next_instruction: next_instruction.to_string(),
            references: HashMap::new(),
        }
    }
    pub fn new(next_instruction: &str, references: HashMap<String, RuntimeReferenceValue>) -> Self {
        Self {
            next_instruction: next_instruction.to_string(),
            references,
        }
    }
}
fn evaluate_math(m: MathOperation, lhs: Box<Expression>, rhs: Box<Expression>) -> State {
    todo!()
}

fn evaluate_operation(expr: Box<Expression>, operation: Operation) -> State {
    match operation {
        Operation::Math(m) => evaluate_math(m, lhs, rhs),
        Operation::Assignment {
            math_operation,
            type_assertion,
            is_constant,
        } => evaluate_assignment(math_operation, type_assertion, is_constant, lhs, rhs),
        Operation::Return => evaluate_return(lhs, rhs),
        Operation::FunctionCall => evaluate_function_call(lhs, rhs),
        Operation::Break => evaluate_break(lhs, rhs),
        Operation::Include => evaluate_include(lhs, rhs),
    }
}

fn evaluate_include(lhs: Box<Expression>, rhs: Box<Expression>) -> State {
    todo!()
}

fn evaluate_break(lhs: Box<Expression>, rhs: Box<Expression>) -> State {
    todo!()
}

fn evaluate_function_call(lhs: Box<Expression>, rhs: Box<Expression>) -> State {
    todo!()
}

fn evaluate_return(return_value: Expression) -> State {
    todo!()
}

fn evaluate_assignment(
    math_operation: Option<MathOperation>,
    type_assertion: Option<crate::language::NativeType>,
    is_constant: bool,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
) -> State {
    todo!()
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

fn evaluate_statement(expression: Option<Box<Expression>>, operation: Operation) -> State {
    todo!()
}

pub fn run(state: State) -> State {
    let lexer = Lexer::new(&state.next_instruction);
    let tokens = lexer.collect::<Vec<Token>>();
    let expression = parser::parse(tokens);
    evaluate(expression, state)
}
