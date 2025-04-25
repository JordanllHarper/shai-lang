mod bindings;
pub mod environment;
mod evaluation;
mod evaluator_impl;
mod iterate;
mod operation;
mod util;

use crate::language::*;
use bindings::*;
use environment::*;
use evaluator_impl::*;
use operation::*;
use util::*;

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    InvalidRedeclaration,
    NoSuchIdentifier { ident: String },
    InvalidFunctionCall { ident: String },
    InvalidEvaluation,
    NotYetImplemented { msg: String },
    NotABooleanValue,
    InvalidNumberOfArguments,
    InvalidOperationValue,
    InvalidIterable,
    InvalidForLoopIdentifier,
    IndexOutOfRange,
    InvalidIndex,
    NotACollection,
    InvalidDictionaryKey,
    InvalidArgumentType,
    InvalidAssignment,
}

pub fn evaluate(
    state: EnvironmentState,
    ast: Expression,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match ast {
        Expression::ValueLiteral(v) => Ok((state, Value::ValueLiteral(v))),
        Expression::Identifier(ident) => evaluate_identifier(state, &ident),
        Expression::Statement(s) => evaluate_statement(state, s),
        Expression::MathOperation(m) => evaluate_operation(state, m),
        Expression::Evaluation(b) => evaluate_evaluation(state, b),
        Expression::Function(f) => {
            let (state, _) =
                add_binding_or_error(state, &f.ident.clone(), EnvironmentBinding::Function(f))?;
            dbg!("Function state", &state);
            Ok((state, Value::Void))
        }
        Expression::If(if_expression) => evaluate_if(state, if_expression),
        Expression::While(w) => evaluate_while(state, w),
        Expression::For(f) => evaluate_for(state, f),
        Expression::Body(b) => evaluate_body(state, b),
        Expression::Range(r) => evaluate_range(state, r),
        Expression::Assignment(a) => evaluate_assignment(state, a),
        Expression::FunctionCall(f) => evaluate_function_call(state, f),
        Expression::Index(i) => evaluate_index(state, i),
    }
}
