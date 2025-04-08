use crate::evaluator::{evaluate_body, get_values_from_binding, EnvironmentBinding};

use super::{
    evaluate, get_binding_from_expression, Body, EnvironmentState, EvaluatorError, Expression,
    ScopedVariable, Value,
};

pub fn iterate_array(
    state: EnvironmentState,
    arr: Vec<Expression>,
    scoped_variable: ScopedVariable,
    body: Body,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let scoped_identifier = match scoped_variable {
        ScopedVariable::Single(s) => s,
        ScopedVariable::Multiple(v) => {
            return (state, Err(EvaluatorError::InvalidForLoopIdentifier))
        }
    };
    let mut new_state = state.clone();
    for expr in arr {
        let result = get_binding_from_expression(&new_state, expr);
        let binding = match result {
            Ok(v) => v,
            Err(e) => return (state, Err(e)),
        };

        let (mut maybe_state, result) = get_values_from_binding(new_state, binding);
        let value = match result {
            Ok(v) => v,
            Err(e) => return (state, Err(e)),
        };
        maybe_state.add_or_mutate_symbols(&scoped_identifier, EnvironmentBinding::Value(value));
        let (maybe_state, result) = evaluate_body(maybe_state, body.clone());
        if result.is_err() {
            return (state, result);
        }
        new_state = maybe_state;
    }
    (new_state, Ok(Value::Void))
}
