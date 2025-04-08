use crate::evaluator::{evaluate_body, get_values_from_binding, EnvironmentBinding};

use super::{
    get_binding_from_expression, Body, EnvironmentState, EvaluatorError, Expression,
    ScopedVariable, Value,
};

pub fn iterate_array(
    state: EnvironmentState,
    arr: Vec<Expression>,
    scoped_variable: ScopedVariable,
    body: Body,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let scoped_identifier = match scoped_variable {
        ScopedVariable::Single(s) => s,
        ScopedVariable::Multiple(v) => return Err(EvaluatorError::InvalidForLoopIdentifier),
    };
    let mut new_state = state.clone();
    for expr in arr {
        let result = get_binding_from_expression(&new_state, expr);
        let binding = match result {
            Ok(v) => v,
            Err(e) => return Err(e),
        };

        let (mut maybe_state, value) = get_values_from_binding(new_state, binding)?;
        maybe_state.add_or_mutate_symbols(&scoped_identifier, EnvironmentBinding::Value(value));
        let (maybe_state, _) = evaluate_body(maybe_state, body.clone())?;
        new_state = maybe_state;
    }
    Ok((new_state, Value::Void))
}
