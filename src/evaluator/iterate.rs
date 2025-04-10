use crate::evaluator::{evaluate_body, EnvironmentBinding};

use super::{
    get_binding_from_expression, get_values_from_binding, Body, EnvironmentState, EvaluatorError,
    Expression, ScopedVariable, Value,
};

pub fn iterate_array(
    state: EnvironmentState,
    arr: Vec<Expression>,
    scoped_variable: ScopedVariable,
    body: Body,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let scoped_identifier = match scoped_variable {
        ScopedVariable::Single(s) => s,
        ScopedVariable::Multiple(_) => return Err(EvaluatorError::InvalidForLoopIdentifier),
    };
    let mut new_state = state.clone();
    for expr in arr {
        let (maybe_state, binding) = get_binding_from_expression(new_state, expr)?;

        let (mut maybe_state, value) = get_values_from_binding(maybe_state, binding)?;
        maybe_state.add_or_mutate_symbols(&scoped_identifier, EnvironmentBinding::Value(value));
        let (maybe_state, _) = evaluate_body(maybe_state, body.clone())?;
        new_state = maybe_state;
    }
    Ok((new_state, Value::Void))
}
