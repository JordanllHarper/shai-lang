use std::collections::HashMap;

use crate::evaluator::{
    evaluate_body, map_dictionary_key_to_value, map_expression_to_binding, EnvironmentBinding,
};

use super::{
    map_expression_to_value, Body, DictionaryKey, EnvironmentState, EvaluatorError, Expression,
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
        ScopedVariable::Multiple(_) => return Err(EvaluatorError::InvalidForLoopIdentifier),
    };
    let mut new_state = state.clone();
    for expr in arr {
        let (mut maybe_state, value) = map_expression_to_value(new_state, expr)?;
        let _ = maybe_state
            .add_or_mutate_symbols(&scoped_identifier, EnvironmentBinding::Value(value))?;
        let (maybe_state, _) = evaluate_body(maybe_state, body.clone())?;
        new_state = maybe_state;
    }
    Ok((new_state, Value::Void))
}
pub fn iterate_dict(
    state: EnvironmentState,
    dict: HashMap<DictionaryKey, Expression>,
    scoped_variable: ScopedVariable,
    body: Body,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let idents = if let ScopedVariable::Multiple(idents) = scoped_variable {
        idents
    } else {
        return Err(EvaluatorError::InvalidForLoopIdentifier);
    };

    let (key_ident, value_ident) = if let (Some(k), Some(v)) = (idents.first(), idents.last()) {
        (k, v)
    } else {
        return Err(EvaluatorError::InvalidIterable);
    };

    let mut new_state = state.clone();

    for (key, value) in dict {
        let (mut maybe_state, key_value) = map_dictionary_key_to_value(new_state, key)?;
        let _ =
            maybe_state.add_or_mutate_symbols(key_ident, EnvironmentBinding::Value(key_value))?;
        let (mut maybe_state, value_binding) = map_expression_to_binding(maybe_state, value)?;
        let _ = maybe_state.add_or_mutate_symbols(value_ident, value_binding)?;
        let (maybe_state, _) = evaluate_body(maybe_state, body.clone())?;
        new_state = maybe_state;
    }

    Ok((state, Value::Void))
}
