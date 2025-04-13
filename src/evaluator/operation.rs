use util::map_expression_to_binding;
use util::map_value_to_string;

use super::environment::*;
use crate::evaluator::*;
use crate::language::*;

pub fn evaluate_operation(
    state: EnvironmentState,
    m: Operations,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let new_state = state.clone();
    let (new_state, lh_binding) = map_expression_to_binding(new_state, *m.lhs)?;
    let (new_state, rh_binding) = map_expression_to_binding(new_state, *m.rhs)?;

    let (new_state, left_value) = map_binding_to_value(new_state, lh_binding)?;
    let (new_state, right_value) = map_binding_to_value(new_state, rh_binding)?;
    let values = (left_value, right_value);

    match m.operation {
        Operator::Add => handle_add(new_state, values),
        Operator::Subtract => handle_subtract(new_state, values),
        Operator::Multiply => handle_multiply(new_state, values),
        Operator::Divide => handle_divide(new_state, values),
    }
}

fn handle_concatenation(
    state: EnvironmentState,
    c: CharacterBasedLiteral,
    other: Value,
    in_reverse: bool,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (new_state, s) = map_value_to_string(state, &other)?;

    let format = if in_reverse {
        format!("{}{}", s, c)
    } else {
        format!("{}{}", c, s)
    };
    Ok((new_state, Value::new_string(&(format))))
}

fn handle_add(
    state: EnvironmentState,
    values: (Value, Value),
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match values {
        (Value::ValueLiteral(ValueLiteral::CharacterBased(c)), other) => {
            handle_concatenation(state, c, other, false)
        }

        (other, Value::ValueLiteral(ValueLiteral::CharacterBased(c))) => {
            handle_concatenation(state, c, other, true)
        }
        (
            Value::ValueLiteral(ValueLiteral::Numeric(n1)),
            Value::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => Ok((state, Value::new_numeric(n1 + n2))),
        _ => Err(EvaluatorError::InvalidOperationValue),
    }
}

fn handle_subtract(
    state: EnvironmentState,
    values: (Value, Value),
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match values {
        (
            Value::ValueLiteral(ValueLiteral::Numeric(n1)),
            Value::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => Ok((state, Value::new_numeric(n1 - n2))),
        _ => Err(EvaluatorError::InvalidOperationValue),
    }
}

fn handle_divide(
    state: EnvironmentState,
    values: (Value, Value),
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match values {
        (
            Value::ValueLiteral(ValueLiteral::Numeric(n1)),
            Value::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => Ok((state, Value::new_numeric(n1 / n2))),

        _ => Err(EvaluatorError::InvalidOperationValue),
    }
}

fn handle_multiply(
    state: EnvironmentState,
    values: (Value, Value),
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match values {
        (
            Value::ValueLiteral(ValueLiteral::Numeric(n1)),
            Value::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => Ok((state, Value::new_numeric(n1 * n2))),
        _ => Err(EvaluatorError::InvalidOperationValue),
    }
}
