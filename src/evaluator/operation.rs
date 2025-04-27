use util::map_value_to_string;

use super::environment::*;
use crate::evaluator::*;

pub fn handle_mod(
    state: EnvironmentState,
    values: (Value, Value),
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match values {
        (
            Value::ValueLiteral(ValueLiteral::Numeric(n1)),
            Value::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => Ok((state, Value::new_numeric(n1 % n2))),
        _ => Err(EvaluatorError::InvalidOperationValue),
    }
}

pub fn handle_concatenation(
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

pub fn handle_add(
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
        ) => {
            let new_numeric = Value::new_numeric(n1 + n2);
            Ok((state, new_numeric))
        }
        (Value::ValueLiteral(_), Value::Range(_)) => todo!(),
        (Value::ValueLiteral(_), Value::Void) => todo!(),
        (Value::Range(_), Value::ValueLiteral(_)) => todo!(),
        (Value::Range(_), Value::Range(_)) => todo!(),
        (Value::Range(_), Value::Void) => todo!(),
        (Value::Void, Value::ValueLiteral(_)) => todo!(),
        (Value::Void, Value::Range(_)) => todo!(),
        (Value::Void, Value::Void) => todo!(),
        _ => todo!(),
    }
}

pub fn handle_subtract(
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

pub fn handle_divide(
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

pub fn handle_multiply(
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
