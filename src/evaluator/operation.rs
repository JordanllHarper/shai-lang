use util::get_binding_from_expression;
use util::value_literal_to_string;

use super::environment::*;
use crate::evaluator::*;
use crate::language::*;

pub fn evaluate_operation(
    state: EnvironmentState,
    m: Operations,
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    let new_state = state.clone();
    let (new_state, lh_binding) = get_binding_from_expression(new_state, *m.lhs);
    let (new_state, rh_binding) = get_binding_from_expression(new_state, *m.rhs);
    let bindings = match (lh_binding, rh_binding) {
        (Ok(b1), Ok(b2)) => (b1, b2),
        (_, Err(e)) | (Err(e), _) => return (state, Err(e)),
    };

    let (new_state, left_value) = get_values_from_binding(new_state, bindings.0);
    let (new_state, right_value) = get_values_from_binding(new_state, bindings.1);
    let values = match (left_value, right_value) {
        (Ok(left_value), Ok(right_value)) => (left_value, right_value),
        (_, Err(e)) | (Err(e), _) => return (state, Err(e)),
    };
    let value_literals = match values {
        (Value::ValueLiteral(v1), Value::ValueLiteral(v2)) => (v1, v2),
        (_, Value::Void) | (Value::Void, _) => {
            return (state, Err(EvaluatorError::InvalidOperationValue))
        }
    };

    match m.operation {
        Operator::Add => handle_add(new_state, value_literals),
        Operator::Subtract => handle_subtract(new_state, value_literals),
        Operator::Multiply => handle_multiply(new_state, value_literals),
        Operator::Divide => handle_divide(new_state, value_literals),
    }
}

fn handle_concatenation(
    state: EnvironmentState,
    c: CharacterBasedLiteral,
    other: ValueLiteral,
    in_reverse: bool,
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    let (new_state, result) = value_literal_to_string(state.clone(), &other);
    let s = match result {
        Ok(s) => s,
        Err(e) => return (state, Err(e)),
    };

    let format = if in_reverse {
        format!("{}{}", s, c)
    } else {
        format!("{}{}", c, s)
    };
    (new_state, Ok(EnvironmentBinding::new_string(&(format))))
}

fn handle_add(
    state: EnvironmentState,
    values: (ValueLiteral, ValueLiteral),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match values {
        (ValueLiteral::CharacterBased(c), other) => handle_concatenation(state, c, other, false),
        (other, ValueLiteral::CharacterBased(c)) => handle_concatenation(state, c, other, true),
        (ValueLiteral::Numeric(n1), ValueLiteral::Numeric(n2)) => {
            (state, Ok(EnvironmentBinding::new_numeric(n1 + n2)))
        }
        _ => (state, Err(EvaluatorError::InvalidOperationValue)),
    }
}

fn handle_subtract(
    state: EnvironmentState,
    values: (ValueLiteral, ValueLiteral),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match values {
        (ValueLiteral::Numeric(n1), ValueLiteral::Numeric(n2)) => {
            (state, Ok(EnvironmentBinding::new_numeric(n1 - n2)))
        }
        _ => (state, Err(EvaluatorError::InvalidOperationValue)),
    }
}

fn handle_divide(
    state: EnvironmentState,
    values: (ValueLiteral, ValueLiteral),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match values {
        (ValueLiteral::Numeric(n1), ValueLiteral::Numeric(n2)) => {
            (state, Ok(EnvironmentBinding::new_numeric(n1 / n2)))
        }

        _ => (state, Err(EvaluatorError::InvalidOperationValue)),
    }
}

fn handle_multiply(
    state: EnvironmentState,
    values: (ValueLiteral, ValueLiteral),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match values {
        (ValueLiteral::Numeric(n1), ValueLiteral::Numeric(n2)) => {
            (state, Ok(EnvironmentBinding::new_numeric(n1 * n2)))
        }
        _ => (state, Err(EvaluatorError::InvalidOperationValue)),
    }
}
