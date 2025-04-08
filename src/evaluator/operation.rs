use std::fmt::format;

use super::environment::*;
use crate::evaluator::*;
use crate::language::*;

pub fn evaluate_operation(
    state: EnvironmentState,
    m: Operations,
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    let sides = (*m.lhs, *m.rhs);
    match m.operation {
        Operator::Add => handle_add(state, sides),
        Operator::Subtract => handle_subtract(state, sides),
        Operator::Multiply => handle_multiply(state, sides),
        Operator::Divide => handle_divide(state, sides),
    }
}

fn handle_concatenation(
    state: EnvironmentState,
    c: CharacterBasedLiteral,
    expr: Expression,
    reverse: bool,
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    let s = match get_binding_from_expression(&state, expr) {
        Ok(b) => match get_string_from_binding(&state, &b) {
            Ok(s) => s,
            Err(e) => return (state, Err(e)),
        },
        Err(e) => return (state, Err(e)),
    };

    let format = if reverse {
        format!("{}{}", c, s)
    } else {
        format!("{}{}", s, c)
    };
    (state, Ok(EnvironmentBinding::new_string(&(format))))
}

fn handle_add(
    state: EnvironmentState,
    sides: (Expression, Expression),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match sides {
        (
            Expression::ValueLiteral(ValueLiteral::Numeric(n1)),
            Expression::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => (state, Ok(EnvironmentBinding::new_numeric(n1 + n2))),
        (Expression::ValueLiteral(ValueLiteral::CharacterBased(c)), other) => {
            handle_concatenation(state, c, other, true)
        }
        (other, Expression::ValueLiteral(ValueLiteral::CharacterBased(c))) => {
            handle_concatenation(state, c, other, false)
        }
        _ => (state, Err(EvaluatorError::InvalidAddition)),
    }
}

fn handle_subtract(
    state: EnvironmentState,
    sides: (Expression, Expression),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match sides {
        (
            Expression::ValueLiteral(ValueLiteral::Numeric(n1)),
            Expression::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => (state, Ok(EnvironmentBinding::new_numeric(n1 - n2))),
        _ => (state, Err(EvaluatorError::InvalidSubtract)),
    }
}

fn handle_divide(
    state: EnvironmentState,
    sides: (Expression, Expression),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match sides {
        (
            Expression::ValueLiteral(ValueLiteral::Numeric(n1)),
            Expression::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => (state, Ok(EnvironmentBinding::new_numeric(n1 / n2))),
        _ => (state, Err(EvaluatorError::InvalidDivide)),
    }
}

fn handle_multiply(
    state: EnvironmentState,
    sides: (Expression, Expression),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match sides {
        (
            Expression::ValueLiteral(ValueLiteral::Numeric(n1)),
            Expression::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => (state, Ok(EnvironmentBinding::new_numeric(n1 * n2))),
        _ => (state, Err(EvaluatorError::InvalidMultiplication)),
    }
}
