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

fn handle_add(
    state: EnvironmentState,
    sides: (Expression, Expression),
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match sides {
        (
            Expression::ValueLiteral(ValueLiteral::Numeric(n1)),
            Expression::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => (state, Ok(EnvironmentBinding::new_numeric(n1 + n2))),
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
