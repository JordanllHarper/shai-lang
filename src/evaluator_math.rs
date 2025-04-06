use crate::{
    environment::{EnvironmentBinding, EnvironmentState},
    evaluator::EvaluatorError,
    language::{Expression, Math, MathOperation, ValueLiteral},
};

pub fn evaluate_math_operation(
    state: EnvironmentState,
    m: MathOperation,
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    let sides = (*m.lhs, *m.rhs);
    match m.operation {
        Math::Add => handle_add(state, sides),
        Math::Subtract => handle_subtract(state, sides),
        Math::Multiply => handle_multiply(state, sides),
        Math::Divide => handle_divide(state, sides),
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
        _ => (state, Err(EvaluatorError::InvalidSubtract)),
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
        _ => (state, Err(EvaluatorError::InvalidSubtract)),
    }
}
