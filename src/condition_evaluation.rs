use crate::{
    environment::{EnvironmentBinding, EnvironmentState, Value},
    evaluator::EvaluatorError,
    language::{
        EvaluationNumericAndString, EvaluationNumericOnly, EvaluationOperator, ValueLiteral,
    },
};

pub fn should_evaluate(
    state: EnvironmentState,
    left: EnvironmentBinding,
    right: Option<EnvironmentBinding>,
    evaluation_op: EvaluationOperator,
) -> (EnvironmentState, Result<bool, EvaluatorError>) {
    let result: bool = match (left, right, evaluation_op) {
        (
            EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(lhs))),
            Some(EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(rhs)))),
            EvaluationOperator::NumericOnly(op),
        ) => match op {
            EvaluationNumericOnly::Lz => lhs.lt(&rhs),
            EvaluationNumericOnly::Gz => lhs.gt(&rhs),
            EvaluationNumericOnly::LzEq => lhs.le(&rhs),
            EvaluationNumericOnly::GzEq => lhs.ge(&rhs),
        },
        (
            EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(lhs))),
            Some(EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(rhs)))),
            EvaluationOperator::NumericAndString(op),
        ) => match op {
            EvaluationNumericAndString::Eq => lhs.eq(&rhs),
            EvaluationNumericAndString::Neq => lhs.ne(&rhs),
        },
        (
            EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(_))),
            Some(EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(_)))),
            EvaluationOperator::BooleanTruthy,
        ) => return (state, Err(EvaluatorError::NotABooleanValue)),

        _ => false,
    };
    (state, Ok(result))
}
