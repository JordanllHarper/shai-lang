use crate::{
    environment::{EnvironmentBinding, EnvironmentState, Value},
    evaluator::{get_identifier_binding, EvaluatorError},
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

        (EnvironmentBinding::Identifier(i), None, EvaluationOperator::BooleanTruthy) => {
            match get_identifier_binding(&state, &i) {
                Ok(v) => match v {
                    EnvironmentBinding::Value(v) => match v {
                        Value::ValueLiteral(ValueLiteral::Bool(b)) => b,
                        _ => return (state, Err(EvaluatorError::NotABooleanValue)),
                    },
                    EnvironmentBinding::Function(_) => todo!(),
                    EnvironmentBinding::Identifier(i) => todo!(),
                    EnvironmentBinding::Range(_) => todo!(),
                },
                Err(_) => todo!(),
            }
        }

        (
            EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Bool(lhs))),
            None,
            EvaluationOperator::BooleanTruthy,
        ) => lhs,

        _ => false,
    };
    (state, Ok(result))
}
