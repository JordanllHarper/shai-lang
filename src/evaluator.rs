use crate::{
    condition_evaluation::should_evaluate,
    environment::{EnvironmentBinding, EnvironmentState, Value},
    language::{
        Assignment, Body, Expression, Function, FunctionArguments, FunctionCall, Math,
        MathOperation, Statement, StatementOperator, ValueLiteral,
    },
    lexer::{EvaluationSymbol, Literal},
    std_lib::RustBinding,
};

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    InvalidRedeclaration,
    NoSuchIdentifier,
    InvalidFunctionCall,
    InvalidFunctionCallArgument,
    EmptyBody,
    InvalidEvaluation,
    NotABooleanValue,
}

pub fn evaluate(
    state: EnvironmentState,
    ast: Expression,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    match ast {
        Expression::ValueLiteral(v) => (state, Ok(Value::ValueLiteral(v))),
        Expression::Identifier(ident) => evaluate_identifier(state, &ident),
        Expression::MultipleValues(_) => todo!(),
        Expression::Statement(s) => evaluate_statement(state, s),
        Expression::MathOperation(_) => todo!(),
        Expression::Evaluation(_) => todo!(),
        Expression::Function(f) => {
            add_binding_or_error(state, &f.ident.clone(), EnvironmentBinding::Function(f))
        }
        Expression::If(if_expression) => evaluate_if(state, if_expression),
        Expression::While(_) => todo!(),
        Expression::For(_) => todo!(),
        Expression::Body(b) => evaluate_body(state, b),
        Expression::Range(_) => todo!(),
        Expression::Assignment(a) => evaluate_assignment(state, a),
        Expression::FunctionCall(f) => evaluate_function_call(state, f),
    }
}

fn evaluate_if(
    state: EnvironmentState,
    if_expression: crate::language::If,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let (state, is_true_eval) = evaluate_evaluation(state, *if_expression.evaluation);
    match is_true_eval {
        Ok(eval) => match (eval, if_expression.on_false_evaluation) {
            (true, _) => evaluate(state.clone(), *if_expression.on_true_evaluation),
            (false, None) => (state, Ok(Value::Void)),
            (false, Some(e)) => evaluate(state.clone(), *e),
        },
        Err(e) => (state, Err(e)),
    }
}

fn evaluate_evaluation(
    state: EnvironmentState,
    evaluation: Expression,
) -> (EnvironmentState, Result<bool, EvaluatorError>) {
    if let Expression::Evaluation(e) = evaluation {
        let lhs_binding = match get_binding_from_expression(&state, *e.lhs) {
            Ok(binding) => binding,
            Err(e) => return (state, Err(e)),
        };

        let rhs_binding = if let Some(expr) = e.rhs {
            let result = get_binding_from_expression(&state, *expr);
            match result {
                Ok(v) => Some(v),
                Err(e) => return (state, Err(e)),
            }
        } else {
            None
        };

        should_evaluate(state, lhs_binding, rhs_binding, e.evaluation_op)
    } else {
        (state, Err(EvaluatorError::InvalidEvaluation))
    }
}

fn get_binding_from_expression(
    state: &EnvironmentState,
    expr: Expression,
) -> Result<EnvironmentBinding, EvaluatorError> {
    let binding = match expr {
        Expression::ValueLiteral(v) => EnvironmentBinding::Value(Value::ValueLiteral(v)),
        Expression::Identifier(i) => {
            let result = get_identifier_binding(state, &i);
            match result {
                Ok(v) => v,
                Err(e) => return Err(e),
            }
        }
        Expression::Evaluation(_) => todo!(),
        Expression::FunctionCall(_) => todo!(),
        Expression::Body(e) => todo!(),
        _ => return Err(EvaluatorError::InvalidEvaluation),
    };
    Ok(binding)
}

fn evaluate_body(
    state: EnvironmentState,
    body: Body,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let mut next_state = state.clone();
    let mut next_value = Value::Void;
    for expr in body {
        let (maybe_state, result) = evaluate(next_state, expr);
        match result {
            Ok(v) => {
                next_state = maybe_state;
                next_value = v;
            }
            Err(e) => {
                return (state, Err(e));
            }
        }
    }
    (next_state, Ok(next_value))
}

fn get_identifier_binding(
    state: &EnvironmentState,
    symbol: &str,
) -> Result<EnvironmentBinding, EvaluatorError> {
    state
        .local_symbols
        .get(symbol)
        .map_or(Err(EvaluatorError::NoSuchIdentifier), |binding| {
            Ok(binding.clone())
        })
}

fn evaluate_assignment_expression(
    state: &EnvironmentState,
    expr: Expression,
) -> Result<EnvironmentBinding, EvaluatorError> {
    match expr {
        Expression::ValueLiteral(v) => Ok(EnvironmentBinding::Value(Value::ValueLiteral(v))),
        Expression::Identifier(i) => Ok(get_identifier_binding(state, &i)?),
        Expression::MultipleValues(_) => todo!(),
        Expression::Statement(_) => todo!(),
        Expression::MathOperation(m) => evaluate_math_operation(state, m),
        Expression::Evaluation(_) => todo!(),
        Expression::Function(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::While(_) => todo!(),
        Expression::For(_) => todo!(),
        Expression::Body(_) => todo!(),
        Expression::Range(_) => todo!(),
        Expression::Assignment(_) => todo!(),
        Expression::FunctionCall(_) => todo!(),
    }
}

fn evaluate_math_operation(
    state: &EnvironmentState,
    m: MathOperation,
) -> Result<EnvironmentBinding, EvaluatorError> {
    match (*m.lhs, *m.rhs) {
        (
            Expression::ValueLiteral(ValueLiteral::Numeric(n1)),
            Expression::ValueLiteral(ValueLiteral::Numeric(n2)),
        ) => {
            match m.operation {
                Math::Add => todo!(),
                Math::Subtract => todo!(),
                Math::Multiply => todo!(),
                Math::Divide => todo!(),
            }
            todo!()
        }
        _ => todo!(),
    }
}

fn evaluate_assignment(
    state: EnvironmentState,
    a: Assignment,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    match evaluate_assignment_expression(&state, *a.rhs) {
        Ok(binding) => add_binding_or_error(state, &a.identifier, binding),
        Err(e) => (state, Err(e)),
    }
}

fn evaluate_function(
    state: EnvironmentState,
    f: &Function,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    todo!()
}

fn evaluate_value(
    state: EnvironmentState,
    v: &Value,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    todo!();
}

fn evaluate_identifier(
    state: EnvironmentState,
    ident: &str,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let maybe_rust_binding = state.std_lib_symbols.get(ident).cloned();
    if let Some(std) = maybe_rust_binding {
        let (state, result) = handle_rust_binding_with_args(state, &std, vec![]);
        return match result {
            Ok(v) => (state, Ok(v)),
            Err(e) => (state, Err(e)),
        };
    }

    let symbol = state.get_local_binding(ident);

    match symbol {
        Some(binding) => match binding {
            EnvironmentBinding::Value(v) => (state, Ok(v)),
            EnvironmentBinding::Function(f) => evaluate_function(state, &f),
            EnvironmentBinding::Identifier(i) => evaluate_identifier(state, &i),
            EnvironmentBinding::Range(_) => todo!(),
        },
        None => (state, Err(EvaluatorError::NoSuchIdentifier)),
    }
}

fn evaluate_statement(
    state: EnvironmentState,
    s: Statement,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    match s.operation {
        StatementOperator::Break => todo!(),
        StatementOperator::Continue => todo!(),
        StatementOperator::Return => todo!(),
        StatementOperator::Include => todo!(),
    }
}

fn resolve_binding_to_string(
    state: &EnvironmentState,
    binding: &EnvironmentBinding,
) -> Result<String, EvaluatorError> {
    match binding {
        EnvironmentBinding::Value(v) => match v {
            Value::ValueLiteral(vl) => Ok(vl.to_string()),
            Value::Void => Ok(String::new()),
        },
        EnvironmentBinding::Function(f) => todo!(),
        EnvironmentBinding::Identifier(i) => match state.local_symbols.get(i) {
            Some(binding) => resolve_binding_to_string(state, binding),
            None => Err(EvaluatorError::NoSuchIdentifier),
        },
        EnvironmentBinding::Range(_) => todo!(),
    }
}

pub fn handle_rust_binding_with_args(
    state: EnvironmentState,
    std: &RustBinding,
    args: FunctionArguments,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    match std {
        RustBinding::Print(std_print) => {
            let (state, result) = resolve_function_arguments_to_string(state, args);
            match result {
                Ok(v) => {
                    std_print(v);
                    (state, Ok(Value::Void))
                }
                Err(v) => (state, Err(v)),
            }
        }
    }
}

fn resolve_function_arguments_to_string(
    state: EnvironmentState,
    args: FunctionArguments,
) -> (EnvironmentState, Result<Vec<String>, EvaluatorError>) {
    let mut new_state = state.clone();
    let mut s_args: Vec<String> = vec![];
    for arg in args {
        let s = match arg {
            Expression::ValueLiteral(v) => v.to_string(),
            Expression::Identifier(i) => match new_state.local_symbols.get(&i) {
                Some(binding) => match resolve_binding_to_string(&new_state, binding) {
                    Ok(v) => v,
                    Err(e) => return (state, Err(e)),
                },
                None => return (state, Err(EvaluatorError::NoSuchIdentifier)),
            },
            Expression::MultipleValues(_) => todo!(),
            Expression::Statement(_) => todo!(),
            Expression::MathOperation(_) => todo!(),
            Expression::Evaluation(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::While(_) => todo!(),
            Expression::For(_) => todo!(),
            Expression::Body(b) => {
                let (state, result) = resolve_body_string_value(new_state, b);
                match result {
                    Ok(s) => {
                        new_state = state;
                        s
                    }
                    Err(e) => {
                        return (state, Err(e));
                    }
                }
            }
            Expression::Range(_) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::FunctionCall(_) => todo!(),
        };
        s_args.push(s);
    }
    (new_state, Ok(s_args))
}

fn resolve_body_string_value(
    state: EnvironmentState,
    b: Vec<Expression>,
) -> (EnvironmentState, Result<String, EvaluatorError>) {
    let mut new_state = state.clone();
    let mut s = String::new();
    for expr in b {
        let (maybe_state, result) = evaluate(new_state, expr);
        match result {
            Ok(v) => {
                s = match v {
                    Value::ValueLiteral(v) => v.to_string(),
                    Value::Void => "".to_string(),
                };
                new_state = maybe_state;
            }
            Err(e) => return (state, Err(e)),
        }
    }

    (new_state, Ok(s))
}

fn evaluate_function_call(
    state: EnvironmentState,
    fc: FunctionCall,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let maybe_std_fn = state.std_lib_symbols.get(&fc.identifier).cloned();

    if let Some(std) = maybe_std_fn {
        let (state, result) = handle_rust_binding_with_args(state, &std, fc.args);
        match result {
            Ok(_) => (),
            Err(e) => {
                return (state, Err(e));
            }
        }
        return (state, Ok(Value::Void));
    }

    if let Some(f) = state.local_symbols.get(&fc.identifier) {
        match f {
            EnvironmentBinding::Value(v) => (state, Err(EvaluatorError::InvalidFunctionCall)),
            EnvironmentBinding::Function(f) => todo!(),
            EnvironmentBinding::Identifier(_) => todo!(),
            EnvironmentBinding::Range(_) => todo!(),
        }
    } else {
        (state, Err(EvaluatorError::InvalidFunctionCall))
    }
}

fn add_binding_or_error(
    state: EnvironmentState,
    symbol: &str,
    binding: EnvironmentBinding,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let (state, binding) = state.add_local_symbols(symbol, binding);
    match binding {
        Some(e) => (state, Err(EvaluatorError::InvalidRedeclaration)),
        None => (state, Ok(Value::Void)),
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{
        environment::{EnvironmentBinding, EnvironmentState, Value},
        evaluator::evaluate,
        language::{Expression, NumericLiteral, ValueLiteral},
    };

    #[test]
    fn assignment() {
        let ast = Expression::new_assignment("x", Expression::new_int(5), None, None, false);
        let state = EnvironmentState::new(HashMap::new());
        let (state, result) = evaluate(state, ast);

        assert_eq!(
            state.local_symbols.get("x"),
            Some(&EnvironmentBinding::Value(Value::ValueLiteral(
                ValueLiteral::Numeric(NumericLiteral::Int(5))
            )))
        );
        assert!(result.is_err());
    }
}
