use crate::{
    environment::{EnvironmentBinding, EnvironmentState, EnvironmentSymbol, Value},
    language::{
        Expression, Function, FunctionArguments, FunctionCall, Statement, StatementOperator,
        ValueLiteral,
    },
    std_lib::Stdlib,
};

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    InvalidRedeclaration,
    NoSuchIdentifier,
    InvalidFunctionCall,
    InvalidFunctionCallArgument,
}

pub fn evaluate(state: EnvironmentState, ast: Expression) -> EvaluatorState {
    match ast {
        Expression::ValueLiteral(_) => (state, None),
        Expression::Identifier(ident) => evaluate_identifier(state, &ident),
        Expression::MultipleValues(_) => (state, None),
        Expression::Statement(s) => evaluate_statement(state, s),
        Expression::MathOperation(_) => (state, None),
        Expression::Evaluation(_) => todo!(),
        Expression::Function(f) => {
            add_binding_or_error(state, f.ident.clone(), EnvironmentBinding::Function(f))
        }
        Expression::If(_) => todo!(),
        Expression::While(_) => todo!(),
        Expression::For(_) => todo!(),
        Expression::Body(_) => todo!(),
        Expression::Range(_) => todo!(),
        Expression::Assignment(a) => evaluate_assignment(state, a),
        Expression::FunctionCall(f) => evaluate_function_call(state, f),
    }
}

fn evaluate_assignment_expression(expr: Expression) -> Result<EnvironmentBinding, EvaluatorError> {
    match expr {
        Expression::ValueLiteral(v) => Ok(EnvironmentBinding::Value(Value::ValueLiteral(v))),
        Expression::Identifier(i) => Ok(EnvironmentBinding::Value(Value::Identifier(i))),
        Expression::MultipleValues(_) => todo!(),
        Expression::Statement(_) => todo!(),
        Expression::MathOperation(_) => todo!(),
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

fn evaluate_assignment(state: EnvironmentState, a: crate::language::Assignment) -> EvaluatorState {
    match evaluate_assignment_expression(*a.rhs) {
        Ok(binding) => add_binding_or_error(state, a.identifier, binding),
        Err(e) => (state, Some(e)),
    }
}

fn evaluate_function(state: &EnvironmentState, f: &Function) -> EvaluatorState {
    todo!()
}

fn evaluate_value(state: &EnvironmentState, v: &Value) -> EvaluatorState {
    todo!();
}

type EvaluatorState = (EnvironmentState, Option<EvaluatorError>);
fn evaluate_identifier(state: EnvironmentState, ident: &str) -> EvaluatorState {
    let maybe_std_fn = state.std_lib_symbols.get(ident);
    if let Some(std) = maybe_std_fn {
        let result = handle_std_lib_arg(&state, std, vec![]);
        return match result {
            Ok(_) => (state, None),
            Err(e) => (state, Some(EvaluatorError::InvalidFunctionCall)),
        };
    }

    let symbol = state.local_symbols.get(ident);
    match symbol {
        Some(v) => match v {
            EnvironmentBinding::Value(v) => evaluate_value(&state, v),
            EnvironmentBinding::Function(f) => evaluate_function(&state, f),
        },
        None => (state, Some(EvaluatorError::NoSuchIdentifier)),
    }
}

fn evaluate_statement(state: EnvironmentState, s: Statement) -> EvaluatorState {
    match s.operation {
        StatementOperator::Break => todo!(),
        StatementOperator::Continue => todo!(),
        StatementOperator::Return => todo!(),
        StatementOperator::Include => todo!(),
    }
}

fn handle_function(f: &Function, args: FunctionArguments) -> EvaluatorState {
    todo!();
}

#[derive(Debug)]
pub enum StringArgumentsError {
    InvalidArgument,
    InvalidIdentifier,
}

fn resolve_binding_to_string(
    state: &EnvironmentState,
    binding: &EnvironmentBinding,
) -> Result<String, StringArgumentsError> {
    match binding {
        EnvironmentBinding::Value(v) => match v {
            crate::environment::Value::ValueLiteral(vl) => Ok(vl.to_string()),
            crate::environment::Value::Identifier(i) => match state.local_symbols.get(i) {
                Some(binding) => resolve_binding_to_string(state, binding),
                None => Err(StringArgumentsError::InvalidIdentifier),
            },
            crate::environment::Value::Range(_) => todo!(),
        },
        EnvironmentBinding::Function(f) => todo!(),
    }
}

pub fn handle_std_lib_arg(
    state: &EnvironmentState,
    std: &Stdlib,
    args: FunctionArguments,
) -> Result<(), StringArgumentsError> {
    match std {
        Stdlib::Print(std_print) => {
            let s = resolve_function_arguments_to_string(state, args)?;
            std_print(s);
            Ok(())
        }
    }
}

fn resolve_function_arguments_to_string(
    state: &EnvironmentState,
    args: FunctionArguments,
) -> Result<Vec<String>, StringArgumentsError> {
    let mut s_args: Vec<String> = vec![];
    for arg in args {
        let s = match arg {
            Expression::ValueLiteral(v) => v.to_string(),
            Expression::Identifier(i) => match state.local_symbols.get(&i) {
                Some(binding) => resolve_binding_to_string(state, binding)?,
                None => return Err(StringArgumentsError::InvalidIdentifier),
            },
            Expression::MultipleValues(_) => todo!(),
            Expression::Statement(_) => todo!(),
            Expression::MathOperation(_) => todo!(),
            Expression::Evaluation(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::While(_) => todo!(),
            Expression::For(_) => todo!(),
            Expression::Body(_) => todo!(),
            Expression::Range(_) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::FunctionCall(_) => todo!(),
        };
        s_args.push(s);
    }
    Ok(s_args)
}

fn evaluate_function_call(state: EnvironmentState, fc: FunctionCall) -> EvaluatorState {
    let maybe_std_fn = state.std_lib_symbols.get(&fc.identifier);

    if let Some(std) = maybe_std_fn {
        let result = handle_std_lib_arg(&state, std, fc.args).map_err(|e| match e {
            StringArgumentsError::InvalidArgument => EvaluatorError::InvalidFunctionCallArgument,
            StringArgumentsError::InvalidIdentifier => EvaluatorError::NoSuchIdentifier,
        });

        return (
            state,
            match result {
                Ok(_) => None,
                Err(e) => Some(e),
            },
        );
    }

    if let Some(f) = state.local_symbols.get(&fc.identifier) {
        match f {
            EnvironmentBinding::Value(v) => (state, Some(EvaluatorError::InvalidFunctionCall)),
            EnvironmentBinding::Function(f) => handle_function(f, fc.args),
        }
    } else {
        (state, Some(EvaluatorError::InvalidFunctionCall))
    }
}

fn add_binding_or_error(
    state: EnvironmentState,
    symbol: EnvironmentSymbol,
    binding: EnvironmentBinding,
) -> EvaluatorState {
    let (state, binding) = state.add_local_symbols(symbol, binding);
    match binding {
        Some(e) => (state, Some(EvaluatorError::InvalidRedeclaration)),
        None => (state, None),
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{
        environment::{EnvironmentBinding, EnvironmentState, EnvironmentSymbol, Value},
        evaluator::evaluate,
        language::{Expression, ValueLiteral},
    };

    #[test]
    fn assignment() {
        let ast = Expression::new_assignment("x", Expression::new_int(5), None, None, false);
        let state = EnvironmentState::new(HashMap::new());
        let (state, error) = evaluate(state, ast);

        assert_eq!(
            state.local_symbols.get("x"),
            Some(&EnvironmentBinding::Value(Value::ValueLiteral(
                ValueLiteral::Int(5)
            )))
        );
        assert_eq!(error, None);
    }
}
