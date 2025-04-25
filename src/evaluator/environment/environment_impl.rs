use std::fmt::Display;

use super::{
    CharacterBasedLiteral, EnvironmentBinding, EvaluatorError, Expression, NumericLiteral,
    RangeValue, Scope, Value, ValueLiteral,
};

impl EnvironmentBinding {
    pub fn new_string(s: &str) -> EnvironmentBinding {
        EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::CharacterBased(
            CharacterBasedLiteral::String(s.to_string()),
        )))
    }

    pub fn new_numeric(n: NumericLiteral) -> EnvironmentBinding {
        EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(n)))
    }

    pub fn new_int(i: i32) -> EnvironmentBinding {
        EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(
            NumericLiteral::Int(i),
        )))
    }

    pub fn new_float(f: f64) -> EnvironmentBinding {
        EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(
            NumericLiteral::Float(f),
        )))
    }

    pub fn new_bool(b: bool) -> EnvironmentBinding {
        EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Bool(b)))
    }
    pub fn new_arr(v: Vec<Expression>) -> EnvironmentBinding {
        EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Array(v)))
    }
}

impl Value {
    pub fn new_numeric(n: NumericLiteral) -> Value {
        Value::ValueLiteral(ValueLiteral::Numeric(n))
    }

    pub fn new_string(s: &str) -> Value {
        Value::ValueLiteral(ValueLiteral::CharacterBased(CharacterBasedLiteral::String(
            s.to_string(),
        )))
    }

    pub fn new_bool(b: bool) -> Value {
        Value::ValueLiteral(ValueLiteral::Bool(b))
    }

    pub fn new_char(c: char) -> Value {
        Value::ValueLiteral(ValueLiteral::CharacterBased(CharacterBasedLiteral::Char(c)))
    }
}

impl Display for RangeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = format!(
            "{}..{}{}",
            self.from,
            {
                if self.inclusive {
                    "="
                } else {
                    ""
                }
            },
            self.to,
        );
        f.write_str(&s)
    }
}

impl RangeValue {
    pub fn new(from: i32, to: i32, inclusive: bool) -> Self {
        Self {
            from,
            to,
            inclusive,
        }
    }
}

pub fn get_local_binding_recursive(
    symbol: &str,
    current_scope: &Scope,
) -> Option<EnvironmentBinding> {
    let local_symbol = current_scope.local_symbols.get(symbol);

    match local_symbol {
        Some(v) => Some(v.clone()),
        None => {
            if let Some(p) = &current_scope.parent {
                get_local_binding_recursive(symbol, p)
            } else {
                None
            }
        }
    }
}

pub fn mutate_local_binding_recursive(
    scope: &mut Scope,
    symbol: &str,
    binding: EnvironmentBinding,
) -> Result<EnvironmentBinding, EvaluatorError> {
    match scope.local_symbols.get(symbol) {
        Some(EnvironmentBinding::Function(_)) => Err(EvaluatorError::InvalidRedeclaration),
        Some(EnvironmentBinding::Identifier(_)) => {
            let _ = scope
                .local_symbols
                .insert(symbol.to_string(), binding.clone());
            Ok(binding)
        }
        _ => match &mut scope.parent {
            Some(p) => mutate_local_binding_recursive(p, symbol, binding),
            None => {
                let _ = scope
                    .local_symbols
                    .insert(symbol.to_string(), binding.clone());
                Ok(binding)
            }
        },
    }
}
