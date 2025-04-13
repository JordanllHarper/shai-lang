use std::{collections::HashMap, fmt::Display};

use crate::language::*;

use super::{std_rust_print, RustBinding};

/// Represents the current Environment State of the program. This is for resolving symbols that a user will reference in their program,
/// including the standard library bindings.
///
/// An EnvironmentState is immutable, so adding or removing symbols from the current state requires
/// the production of a new EnvironmentState.
#[derive(Debug, Clone)]
pub struct EnvironmentState {
    pub current_scope: Scope,
    pub std_lib_symbols: HashMap<String, RustBinding>,
}

#[derive(Debug, Clone)]
pub enum MutateBindingError {
    InvalidRedeclaration,
    NoIdentifier,
}

#[derive(Debug, Clone)]
pub struct NoSuchBinding;

/// A binding in the Environment. This represents all possible values a symbol can resolve to.
///
/// In this case, either a value or a function.
///
/// Example:
///
/// `x = 5` produces a binding of Value::ValueLiteral::Int(5)
#[derive(Debug, Clone, PartialEq)]
pub enum EnvironmentBinding {
    Value(Value),
    Function(Function),
    Identifier(String),
}

/// A Scoped set of variables.
#[derive(Debug, Clone)]
pub struct Scope {
    pub local_symbols: HashMap<String, EnvironmentBinding>,
    pub parent: Option<Box<Scope>>,
}

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

/// The variations an Environment binding value can be.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    ValueLiteral(ValueLiteral),
    Range(RangeValue),
    Void,
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

    pub(crate) fn new_char(c: char) -> Value {
        Value::ValueLiteral(ValueLiteral::CharacterBased(CharacterBasedLiteral::Char(c)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeValue {
    pub from: i32,
    pub to: i32,
    pub inclusive: bool,
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

fn get_local_binding_recursive(symbol: &str, current_scope: &Scope) -> Option<EnvironmentBinding> {
    let local_symbol = current_scope.local_symbols.get(symbol);

    match local_symbol {
        Some(v) => Some(v.clone()),
        None => match &current_scope.parent {
            Some(p) => get_local_binding_recursive(symbol, p),
            None => None,
        },
    }
}

fn mutate_local_binding_recursive(
    scope: &mut Scope,
    symbol: &str,
    binding: EnvironmentBinding,
) -> Option<MutateBindingError> {
    match scope.local_symbols.get(symbol) {
        Some(EnvironmentBinding::Function(_)) => Some(MutateBindingError::InvalidRedeclaration),
        _ => {
            scope
                .local_symbols
                .insert(symbol.to_string(), binding.clone());
            match &mut scope.parent {
                Some(p) => mutate_local_binding_recursive(p, symbol, binding),
                None => None,
            };
            None
        }
    }
}

impl EnvironmentState {
    pub fn new(local_symbols: HashMap<String, EnvironmentBinding>) -> Self {
        Self {
            current_scope: Scope::new_with_symbols(local_symbols, None),
            std_lib_symbols: HashMap::from([
                ("print".to_string(), RustBinding::Print(std_rust_print)),
                ("len".to_string(), RustBinding::Len),
            ]),
        }
    }

    pub fn get_local_binding(&self, symbol: &str) -> Option<EnvironmentBinding> {
        get_local_binding_recursive(symbol, &self.current_scope)
    }

    pub fn add_or_mutate_symbols(
        &mut self,
        symbol: &str,
        binding: EnvironmentBinding,
    ) -> Option<MutateBindingError> {
        mutate_local_binding_recursive(&mut self.current_scope, symbol, binding)
    }
}

impl Scope {
    pub fn new(parent: Option<Scope>) -> Self {
        Self {
            local_symbols: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn new_with_symbols(
        local_symbols: HashMap<String, EnvironmentBinding>,
        parent: Option<Scope>,
    ) -> Self {
        Self {
            local_symbols,
            parent: parent.map(Box::new),
        }
    }
}
