use std::collections::HashMap;

use crate::language::{Function, Range, ValueLiteral};

/// A simple type alias for handling symbols.
///
/// To give context, a symbol can be a variable or function.
///
/// Example:
///
/// `x = 5` produces a symbol "x" with associated value of 5
/// `square (num) = num * num ` produces a symbol "square" with the value of an expression that
/// will be resolved.
type EnvironmentSymbol = String;

/// Represents the current Environment State of the program. This is for resolving symbols that a user will reference in their program,
/// including the standard library bindings.
///
/// An EnvironmentState is immutable, so adding or removing symbols from the current state requires
/// the production of a new EnvironmentState.
pub struct EnvironmentState {
    pub symbols: HashMap<EnvironmentSymbol, EnvironmentBinding>,
}

pub enum Value {
    ValueLiteral(ValueLiteral),
    Identifier(String),
    Range(Range),
}

pub enum EnvironmentBinding {
    Value(Value),
    Function(Function),
}
