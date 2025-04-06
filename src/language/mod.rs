pub mod language_impls;
pub mod language_trait_impls;
pub mod math_trait_impls;

use std::collections::HashMap;

/// Shai-lang is an expression based language. The [Expression] enum represents what expressions in the
/// language can be.
///
/// An expression is defined as something which returns a value.
///
/// Return values of each expression:
///
/// - ValueLiteral -> The underlying value
/// - Identifier -> The value attached to the identifier.
/// - Statement -> Either:
///     - Void if the statement doesn't have an expression attached.
///     - The expression Value attached (such as in a return statement).
/// - Math Operation -> The result of the math operation.
/// - Evaluation -> The boolean result.
/// - Function -> The "Function" value.
/// - If -> The resulting value of the If block, or Void if None.
/// - While, For -> Void
/// - Body -> The ending result of the body.
/// - Range -> The range value.
/// - Assignment -> Void
/// - FunctionCall -> The result of calling the function.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    ValueLiteral(ValueLiteral),
    Identifier(String),
    Statement(Statement),
    MathOperation(Operations),
    Evaluation(Evaluation),
    Function(Function),
    If(If),
    While(While),
    For(For),
    Body(Body),
    Range(Range),
    Assignment(Assignment),
    FunctionCall(FunctionCall),
}

/// The supported native data types in the language.
///
/// Denotes the type for use in identifiers and literals.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NativeType {
    Void,
    Char,
    Int,
    String,
    Float,
    Bool,
    Array,
    Dictionary,
    Function,
}

/// Defines what can be a valid Dictionary key in the language.
///
/// Example: we can't have a Function as a valid DictionaryKey.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DictionaryKey {
    String(String),
    Int(i32),
    Bool(bool),
    Identifier(String),
    Char(char),
}

/// A single value representation.
///
/// Example:
///
/// x = 5
///     |- the value literal '5', with a native type of 'int'
///
/// x = 'c'
///      |- the value literal 'c', with a native type of 'char'
#[derive(Debug, Clone, Eq)]
pub enum ValueLiteral {
    CharacterBased(CharacterBasedLiteral),
    Numeric(NumericLiteral),
    Bool(bool),
    // These recurse - we can have a Array of Arrays of Integers
    Array(Vec<Expression>),
    Dictionary(HashMap<DictionaryKey, Expression>),
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CharacterBasedLiteral {
    Char(char),
    String(String),
}

/// The supported numerical types. Int and Float.
///
/// f64 allows for easy conversion from i32.
#[derive(Debug, Clone, PartialOrd)]
pub enum NumericLiteral {
    Int(i32),
    Float(f64),
}

/// Represents a program 'body'. That is, a collection of expressions.
///
/// Examples:
///
/// ```
/// add (numOne, numTwo) {  
///     /* start of function *body* */
/// }
/// ```
pub type Body = Vec<Expression>;

/// A While loop construct. Executes a body while some condition resolves to true.
///
/// e.g.
///
/// while true {
///     print "Hello"
/// }
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct While {
    pub condition: Option<Box<Expression>>,
    pub body: Box<Expression>,
}

/// Represents a series of values between two numbers. Commonly used in For loops. Exclusive by default.
///
/// e.g.
///
/// ```
///     my_range = 0..5 // 0 to (but not including) 5
///     my_inclusive_range = 0..=5 // 0 to and including 5
/// ```

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Range {
    pub from: Box<Expression>,
    pub to: Box<Expression>,
    pub inclusive: bool,
}

/// For loop construct. Executes a body while not at the end of an iterable.
///
/// e.g.
///
/// ```
/// for i in 0..5 {
///     print "Hello!"
/// }
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct For {
    pub scoped_variable: Box<Expression>,
    pub iterable: Box<Expression>,
    pub body: Box<Expression>,
}

/// Function construct. Give a name, some parameters and optionally some return type to a body.
///
/// Example:
///
/// ```
/// add(x, y) -> int {
///     return x + y
/// }
///
/// ```
///
/// where
/// add = ident
/// (x, y) = args
/// int = return type
/// { return x + y } = Expression
///
/// ```
///
/// add(x, y) = return x + y
///
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub ident: String,
    pub params: FunctionParameters,
    pub return_type: Option<ReturnType>,
    pub body: Box<Expression>,
}

/// Type alias to represent a collection of parameters in a function
/// e.g. some_function > (x, y, z) <  -> ...
pub type FunctionParameters = Vec<Parameter>;

/// Parameter construct. These have an optional type hint in the language.
///
///
///
/// Example: (x) -> int { ... }
///           |- Arg with type inferred
///
/// Example: (x int) -> int { ... }
///              |- Arg type explicitly
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parameter {
    pub ident: String,
    pub native_type: Option<NativeType>,
}

/// Representation of a Return Type in a function
/// Example: (...) -> int/string/char/etc
pub type ReturnType = NativeType;

/// Represents doing operations in the language.
///
/// Example: 3 + 4
/// In this case:
///     - 3 is the lhs
///     - 4 is the rhs
///     - The operation is add
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Operations {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub operation: Operator,
}

/// Represents a function call with arguments.
///
/// Example:
/// ```
/// print "Hello" "World" 5 // Hello World 5
///
/// print "Hello" {
///     if true {
///       "World"
///     } else {
///       "Planet"
///     }
/// } // Hello World
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionCall {
    pub identifier: String,
    pub args: Vec<Expression>,
}

/// Represents language features: managing control flow, importing packages, or returning values from
/// functions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Statement {
    pub expression: Option<Box<Expression>>,
    pub operation: StatementOperator,
}

/// Represents the if expression, which allows conditional branching.
///
/// Given on_false_evaluation is *not* None, this represents an else branch:
///
/// ```
/// if true {
///     ...
/// } else {
///     ...      
/// }
/// ```
///
/// ...otherwise it's a single if branch.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub evaluation: Box<Expression>,
    pub on_true_evaluation: Box<Expression>,
    pub on_false_evaluation: Option<Box<Expression>>,
}

/// Defines the 4 operators.
///
/// NOTE: Add has a multiple use case of adding strings together.
///
/// Examples:
///
/// 4 + 5
/// 4 + "hi" // = "4hi"
///
/// 9 * 10
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// These are normally special operations such as breaking a loop or
/// returning a value.
///
/// e.g
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StatementOperator {
    Break,
    Continue,
    Return,
    Include,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub identifier: String,
    pub rhs: Box<Expression>,
    pub math_operation: Option<Operator>,
    pub type_assertion: Option<NativeType>,
    pub is_constant: bool,
}

/// Various methods of evaluating 2 expressions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvaluationOperator {
    NumericOnly(EvaluationNumericOnly),
    NumericAndString(EvaluationNumericAndString),
    BooleanTruthy, // if true { ... }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvaluationNumericOnly {
    Lz,
    Gz,
    LzEq,
    GzEq,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvaluationNumericAndString {
    Eq,
    Neq,
}

/// Defines a method of comparison between 2 expressions. Returns boolean.
///
/// if 1 == 1 { ... }
///
/// Given the rhs is `None`, this is equatable to the following:
///
/// if true { ... }
///
/// where the rhs is implicitly true
///
/// if true == true { ... }
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Evaluation {
    pub lhs: Box<Expression>,
    pub rhs: Option<Box<Expression>>,
    pub evaluation_op: EvaluationOperator,
}
