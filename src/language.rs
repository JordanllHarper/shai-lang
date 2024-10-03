// TODO: Review Function calls!!!

/// The supported native data types in the language.
#[derive(Debug, PartialEq, Eq)]
pub enum NativeType {
    Void,
    Char,
    Int,
    String,
    Float,
    // These recurse - we can have a Vector of Vectors of Integers
    Array(Box<NativeType>),
    // These recurse - we can have a Dictionary of Vectors to Integers
    Dictionary {
        key: Box<NativeType>,
        value: Box<NativeType>,
    },
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
#[derive(Debug, PartialEq, Eq)]
pub struct ValueLiteral {
    native_type: NativeType,
    // NOTE: This can be "5", but it might mean 5. String is for storing in a
    // flexible format.
    // Use native type to figure out how this should actually be utilised.
    // E.g. "5" with a native type of int -> 5
    representation: String,
}

impl ValueLiteral {
    pub fn new(native_type: NativeType, representation: &str) -> Self {
        Self {
            native_type,
            representation: representation.to_string(),
        }
    }
}
/// Shai-lang is an expression based language. The [Expression] enum represents what expressions in the
/// language can be.
///
/// An expression is defined as something which returns a value.
///
/// There are 3 types of expressions in Shai-lang:
///
/// NOTE: For below, we use '{ }' to denote both real syntax and the ideas of expressions.
///
/// ----
///
/// [Expression::SingleValue] represents expressing some single value. This will evaluate to a [ValueLiteral].
///
/// Example:
///
/// 5 == { return 5 }
///
/// ----
///
/// [Expression::Operation] represents doing some operation on 2 other types of [Expression], and that results to a value.
///
/// y + z
/// Which is the same as
/// { return { return y } + { return z } }
///
/// This can then be used to assign an expression to an identifier:
/// x = y + z
/// which is the same as
/// x = { return { return y } + { return z } }
///
/// The reason why an operation takes 2 expressions (and not say, [ValueLiteral]) is that we
/// should be able to compose them together.
///
/// E.g.
/// x = (5 * 10) + (3 * 4 - 1)
/// OR a more sophisticated example with body expressions (see below)
/// x = {
///     y = 3
///     z = 43
///     return y + z
/// } + 5
///
/// ----
///
/// [Expression::Evaluation] is a comparison of 2 expressions that folds to a true or false
/// statement.
///
/// Examples:
///
/// is_true = 5 == 5
/// where
/// Both `5` are single value expressions
/// == = [EvaluationOperator]
///
/// ----
///
/// [Expression::Body] are series of the above 2 types of expressions.
/// Body expressions are *required* to have an explicit return keyword.
///
/// y = {
///  x = 5 <- Value expression
///
///  // A body expression within an expression
///  if x > 3 {
///    x = 3
///  }
///
///  return x // return value
/// }
///
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    SingleValue(ValueLiteral),
    Operation {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        operation: Operation,
    },
    Evaluation {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        eval: Evaluation,
    },
    Body {
        body: Vec<Expression>,
    },
}

/// Defines the 4 basic math operations supported
///
/// Examples:
///
/// 4 + 5
/// 9 * 10
#[derive(Debug, PartialEq, Eq)]
pub enum MathOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Representation of the operations supported in the language.
///
/// Examples:
///
/// Math operation
/// 5 + 3
///
/// Assignment operation
/// x = 5
///
/// Assignment operation specified with math operation
/// x += 5
///
///
///
#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    Math(MathOperation),               // +, -, /, *
    Assignment(Option<MathOperation>), // = or +=
}

#[derive(Debug, PartialEq, Eq)]
pub enum EvaluationOperator {
    Lz,
    Gz,
    Eq,
    Neq,
}

/// Defines a method of comparison between 2 expressions.
///
/// For clarity, given boolean expressions:
/// if true { ... }
/// if false { ... }
///
/// lhs = expression that has the value assigned (true or false)
/// rhs = expression that returns true constantly
/// comparator = [EvaluationOperator::Eq]
#[derive(Debug, PartialEq, Eq)]
pub struct Evaluation {
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    comparator: EvaluationOperator,
}

/// Represents an argument in a function declaration.
/// Example: (x) -> int { ... }
///           |- Arg with type inferred
///
/// Example: (x int) -> int { ... }
///              |- Arg type explicitly
#[derive(Debug, PartialEq, Eq)]
pub struct Arg {
    ident: String,
    native_type: NativeType,
}

/// Type alias to represent a collection of arguments in a function
/// Example: (x, y, z) -> ...
type Args = Vec<Arg>;

/// Type alias to represent a Return Type in a function
/// Example: (...) -> int/string/char/etc
type ReturnType = NativeType;

//
//
// ----
//
// Complex language constructs
// These are built from above

/// Represents the language construct of assigning some expression to some identifier.
///
/// Example: x = 5
///
/// ident = x
/// rhs = expression of 5
#[derive(Debug, PartialEq, Eq)]
pub struct Assignment {
    ident: String,
    rhs: Expression,
}

impl Assignment {
    pub fn new(ident: &str, rhs: Expression) -> Self {
        Self {
            ident: ident.to_string(),
            rhs,
        }
    }
}

/// Represents a function in the language.
///
/// Example:
///
/// add(x, y) -> int {
///     return x + y
/// }
/// where
/// add = ident
/// (x, y) = args
/// int = return type
/// { return x + y } = Expression
#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    ident: String,
    args: Args,
    return_type: ReturnType,
    body: Expression,
}
