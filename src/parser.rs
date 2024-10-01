use crate::lexer::Token;
// Parser Ideas
//
// Iterator API: each call to `.next()` will generate a new state of the AST.
//
// Nodes:
// x = 5
// where
// x = identifier
// op = assignment
// 5 = expression that results in 5
//
// Examples:
//
// x = { 5 }
// 5 is an expression of an integer literal
//
// More complicated
//
// ```
//  x = if (true) {
//          x = 1
//          b = 2
//          return x + b
//      } else {
//          return 6
//      }
//      ==
//  x = { if (true) {
//          x = 1
//          b = 2
//          return x + b
//      } else {
//          return 6
//      } }
//
//
//      what is an expression
//
//      optional collection of other expressions
//
//      return value
//
// ```
//
// This is an assignment expression
// which needs a lhs + rhs
//
// ----
//
// 5 + 5
// where
// 5 = lhs
// op = add
// 5 = rhs
//
// This is an operation expression
// This needs lhs, operation, rhs
//
// ----
//
// add(x) -> int {
//    return x + x
// }
//
// where
// lhs = function signature
// op  = assignment
// rhs = function body { ... }
//
// where function signature =
//
// lhs = name -> add
//
// op  = assignment
//
// rhs = function information
//
//
// where function information =
//
// lhs = args
// op = return assignment
// rhs = return type
//
// where args =
//
// vec of arg
//
// where arg =
//
// lhs : identifier
// op : assignment
// rhs : type
//
//
// where type =
// lhs = native type
// op  = optional collection
// rhs = optional collection type
//
// where collection type =
//
// array or dictionary
//
// This is a function declaration
// which needs various types listed above
//
// ----

/// The supported native data types in the language. 
#[derive(Debug)]
enum NativeType {
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
struct ValueLiteral {
    native_type: NativeType,
    // NOTE: This can be "5", but it might mean 5. String is for storing in a
    // flexible format.
    // Use native type to figure out how this should actually be utilised.
    // E.g. "5" with a native type of int -> 5
    representation: String,
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
///[Expression::Operation] represents doing some operation on 2 other types of [Expression], and that results to a value.
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
enum Expression {
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
/// Example:
///
/// 4 + 5
/// 9 * 10
#[derive(Debug)]
enum MathOperation {
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
#[derive(Debug)]
enum Operation {
    Math(MathOperation),               // +, -, /, *
    Assignment(Option<MathOperation>), // = or +=
}

#[derive(Debug)]
enum EvaluationOperator {
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
struct Evaluation {
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
struct Arg {
    ident: String,
    native_type: NativeType,
}

/// Type alias to represent a collection of arguments in a function
/// Example: (x, y, z) -> ...
type Args = Vec<Arg>;

/// Type alias to represent a Return Type in a function
/// Example: (...) -> int/string/char/etc
type ReturnType = NativeType;

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
struct Function {
    ident: String,
    args : Args,
    return_type: ReturnType,
    body : Expression,
}

/// Represents the language construct of assigning some expression to some identifier.
///
/// Example: x = 5
///
/// ident = x
/// rhs = expression of 5
struct Assignment {
    ident: String,
    rhs: Expression,
}
/// The model which holds the generated AST.
///
/// Will implement the [Iterator] trait to provide an updated 'state' of the AST on each call to
/// .next()
#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    // The internal parse state of the current generated AST.
    // This will be returned after every call to .next(),
    // representing another parsing step.
    // internal_tree: Box<Node>,
}
