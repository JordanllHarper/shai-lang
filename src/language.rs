use crate::*;

type MultipleValues = Vec<Expression>;

/// Shai-lang is an expression based language. The [Expression] enum represents what expressions in the
/// language can be.
///
/// An expression is defined as something which returns a value.
///
/// There are several types of expressions in Shai-lang:
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    ValueLiteral(ValueLiteral),
    Identifier(String),
    MultipleValues(MultipleValues),
    Statement(Statement),
    MathOperation(MathOperation),
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

/// The supported native data types in the language. Denotes the type for use in identifiers and
/// literals.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NativeType {
    Void,
    Char,
    Int,
    String,
    Float,
    Bool,
    // These recurse - we can have a Array of Arrays of Integers
    Array,
    Dictionary,
    Function,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// A set of key value pairs.
pub struct Dictionary {
    pub key: Box<NativeType>,
    pub value: Box<NativeType>,
}

/// A single value representation. Representation is held as a String to be deserialized.
///
/// Example:
///
/// x = 5
///     |- the value literal '5', with a native type of 'int'
///
/// x = 'c'
///      |- the value literal 'c', with a native type of 'char'
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ValueLiteral {
    pub native_type: NativeType,
    pub representation: String,
}

/// Represents a program 'body'. That is, a collection of expressions in a row.
///
/// Examples:
///
/// ```
/// add (numOne, numTwo) {  
///     /* start of function *body* */
/// }
/// ```
pub type Body = Vec<Expression>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct While {
    pub condition: Option<Box<Expression>>,
    pub body: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Range {
    from: Box<Expression>,
    to: Box<Expression>,
    inclusive: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct For {
    pub scoped_variable: Box<Expression>,
    pub iterable: Box<Expression>,
    pub body: Box<Expression>,
}

/// Represents a function in the language.
///
/// Example:
///
/// ```shai
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
/// ```shai
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
/// Example: (x, y, z) -> ...
pub type FunctionParameters = Vec<Parameter>;

/// Represents an parameters in a function declaration.
/// Example: (x) -> int { ... }
///           |- Arg with type inferred
///
/// Example: (x int) -> int { ... }
///              |- Arg type explicitly
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parameter {
    ident: String,
    native_type: Option<NativeType>,
}

/// Representation of a Return Type in a function
/// Example: (...) -> int/string/char/etc
pub type ReturnType = NativeType;

/// Represents doing math operations in the language.
///
/// Example: 3 + 4
/// In this case:
///     - 3 is the lhs
///     - 4 is the rhs
///     - The operation is add
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MathOperation {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub operation: Math,
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
    pub args: FunctionArguments,
}
pub type FunctionArguments = MultipleValues;

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

/// Defines the 4 basic math operations supported
///
/// Examples:
///
/// 4 + 5
/// 9 * 10
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Math {
    Add,
    Subtract,
    Multiply,
    Divide,
}

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
    pub math_operation: Option<Math>,
    pub type_assertion: Option<NativeType>,
    pub is_constant: bool,
}

/// Various methods of evaluating 2 expressions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvaluationOperator {
    Lz,
    Gz,
    LzEq,
    GzEq,
    Eq,
    Neq,
    BooleanTruthy, // if true { ... }
}

/// Defines a method of comparison between 2 expressions.
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
    lhs: Box<Expression>,
    rhs: Option<Box<Expression>>,
    evaluation_op: EvaluationOperator,
}

impl Evaluation {
    fn new(lhs: Expression, rhs: Option<Expression>, evaluation_op: EvaluationOperator) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: rhs.map(Box::new),
            evaluation_op,
        }
    }
}

impl EvaluationOperator {
    pub fn from_evaluation_symbol(evaluation_symbol: &EvaluationSymbol) -> Self {
        match evaluation_symbol {
            EvaluationSymbol::Equality => Self::Eq,
            EvaluationSymbol::NotEquality => Self::Neq,
            EvaluationSymbol::LzEq => Self::LzEq,
            EvaluationSymbol::GzEq => Self::GzEq,
            EvaluationSymbol::Lz => Self::Lz,
            EvaluationSymbol::Gz => Self::Gz,
        }
    }
}

impl Parameter {
    pub fn new(ident: &str, native_type: Option<NativeType>) -> Self {
        Self {
            ident: ident.to_string(),
            native_type,
        }
    }
}

impl Function {
    fn new(
        ident: &str,
        args: FunctionParameters,
        return_type: Option<ReturnType>,
        body: Expression,
    ) -> Self {
        Self {
            ident: ident.to_string(),
            params: args,
            return_type,
            body: Box::new(body),
        }
    }
}

impl If {
    fn new(
        evaluation: Expression,
        on_true_evaluation: Expression,
        on_false_evaluation: Option<Expression>,
    ) -> Self {
        Self {
            evaluation: Box::new(evaluation),
            on_true_evaluation: Box::new(on_true_evaluation),
            on_false_evaluation: on_false_evaluation.map(Box::new),
        }
    }
}

impl Range {
    fn new(from: Expression, to: Expression, inclusive: bool) -> Self {
        Self {
            from: Box::new(from),
            to: Box::new(to),
            inclusive,
        }
    }
}

impl ValueLiteral {
    fn new(native_type: NativeType, representation: &str) -> Self {
        Self {
            native_type,
            representation: representation.to_string(),
        }
    }
}
impl NativeType {
    pub fn from_datatype_kwd(kwd: &DataTypeKwd) -> Self {
        match kwd {
            DataTypeKwd::Bool => Self::Bool,
            DataTypeKwd::Char => Self::Char,
            DataTypeKwd::Float => Self::Float,
            DataTypeKwd::Int => Self::Int,
            DataTypeKwd::String => Self::String,
            DataTypeKwd::Void => Self::Void,
        }
    }
}

impl While {
    fn new(condition: Option<Expression>, body: Expression) -> Self {
        Self {
            condition: condition.map(Box::new),
            body: Box::new(body),
        }
    }
}
impl For {
    fn new(scoped_variable: Expression, iterable: Expression, body: Expression) -> Self {
        Self {
            scoped_variable: Box::new(scoped_variable),
            iterable: Box::new(iterable),
            body: Box::new(body),
        }
    }
}
impl Statement {
    fn new(expression: Option<Expression>, operation: StatementOperator) -> Self {
        Self {
            expression: expression.map(Box::new),
            operation,
        }
    }
}

impl Expression {
    pub fn new_from_literal(l: &Literal) -> Self {
        let l = match l {
            Literal::Bool(b) => ValueLiteral::new(NativeType::Bool, &b.to_string()),
            Literal::Int(i) => ValueLiteral::new(NativeType::Int, &i.to_string()),
            Literal::String(s) => ValueLiteral::new(NativeType::String, &s.to_string()),
            Literal::Float(f) => ValueLiteral::new(NativeType::Float, &f.to_string()),
        };
        Expression::ValueLiteral(l)
    }

    pub fn new_identifier(ident: &str) -> Expression {
        Expression::Identifier(ident.to_string())
    }

    pub fn new_string(representation: &str) -> Self {
        Self::ValueLiteral(ValueLiteral::new(NativeType::String, representation))
    }

    pub fn new_int(representation: &str) -> Self {
        Self::ValueLiteral(ValueLiteral::new(NativeType::Int, representation))
    }

    pub fn new_bool(representation: &str) -> Self {
        Self::ValueLiteral(ValueLiteral::new(NativeType::Bool, representation))
    }

    pub fn new_float(representation: &str) -> Self {
        Self::ValueLiteral(ValueLiteral::new(NativeType::Float, representation))
    }

    pub fn new_array(representation: &str) -> Self {
        Self::ValueLiteral(ValueLiteral::new(NativeType::Array, representation))
    }
    pub fn new_math_expression(lhs: Expression, rhs: Expression, operation: Math) -> Expression {
        Expression::MathOperation(MathOperation::new(lhs, rhs, operation))
    }
    pub fn new_range(from: Expression, to: Expression, inclusive: bool) -> Expression {
        Expression::Range(Range::new(from, to, inclusive))
    }
    pub fn new_while(condition: Option<Expression>, body: Expression) -> Expression {
        Expression::While(While::new(condition, body))
    }

    pub fn new_for(
        scoped_variable: Expression,
        iterable: Expression,
        body: Expression,
    ) -> Expression {
        Expression::For(For::new(scoped_variable, iterable, body))
    }
    pub fn new_function(
        ident: &str,
        args: FunctionParameters,
        return_type: Option<ReturnType>,
        body: Expression,
    ) -> Expression {
        Expression::Function(Function::new(ident, args, return_type, body))
    }
    pub fn new_function_call(identifier: &str, args: MultipleValues) -> Expression {
        Expression::FunctionCall(FunctionCall::new(identifier, args))
    }
    pub fn new_statement(
        expression: Option<Expression>,
        operation: StatementOperator,
    ) -> Expression {
        Expression::Statement(Statement::new(expression, operation))
    }
    pub fn new_assignment(
        identifier: &str,
        rhs: Expression,
        math_operation: Option<Math>,
        type_assertion: Option<NativeType>,
        is_constant: bool,
    ) -> Expression {
        Expression::Assignment(Assignment::new(
            identifier,
            rhs,
            math_operation,
            type_assertion,
            is_constant,
        ))
    }

    pub fn new_if(
        evaluation: Expression,
        on_true_evaluation: Expression,
        on_false_evaluation: Option<Expression>,
    ) -> Expression {
        Expression::If(If::new(evaluation, on_true_evaluation, on_false_evaluation))
    }
    pub fn new_body(body: Body) -> Expression {
        Expression::Body(body)
    }

    pub fn new_evaluation(
        lhs: Expression,
        rhs: Option<Expression>,
        evaluation_op: EvaluationOperator,
    ) -> Expression {
        Expression::Evaluation(Evaluation::new(lhs, rhs, evaluation_op))
    }

    pub fn new_char(representation: &str) -> Expression {
        Expression::ValueLiteral(ValueLiteral::new(NativeType::Char, representation))
    }
}

impl Math {
    pub fn from_token(t: &MathSymbol) -> Self {
        match t {
            MathSymbol::FwdSlash => Self::Divide,
            MathSymbol::Asterisk => Self::Multiply,
            MathSymbol::Plus => Self::Add,
            MathSymbol::Minus => Self::Subtract,
        }
    }
}

impl Assignment {
    fn new(
        identifier: &str,
        rhs: Expression,
        math_operation: Option<Math>,
        type_assertion: Option<NativeType>,
        is_constant: bool,
    ) -> Self {
        Self {
            math_operation,
            type_assertion,
            is_constant,
            identifier: identifier.to_string(),
            rhs: Box::new(rhs),
        }
    }
}

impl FunctionCall {
    pub fn new(identifier: &str, args: MultipleValues) -> Self {
        Self {
            identifier: identifier.to_string(),
            args,
        }
    }
}

impl MathOperation {
    pub fn new(lhs: Expression, rhs: Expression, operation: Math) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operation,
        }
    }
}
