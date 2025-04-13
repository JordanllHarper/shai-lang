use std::collections::HashMap;

use token::*;

use crate::{language::*, lexer::*};

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
            EvaluationSymbol::Equality => Self::NumericAndString(EvaluationNumericAndString::Eq),
            EvaluationSymbol::NotEquality => {
                Self::NumericAndString(EvaluationNumericAndString::Neq)
            }
            EvaluationSymbol::LzEq => Self::NumericOnly(EvaluationNumericOnly::LzEq),
            EvaluationSymbol::GzEq => Self::NumericOnly(EvaluationNumericOnly::GzEq),
            EvaluationSymbol::Lz => Self::NumericOnly(EvaluationNumericOnly::Lz),
            EvaluationSymbol::Gz => Self::NumericOnly(EvaluationNumericOnly::Gz),
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
    pub fn from_literal(literal: &Literal) -> Self {
        match literal {
            Literal::Bool(b) => Self::Bool(*b),
            Literal::Int(i) => Self::Numeric(NumericLiteral::Int(*i)),
            Literal::Float(f) => Self::Numeric(NumericLiteral::Float(*f)),
            Literal::String(s) => {
                Self::CharacterBased(CharacterBasedLiteral::String(s.to_string()))
            }
        }
    }
}

impl NativeType {
    pub fn from_datatype_kwd(kwd: &DataTypeKwd) -> Self {
        match kwd {
            DataTypeKwd::Bool => Self::Bool,
            DataTypeKwd::Float => Self::Float,
            DataTypeKwd::Int => Self::Int,
            DataTypeKwd::String => Self::String,
            DataTypeKwd::Void => Self::Void,
            DataTypeKwd::Dict => Self::Dictionary,
            DataTypeKwd::Arr => Self::Array,
        }
    }
}

impl While {
    fn new(condition: Option<Expression>, body: Body) -> Self {
        Self {
            condition: condition.map(Box::new),
            body,
        }
    }
}
impl For {
    fn new(scoped_variable: ScopedVariable, iterable: Expression, body: Body) -> Self {
        Self {
            scoped_variable,
            iterable: Box::new(iterable),
            body,
        }
    }
}
impl Statement {
    pub fn new(expression: Option<Expression>, operation: StatementOperator) -> Self {
        Self {
            expression: expression.map(Box::new),
            operation,
        }
    }
}

impl Expression {
    pub fn new_from_literal(l: &Literal) -> Self {
        let l = ValueLiteral::from_literal(l);
        Expression::ValueLiteral(l)
    }

    pub fn new_index(collection: Expression, index: Expression) -> Expression {
        Expression::Index(Index::new(collection, index))
    }

    pub fn new_identifier(ident: &str) -> Expression {
        Expression::Identifier(ident.to_string())
    }

    pub fn new_string(s: &str) -> Self {
        Self::ValueLiteral(ValueLiteral::CharacterBased(CharacterBasedLiteral::String(
            s.to_string(),
        )))
    }

    pub fn new_char(c: &char) -> Self {
        Self::ValueLiteral(ValueLiteral::CharacterBased(CharacterBasedLiteral::Char(
            c.to_owned(),
        )))
    }

    pub fn new_int(i: i32) -> Self {
        Self::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i)))
    }

    pub fn new_bool(b: bool) -> Self {
        Self::ValueLiteral(ValueLiteral::Bool(b))
    }

    pub fn new_float(f: f64) -> Self {
        Self::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Float(f)))
    }

    pub fn new_array(arr: Vec<Expression>) -> Self {
        Self::ValueLiteral(ValueLiteral::Array(arr))
    }

    pub fn new_dict(dict: HashMap<DictionaryKey, Expression>) -> Self {
        Self::ValueLiteral(ValueLiteral::Dictionary(dict))
    }

    pub fn new_math_expression(
        lhs: Expression,
        rhs: Expression,
        operation: Operator,
    ) -> Expression {
        Expression::MathOperation(Operations::new(lhs, rhs, operation))
    }
    pub fn new_range(from: Expression, to: Expression, inclusive: bool) -> Expression {
        Expression::Range(Range::new(from, to, inclusive))
    }
    pub fn new_while(condition: Option<Expression>, body: Body) -> Expression {
        Expression::While(While::new(condition, body))
    }

    pub fn new_for(
        scoped_variable: ScopedVariable,
        iterable: Expression,
        body: Body,
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
    pub fn new_function_call(identifier: &str, args: Vec<Expression>) -> Expression {
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
        math_operation: Option<Operator>,
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
}

impl Operator {
    pub fn from_token(t: &OpSymbol) -> Self {
        match t {
            OpSymbol::FwdSlash => Self::Divide,
            OpSymbol::Asterisk => Self::Multiply,
            OpSymbol::Plus => Self::Add,
            OpSymbol::Minus => Self::Subtract,
        }
    }
}

impl Assignment {
    fn new(
        identifier: &str,
        rhs: Expression,
        math_operation: Option<Operator>,
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
    pub fn new(identifier: &str, args: Vec<Expression>) -> Self {
        Self {
            identifier: identifier.to_string(),
            args,
        }
    }
}

impl Operations {
    pub fn new(lhs: Expression, rhs: Expression, operation: Operator) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operation,
        }
    }
}
impl ScopedVariable {
    pub fn new_single(s: &str) -> Self {
        Self::Single(s.to_string())
    }
    pub fn new_multiple(multiple: Vec<String>) -> Self {
        Self::Multiple(multiple.to_vec())
    }
}

impl Index {
    pub fn new(collection: Expression, index: Expression) -> Self {
        Self {
            collection: Box::new(collection),
            index: Box::new(index),
        }
    }
}
