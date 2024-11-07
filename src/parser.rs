use pretty_assertions::assert_eq;

use crate::{language::*, lexer::*, parse_state::ParseState};

type ExpressionState = (Expression, ParseState);

type ParseResult<T> = Result<T, ParseError>;

pub enum ParseError {
    InvalidSyntax { expected: Token, actual: Token },
    NoMoreTokens,
}

type ArgumentsState = (FunctionArguments, ParseState);
fn parse_arguments(state: ParseState, args: &mut FunctionArguments) -> ArgumentsState {
    let peek = state.peek();

    match peek {
        Some(Token::Symbol(Symbol::Newline)) => return (args.to_vec(), state.advance()),
        Some(Token::Symbol(Symbol::BraceClose)) => return (args.to_vec(), state),
        _ => (),
    }

    let (next, state) = state.next();

    let state = if let Some(Token::Symbol(Symbol::BraceOpen)) = next {
        let (body, state) = on_body(state);
        args.push(body);
        state
    } else {
        state
    };

    match next {
        Some(Token::Literal(l)) => args.push(Expression::SingleValue(l.to_single_value())),
        Some(Token::Ident(i)) => args.push(SingleValue::new_identifier_expression(&i)),
        _ => unreachable!("Invalid syntax"),
    };
    parse_arguments(state, args)
}

fn on_arguments(state: ParseState, first_arg: SingleValue) -> ArgumentsState {
    parse_arguments(state, &mut vec![Expression::SingleValue(first_arg)])
}

fn on_parameter_identifier(state: ParseState, ident: &str) -> (Parameter, ParseState) {
    let next = state.peek();
    let (native_type, state) = match next {
        Some(Token::Kwd(Kwd::DataType(d))) => {
            (Some(NativeType::from_datatype_kwd(d)), state.advance())
        }
        _ => (None, state),
    };
    (Parameter::new(ident, native_type), state)
}

fn parse_parameters(state: ParseState, parameters: &mut Vec<Parameter>) -> ParametersState {
    let (t, state) = state.next();
    match t {
        Some(Token::Symbol(Symbol::ParenClose)) => return (parameters.to_vec(), state),
        Some(Token::Symbol(Symbol::Comma)) => return parse_parameters(state, parameters),
        _ => (),
    }

    let (parameter, state) = match t {
        Some(Token::Ident(ident)) => on_parameter_identifier(state, &ident),
        _ => unreachable!(),
    };
    parameters.push(parameter);

    parse_parameters(state, parameters)
}

type ParametersState = (FunctionParameters, ParseState);

fn on_parameters(state: ParseState) -> ParametersState {
    parse_parameters(state, &mut vec![])
}

fn on_function(state: ParseState, ident: &str) -> ExpressionState {
    let (params, state) = on_parameters(state);
    let (return_type, state) = on_return_type(state);

    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // Body
            let (body, state) = on_body(state);
            let f = Function::new(ident, params, return_type, body);
            (Expression::Function(Box::new(f)), state)
        }
        Some(Token::Symbol(Symbol::Equals)) => {
            let (expression, state) = on_expression(state);
            let function = Function::new(ident, params, return_type, expression);
            (Expression::Function(Box::new(function)), state)
        }
        _ => unreachable!("Invalid syntax"),
    }
}

type ReturnState = (Option<NativeType>, ParseState);
fn on_return_type(state: ParseState) -> ReturnState {
    let peek = state.peek();
    match peek {
        Some(Token::Symbol(Symbol::Arrow)) => {
            let state = state.advance();
            let (next, state) = state.next();
            let ret_type = match next {
                Some(Token::Kwd(Kwd::DataType(d))) => Some(NativeType::from_datatype_kwd(&d)),
                _ => unreachable!("Invalid syntax"),
            };
            (ret_type, state)
        }
        _ => (None, state),
    }
}

type BodyState = (Body, ParseState);
fn parse_body(state: ParseState, body: &mut Body) -> BodyState {
    let peek = state.peek();
    println!("Parse body:{:?}", peek);

    match peek {
        Some(Token::Symbol(Symbol::BraceClose)) => return (body.to_vec(), state.advance()),
        Some(Token::Symbol(Symbol::Newline)) => return parse_body(state.advance(), body),
        _ => (),
    }
    println!("Parse body: Got past peek");

    let (expression, state) = on_expression(state);

    body.push(expression);

    parse_body(state, body)
}

fn on_body(state: ParseState) -> ExpressionState {
    let (body, state) = parse_body(state, &mut vec![]);
    (Expression::Body(body), state)
}

fn on_assignment(state: ParseState, ident: &str) -> ExpressionState {
    let (t, state) = state.next();
    match t {
        Some(Token::Literal(l)) => {
            let sv = l.to_single_value();
            (
                Expression::Operation {
                    lhs: Expression::SingleValue(SingleValue::Identifier(ident.to_string()))
                        .boxed(),
                    rhs: Box::new(Expression::SingleValue(sv)),
                    operation: Operation::Assignment(None),
                },
                state,
            )
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // TODO: Expression assignment
            todo!()
        }
        Some(Token::Ident(i)) => {
            let lhs = SingleValue::new_identifier_expression(ident);
            let (rhs, state) = on_identifier(state, &i);
            (
                Expression::Operation {
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
                    operation: Operation::Assignment(None),
                },
                state,
            )
        }
        _ => todo!(),
    }
}

// Identifier options
//
// Variable usage
// y = *x* + 3
//      |- here is usage
fn on_identifier(state: ParseState, ident: &str) -> ExpressionState {
    // TODO: Expression (empty statement) e.g. x + y
    // TODO: Variable Usage e.g. y = x + 3
    let t = state.peek();

    if let Some(Token::Symbol(Symbol::BraceOpen)) = t {
        return on_evaluation(
            state,
            SingleValue::new_identifier_expression(ident),
            EvaluationOperator::BooleanTruthy,
        );
    }
    let (t, state) = state.next();
    match t {
        // Assignment
        Some(Token::Symbol(Symbol::Equals)) => on_assignment(state, ident),

        Some(Token::Symbol(Symbol::Math(t))) => {
            let operation = MathOperation::from_token(&t);
            on_math_expression(
                state,
                operation,
                SingleValue::new_identifier_expression(ident),
            )
        }

        // Operations
        // Function call if value literal or string
        Some(Token::Literal(l)) => on_function_call(state, ident, &l),
        Some(Token::Symbol(Symbol::ParenOpen)) => on_function(state, ident),
        _ => (
            Expression::SingleValue(SingleValue::Identifier(ident.to_string())),
            state,
        ),
    }
}

fn on_function_call(state: ParseState, ident: &str, first_arg: &Literal) -> ExpressionState {
    let (args, state) = on_arguments(state, first_arg.to_single_value());
    (
        Expression::Operation {
            lhs: SingleValue::new_identifier_expression(ident).boxed(),
            rhs: Expression::MultipleValues(args).boxed(),
            operation: Operation::FunctionCall,
        },
        state,
    )
}

fn on_math_expression(
    state: ParseState,
    operation: MathOperation,
    lhs: Expression,
) -> ExpressionState {
    let (rhs, state) = on_expression(state);
    (
        Expression::Operation {
            lhs: lhs.boxed(),
            rhs: rhs.boxed(),
            operation: Operation::Math(operation),
        },
        state,
    )
}

fn on_evaluation(
    state: ParseState,
    lhs: Expression,
    evaluation: EvaluationOperator,
) -> ExpressionState {
    let peek = state.peek();
    if peek.is_none() {
        unreachable!("Invalid syntax! Expect a token in on evaluation")
    }

    if let EvaluationOperator::BooleanTruthy = evaluation {
        return (
            Expression::Evaluation(Evaluation::new(lhs, None, evaluation)),
            state,
        );
    }
    let (rhs, state) = on_expression(state);
    (
        Expression::Evaluation(Evaluation::new(lhs, Some(rhs), evaluation)),
        state,
    )
}

fn on_boolean_literal(state: ParseState, boolean: Expression) -> ExpressionState {
    let peek = state.peek();
    match peek {
        Some(Token::Symbol(Symbol::Evaluation(e))) => {
            let evaluation_operator = EvaluationOperator::from_evaluation_symbol(e);
            let state = state.advance();
            on_evaluation(state, boolean, evaluation_operator)
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            on_evaluation(state, boolean, EvaluationOperator::BooleanTruthy)
        }
        _ => (boolean, state),
    }
}

fn on_literal(state: ParseState, l: Literal) -> ExpressionState {
    let lhs = Expression::SingleValue(l.to_single_value());
    if let Literal::Bool(_) = l {
        on_boolean_literal(state, lhs)
    } else {
        let peek = state.peek();
        match peek {
            Some(Token::Symbol(Symbol::Evaluation(e))) => {
                let evaluation_operator = EvaluationOperator::from_evaluation_symbol(e);
                let state = state.advance();
                on_evaluation(state, lhs, evaluation_operator)
            }
            _ => (lhs, state),
        }
    }
}
fn on_expression(state: ParseState) -> ExpressionState {
    let (next, state) = state.next();
    match next.expect("There should be more tokens given this function is called") {
        Token::Ident(ident) => on_identifier(state, &ident),
        Token::Kwd(k) => on_keyword(state, &k),
        Token::Literal(l) => on_literal(state, l),
        Token::Symbol(Symbol::BraceOpen) => on_body(state),
        Token::Symbol(Symbol::Newline) => on_expression(state),

        t => {
            panic!("Invalid token. Got {:?}", t)
        }
    }
}

fn on_keyword(state: ParseState, k: &Kwd) -> ExpressionState {
    match k {
        Kwd::Return => on_return(state),
        Kwd::DataType(_) => todo!(),
        Kwd::While => on_while(state),
        Kwd::For => todo!(),
        Kwd::If => on_if(state),
        Kwd::In => todo!(),
        Kwd::Break => todo!(),
        Kwd::Include => todo!(),
        Kwd::Else => on_else(state),
        Kwd::Const => todo!(),
    }
}

fn on_while(state: ParseState) -> ExpressionState {
    let peek = state.peek();
    let (condition, state) = match peek {
        Some(Token::Symbol(Symbol::BraceOpen)) => (None, state),
        Some(_) => {
            let (expr, state) = on_expression(state);
            (Some(expr), state)
        }
        None => unreachable!("Invalid syntax!"),
    };

    let (next, state) = state.next();

    assert_eq!(Some(Token::Symbol(Symbol::BraceOpen)), next);

    let (body, state) = on_body(state);
    let while_expr = Expression::While {
        condition: condition.map(Box::new),
        body: body.boxed(),
    };
    (while_expr, state)
}

fn on_else(state: ParseState) -> ExpressionState {
    on_expression(state)
}

fn on_if(state: ParseState) -> ExpressionState {
    let (evaluation, state) = on_expression(state);
    let (next, state) = state.next();

    assert_eq!(Some(Token::Symbol(Symbol::BraceOpen)), next);

    let (on_true_evaluation, state) = on_body(state);
    // NOTE: this will handle else if
    let (next, state) = state.next();
    let (on_false_evaluation, state) = if let Some(Token::Kwd(Kwd::Else)) = next {
        let (expr, state) = on_expression(state);
        (Some(expr), state)
    } else {
        (None, state)
    };
    (
        Expression::If(If::new_boxed(
            evaluation,
            on_true_evaluation,
            on_false_evaluation,
        )),
        state,
    )
}

fn on_return(state: ParseState) -> ExpressionState {
    let (e, state) = on_expression(state);
    (
        Expression::Statement {
            expression: e.boxed(),
            operation: Operation::Return,
        },
        state,
    )
}

pub fn parse<I>(tokens: I) -> Expression
where
    I: std::iter::IntoIterator<Item = Token>,
{
    let parse_state = ParseState::new(tokens.into_iter().collect::<Vec<Token>>(), 0);
    on_expression(parse_state).0
}

#[cfg(test)]
mod tests {

    use super::*;

    use pretty_assertions::assert_eq;

    fn test(input: Vec<Token>, expected: Expression) {
        let actual = parse(input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn if_expression() {
        // if true { }
        test(
            vec![
                Token::Kwd(Kwd::If),
                Token::Literal(Literal::Bool(true)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::If(If::new_boxed(
                Expression::Evaluation(Evaluation::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Bool,
                        "true",
                    )),
                    None,
                    EvaluationOperator::BooleanTruthy,
                )),
                Expression::Body(vec![
                    // empty body
                ]),
                None,
            )),
        );
        test(
            // if 3 == 3 {
            //     print "Hello"
            // }
            vec![
                Token::Kwd(Kwd::If),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Evaluation(EvaluationSymbol::Equality)),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::If(If::new_boxed(
                Expression::Evaluation(Evaluation::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Int,
                        "3",
                    )),
                    Some(SingleValue::new_value_literal_expression(
                        ValueLiteral::new(NativeType::Int, "3"),
                    )),
                    EvaluationOperator::Eq,
                )),
                Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Hello",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }]),
                None,
            )),
        );
        // where x might be true or false
        // if x {
        //     print "Hello"
        // }
        test(
            vec![
                Token::Kwd(Kwd::If),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::If(If::new_boxed(
                Expression::Evaluation(Evaluation::new(
                    SingleValue::new_identifier_expression("x"),
                    None,
                    EvaluationOperator::BooleanTruthy,
                )),
                Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Hello",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }]),
                None,
            )),
        );
    }

    #[test]
    fn if_else_expression() {
        // if true {
        // empty body
        // } else {
        // empty body
        // }
        test(
            vec![
                Token::Kwd(Kwd::If),
                Token::Literal(Literal::Bool(true)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
                Token::Kwd(Kwd::Else),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::If(If::new_boxed(
                Expression::Evaluation(Evaluation::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Bool,
                        "true",
                    )),
                    None,
                    EvaluationOperator::BooleanTruthy,
                )),
                Expression::Body(vec![
                    // empty body
                ]),
                Some(Expression::Body(vec![])),
            )),
        );

        // if 3 == 3 {
        //     print "Hello"
        // } else {
        //     print "Goodbye"
        // }
        test(
            vec![
                Token::Kwd(Kwd::If),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Evaluation(EvaluationSymbol::Equality)),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
                Token::Kwd(Kwd::Else),
                Token::Symbol(Symbol::BraceOpen),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Goodbye".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::If(If::new_boxed(
                Expression::Evaluation(Evaluation::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Int,
                        "3",
                    )),
                    Some(SingleValue::new_value_literal_expression(
                        ValueLiteral::new(NativeType::Int, "3"),
                    )),
                    EvaluationOperator::Eq,
                )),
                Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Hello",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }]),
                Some(Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Goodbye",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }])),
            )),
        );
        test(
            vec![
                Token::Kwd(Kwd::If),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::BraceOpen),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
                Token::Kwd(Kwd::Else),
                Token::Symbol(Symbol::BraceOpen),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Goodbye".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::If(If::new_boxed(
                Expression::Evaluation(Evaluation::new(
                    SingleValue::new_identifier_expression("x"),
                    None,
                    EvaluationOperator::BooleanTruthy,
                )),
                Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Hello",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }]),
                Some(Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Goodbye",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }])),
            )),
        )
    }

    #[test]
    fn while_loops() {
        // while {
        //
        // }
        test(
            vec![
                Token::Kwd(Kwd::While),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::While {
                condition: None,
                body: Expression::Body(Body::new()).boxed(),
            },
        );
        // With function call:
        // while {
        //    print "Hello"
        // }
        test(
            vec![
                Token::Kwd(Kwd::While),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::While {
                condition: None,
                body: Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Hello",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }])
                .boxed(),
            },
        );

        // With condition
        test(
            vec![
                Token::Kwd(Kwd::While),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Evaluation(EvaluationSymbol::Equality)),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::While {
                condition: Some(
                    Expression::Evaluation(Evaluation::new(
                        SingleValue::new_value_literal_expression(ValueLiteral::new(
                            NativeType::Int,
                            "3",
                        )),
                        Some(SingleValue::new_value_literal_expression(
                            ValueLiteral::new(NativeType::Int, "3"),
                        )),
                        EvaluationOperator::Eq,
                    ))
                    .boxed(),
                ),
                body: Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Hello",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }])
                .boxed(),
            },
        );

        test(
            vec![
                Token::Kwd(Kwd::While),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::While {
                condition: Some(
                    Expression::Evaluation(Evaluation::new(
                        SingleValue::new_identifier_expression("x"),
                        None,
                        EvaluationOperator::BooleanTruthy,
                    ))
                    .boxed(),
                ),
                body: Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Hello",
                        )),
                    ])
                    .boxed(),
                    operation: Operation::FunctionCall,
                }])
                .boxed(),
            },
        );
    }

    #[test]
    fn variable_assignment_with_another_variable() {
        // y = x + 3
        test(
            vec![
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
            ],
            Expression::Operation {
                lhs: Box::new(SingleValue::new_identifier_expression("y")),
                rhs: Box::new(SingleValue::new_identifier_expression("x")),
                operation: Operation::Assignment(None),
            },
        )
    }

    #[test]
    fn variable_usage_in_expressions() {
        // y = x + 3
        test(
            vec![
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Math(MathSymbol::Plus)),
                Token::Literal(Literal::Int(3)),
            ],
            Expression::Operation {
                lhs: Box::new(SingleValue::new_identifier_expression("y")),
                rhs: Box::new(Expression::Operation {
                    lhs: Box::new(SingleValue::new_identifier_expression("x")),
                    rhs: Box::new(SingleValue::new_value_literal_expression(
                        ValueLiteral::new(NativeType::Int, "3"),
                    )),
                    operation: Operation::Math(MathOperation::Add),
                }),
                operation: Operation::Assignment(None),
            },
        )
    }

    #[test]
    fn function_declaration_no_params_no_return() {
        // do_nothing () {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![],
                None,
                Expression::Body(vec![]),
            ))),
        );
    }

    #[test]
    fn function_declaration_no_params_return() {
        // do_nothing () -> int {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Symbol(Symbol::ParenClose),
                // End of Args
                //Return type
                Token::Symbol(Symbol::Arrow),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                //End of Return Type
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ))),
        );
        // with body
    }

    #[test]
    fn function_declaration_params_no_return() {
        // do_nothing (numOne, numTwo) {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                None,
                Expression::Body(vec![]),
            ))),
        );
        // typed arguments
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![
                    Parameter::new("numOne", Some(NativeType::Int)),
                    Parameter::new("numTwo", Some(NativeType::Int)),
                ],
                None,
                Expression::Body(vec![]),
            ))),
        )
    }

    #[test]
    fn function_declaration_params_return() {
        // do_nothing (numOne, numTwo) -> int {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::ParenClose),
                // End of Args
                //Return type
                Token::Symbol(Symbol::Arrow),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                //End of Return Type
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ))),
        );
        // Typed Parameters
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::ParenClose),
                // End of Params
                //Return type
                Token::Symbol(Symbol::Arrow),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                //End of Return Type
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![
                    Parameter::new("numOne", Some(NativeType::Int)),
                    Parameter::new("numTwo", Some(NativeType::Int)),
                ],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ))),
        )
    }

    #[test]
    fn function_expression_params_no_return() {
        // No specified types
        // add (numOne, numTwo) = num_one + num_two\n
        test(
            vec![
                // Signature
                Token::Ident("add".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                Token::Symbol(Symbol::Equals),
                // End of return type
                // body (assume 4 spaces)
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Math(MathSymbol::Plus)),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::Newline),
                // end
            ],
            Expression::Function(Box::new(Function::new(
                "add",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                None,
                Expression::Operation {
                    lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                        "numOne".to_string(),
                    ))),
                    rhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                        "numTwo".to_string(),
                    ))),
                    operation: Operation::Math(MathOperation::Add),
                },
            ))),
        );
    }

    #[test]
    fn function_call_no_values() {
        // print "Hello"
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "print".to_string(),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![Expression::SingleValue(
                    SingleValue::ValueLiteral(ValueLiteral::new(NativeType::String, "Hello")),
                )])),
                operation: Operation::FunctionCall,
            },
        );
    }

    #[test]
    fn floating_point_nums_in_assignment() {
        // x=3.5\n
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Float(3.5)),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "x".to_string(),
                ))),
                rhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                    ValueLiteral::new(NativeType::Float, "3.5"),
                ))),
                operation: Operation::Assignment(None),
            },
        );
    }

    #[test]
    fn function_call_multiple_values() {
        // print "Hello" "World"
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Literal(Literal::String("World".to_string())),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "print".to_string(),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new_string(
                        "Hello",
                    ))),
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new_string(
                        "World",
                    ))),
                ])),
                operation: Operation::FunctionCall,
            },
        );
    }

    #[test]
    fn function_call_multiple_values_with_identifier() {
        // print "Hello" x
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "print".to_string(),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new_string(
                        "Hello",
                    ))),
                    Expression::SingleValue(SingleValue::Identifier("x".to_string())),
                ])),
                operation: Operation::FunctionCall,
            },
        );
    }

    #[test]
    fn function_call_multiple_values_with_literal() {
        // print "Hello" true
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Literal(Literal::Bool(true)),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "print".to_string(),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new_string(
                        "Hello",
                    ))),
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                        NativeType::Bool,
                        "true",
                    ))),
                ])),
                operation: Operation::FunctionCall,
            },
        );
    }

    #[test]
    fn literal_assignment() {
        // "x=5"
        let expected = Expression::Operation {
            lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                "x".to_string(),
            ))),
            rhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                ValueLiteral::new(NativeType::Int, "5"),
            ))),
            operation: Operation::Assignment(None),
        };
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected.clone(),
        );
        // Whitespace handling
        // "x = 5"
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected.clone(),
        );

        // Uneven whitespace handling (unlike swift)
        // "x= 5"

        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected,
        );
    }
}
