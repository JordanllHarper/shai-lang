use crate::{language::*, lexer::*, parse_state::ParseState};

type ExpressionState = (Expression, ParseState);

fn on_arguments(state: ParseState, first_arg: SingleValue) -> (FunctionArguments, ParseState) {
    let mut args: FunctionArguments = vec![Expression::SingleValue(first_arg)];
    let mut arg_state = state;

    while let (Some(t), state) = arg_state.next() {
        if t == Token::Symbol(Symbol::Newline) {
            return (args, state);
        }
        if t ==  Token::Symbol(Symbol::BraceClose){
            let state = state.step_back();
            return (args, state.1);
        }
        println!("On arguments: {:?}", t);
        let (expr, state) = match t {
            Token::Literal(l) => (Expression::SingleValue(l.to_single_value()), state),
            Token::Ident(i) => (SingleValue::new_identifier_expression(&i), state),
            Token::Symbol(Symbol::BraceOpen) => on_body(state),
            _ => todo!(),
        };
        args.push(expr);
        arg_state = state;
    }
    unreachable!("Invalid syntax!")
}

fn on_parameters(state: ParseState) -> (FunctionParameters, ParseState) {
    let mut parameters: FunctionParameters = Vec::new();
    let mut param_state = state;

    while let (Some(t), state) = param_state.next() {
        if t == Token::Symbol(Symbol::ParenClose) {
            return (parameters, state);
        }
        let new_state = match t {
            Token::Ident(param_ident) => {
                let (t, state) = state.next();

                let (native_type, state) = match t {
                    Some(Token::Kwd(Kwd::DataType(d))) => {
                        (Some(NativeType::from_datatype_kwd(&d)), state)
                    }
                    _ => (None, state.step_back().1),
                };

                let parameter = Parameter::new(&param_ident, native_type);
                parameters.push(parameter);
                state
            }
            Token::Symbol(Symbol::Comma) => {
                // do nothing
                //
                state
            }
            t => {
                // TODO: Invalid syntax
                todo!("{:?}", t)
            }
        };
        param_state = new_state;
    }
    unreachable!("Invalid syntax!");
}

fn on_function(state: ParseState, ident: &str) -> ExpressionState {
    let (params, state) = on_parameters(state);
    let (return_type, state) = on_return_type(state);

    let (next, state) = state.next();
    match next {
        Some(t) => {
            match t {
                // function declaration
                Token::Symbol(Symbol::BraceOpen) => {
                    // Body
                    let (body, state) = on_body(state);
                    let f = Function::new(ident, params, return_type, body);
                    (Expression::Function(Box::new(f)), state)
                }
                // Function expression
                Token::Symbol(Symbol::Equals) => {
                    let (expression, state) = on_expression(state, None);
                    let x = Function::new(ident, params, return_type, expression);
                    (Expression::Function(Box::new(x)), state)
                }
                t => {
                    // TODO: Invalid syntax
                    todo!("{:?}\n{:?}", t, state)
                }
            }
        }
        None => {
            // TODO: Invalid syntax
            todo!()
        }
    }
}

fn on_return_type(state: ParseState) -> (Option<NativeType>, ParseState) {
    let (next, state) = state.next();
    if let Some(Token::Symbol(Symbol::Arrow)) = next {
        let (next, state) = state.next();
        let ret_type = match next {
            Some(Token::Kwd(Kwd::DataType(d))) => Some(NativeType::from_datatype_kwd(&d)),
            _ => {
                // TODO: Invalid syntax
                todo!()
            }
        };
        (ret_type, state)
    } else {
        (None, state.step_back().1)
    }
}

fn on_body(state: ParseState) -> ExpressionState {
    let mut body: Body = Vec::new();
    let mut body_state = state.clone();
    while let (Some(t), state) = body_state.clone().next() {
        println!("Iterating tokens: {:?}", t);
        if t == Token::Symbol(Symbol::BraceClose) {
            println!("Found closing brace");
            return (Expression::Body(body), state);
        }

        println!("On body: {:?}", t);
        let state = match t {
            Token::Symbol(Symbol::Newline) => state,
            _ => {
                let (_, state) = state.step_back();
                let (expression, state) = on_expression(state, None);
                body.push(expression);
                state
            }
        };

        body_state = state;
    }
    println!("We exited loop {:?}", body_state);
    // No closing brace = invalid!!
    unreachable!("Invalid syntax");
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
            let (rhs, state) =
                on_identifier(state, &i, Some(SingleValue::new_identifier_expression(&i)));
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
fn on_identifier(state: ParseState, ident: &str, previous: Option<Expression>) -> ExpressionState {
    // TODO: Expression (empty statement) e.g. x + y
    // TODO: Variable Usage e.g. y = x + 3
    let (t, state) = state.next();
    println!("On identifier: {:?}", t);
    match t {
        // Assignment
        Some(Token::Symbol(Symbol::Equals)) => on_assignment(state, ident),

        Some(Token::Symbol(Symbol::Math(t))) => {
            let operation = MathOperation::from_token(&t);
            on_math_expression(
                state,
                operation,
                previous.expect("There should be a previous expresssion this is applying to."),
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
    println!("On function call args {:?}", args);

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
    previous: Expression,
) -> ExpressionState {
    let prev = previous.clone();

    let (rhs, state) = on_expression(state, Some(prev));
    (
        Expression::Operation {
            lhs: previous.boxed(),
            rhs: rhs.boxed(),
            operation: Operation::Math(operation),
        },
        state,
    )
}

fn on_expression(state: ParseState, previous: Option<Expression>) -> ExpressionState {
    let (next, state) = state.next();
    let next = next.expect("There should be more tokens given this function is called");
    println!("On expression {:?}", next);
    match next {
        Token::Ident(ident) => on_identifier(
            state,
            &ident,
            Some(SingleValue::new_identifier_expression(&ident)),
        ),
        Token::Kwd(k) => on_keyword(state, &k),
        Token::Literal(l) => (Expression::SingleValue(l.to_single_value()), state),
        Token::Symbol(Symbol::Math(t)) => on_math_expression(
            state,
            MathOperation::from_token(&t),
            previous.expect("Given a math expression, there should be a lhs."),
        ),
        Token::Symbol(Symbol::BraceOpen) => on_body(state),

        _ => todo!(),
    }
}

fn on_keyword(state: ParseState, k: &Kwd) -> ExpressionState {
    match k {
        Kwd::Return => {
            let (remaining_expression, state) = on_expression(state, None);
            (
                Expression::Statement {
                    expression: remaining_expression.boxed(),
                    operation: Operation::Return,
                },
                state,
            )
        }
        Kwd::DataType(_) => todo!(),
        Kwd::While => todo!(),
        Kwd::For => todo!(),
        Kwd::If => on_if(state),
        Kwd::In => todo!(),
        Kwd::Break => todo!(),
        Kwd::Include => todo!(),
        Kwd::Else => on_else(state),
        Kwd::Const => todo!(),
    }
}

fn on_else(state: ParseState) -> ExpressionState {
    println!("In On else");
    on_expression(state, None)
}

pub fn on_if(state: ParseState) -> ExpressionState {
    let (lhs, state) = on_expression(state, None);
    let (next, state) = state.next();
    match next.expect("There should be an evaluation operation after the lhs in an if statement.") {
        Token::Symbol(Symbol::Evaluation(e)) => {
            let evaluation_op = EvaluationOperator::from_evaluation_symbol(&e);

            // we need to have a rhs given some equality operator
            let (rhs, state) = on_expression(state, None);

            let (on_true_evaluation, state) = on_expression(state, None);
            println!("True evaluation result: {:?}", on_true_evaluation);

            let (next, state) = state.next();
            let (on_false_evaluation, state) = if let Some(Token::Kwd(Kwd::Else)) = next {
                let (expr, state) = on_expression(state, None);
                (Some(expr), state)
            } else {
                (None, state)
            };
            println!("false evaluation result: {:?}", on_false_evaluation);
            (
                Expression::If(If::new_boxed(
                    Evaluation::new(lhs, Some(rhs), Some(evaluation_op)),
                    on_true_evaluation,
                    on_false_evaluation,
                )),
                state,
            )
        }
        Token::Symbol(Symbol::BraceOpen) => {
            let (on_true_evaluation, state) = on_body(state);
            // NOTE: this will handle else if
            let (next, state) = state.next();
            let (on_false_evaluation, state) = if next.is_some() {
                let (expr, state) = on_expression(state, None);
                (Some(expr), state)
            } else {
                (None, state)
            };
            println!("{:?}", on_false_evaluation);
            (
                Expression::If(If::new_boxed(
                    Evaluation::new(lhs, None, None),
                    on_true_evaluation,
                    on_false_evaluation,
                )),
                state,
            )
        }
        t => {
            println!("{:?}", t);
            // TODO: Invalid syntax
            todo!()
        }
    }
}

pub fn parse<I>(tokens: I) -> Expression
where
    I: std::iter::IntoIterator<Item = Token>,
{
    let parse_state = ParseState::new(tokens.into_iter().collect::<Vec<Token>>(), 0);
    on_expression(parse_state, None).0
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
            Expression::If(Box::new(If::new(
                Evaluation::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Bool,
                        "true",
                    )),
                    None,
                    None,
                ),
                Expression::Body(vec![
                    // empty body
                ]),
                None,
            ))),
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
                Evaluation::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Int,
                        "3",
                    )),
                    Some(SingleValue::new_value_literal_expression(
                        ValueLiteral::new(NativeType::Int, "3"),
                    )),
                    Some(EvaluationOperator::Eq),
                ),
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
                Evaluation::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Bool,
                        "true",
                    )),
                    None,
                    None,
                ),
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
                Evaluation::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Int,
                        "3",
                    )),
                    Some(SingleValue::new_value_literal_expression(
                        ValueLiteral::new(NativeType::Int, "3"),
                    )),
                    Some(EvaluationOperator::Eq),
                ),
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
