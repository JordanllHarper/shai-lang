use pretty_assertions::assert_eq;

use crate::{language::*, lexer::*, parse_state::ParseState};

type ExpressionState = (Expression, ParseState);

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    InvalidSyntax {
        message: String,
        token_context: Token,
    },
    NoMoreTokens,
}

fn is_new_body(t: Option<&Token>) -> bool {
    t == Some(&Token::Symbol(Symbol::BraceOpen))
}

type ArgumentsState = (FunctionArguments, ParseState);
fn parse_arguments(state: ParseState, args: &mut FunctionArguments) -> ParseResult<ArgumentsState> {
    let peek = state.peek();

    match peek {
        Some(Token::Symbol(Symbol::Newline)) => return Ok((args.to_vec(), state.advance())),
        Some(Token::Symbol(Symbol::BraceClose)) => return Ok((args.to_vec(), state)),
        _ => (),
    }

    let (next, state) = state.next();

    let state = if is_new_body(next.as_ref()) {
        let (body, state) = on_body(state)?;
        args.push(body);
        state
    } else {
        state
    };

    match next {
        Some(Token::Literal(l)) => args.push(Expression::SingleValue(l.to_single_value())),
        Some(Token::Ident(i)) => args.push(SingleValue::new_identifier_expression(&i)),
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected parameter notation".to_string(),
                token_context: t,
            })
        }
        None => return Err(ParseError::NoMoreTokens),
    };
    parse_arguments(state, args)
}

fn on_arguments(state: ParseState, first_arg: SingleValue) -> ParseResult<ArgumentsState> {
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

fn parse_parameters(
    state: ParseState,
    parameters: &mut Vec<Parameter>,
) -> ParseResult<ParametersState> {
    let (t, state) = state.next();
    match t {
        Some(Token::Symbol(Symbol::ParenClose)) => return Ok((parameters.to_vec(), state)),
        Some(Token::Symbol(Symbol::Comma)) => return parse_parameters(state, parameters),
        _ => (),
    }

    let (parameter, state) = match t {
        Some(Token::Ident(ident)) => on_parameter_identifier(state, &ident),
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a parameter identifier".to_string(),
                token_context: t,
            })
        }
        None => return Err(ParseError::NoMoreTokens),
    };
    parameters.push(parameter);

    parse_parameters(state, parameters)
}

type ParametersState = (FunctionParameters, ParseState);

fn on_parameters(state: ParseState) -> ParseResult<ParametersState> {
    parse_parameters(state, &mut vec![])
}

fn on_function(state: ParseState, ident: &str) -> ParseResult<ExpressionState> {
    let (params, state) = on_parameters(state)?;
    let (return_type, state) = on_return_type(state)?;

    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // Body
            let (body, state) = on_body(state)?;
            let f = Function::new(ident, params, return_type, body);
            Ok((Expression::Function(Box::new(f)), state))
        }
        Some(Token::Symbol(Symbol::Equals)) => {
            let (expression, state) = on_expression(state)?;
            let function = Function::new(ident, params, return_type, expression);
            Ok((Expression::Function(Box::new(function)), state))
        }
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected a function body or expression".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens),
    }
}

type ReturnState = (Option<NativeType>, ParseState);
fn on_return_type(state: ParseState) -> ParseResult<ReturnState> {
    let peek = state.peek();
    match peek {
        Some(Token::Symbol(Symbol::Arrow)) => {
            let state = state.advance();
            let (next, state) = state.next();
            let ret_type = match next {
                Some(Token::Kwd(Kwd::DataType(d))) => Some(NativeType::from_datatype_kwd(&d)),
                Some(t) => {
                    return Err(ParseError::InvalidSyntax {
                        message:
                            "Expected a data type keyword as a return type after a return arrow"
                                .to_string(),
                        token_context: t,
                    })
                }
                None => return Err(ParseError::NoMoreTokens),
            };
            Ok((ret_type, state))
        }
        _ => Ok((None, state)),
    }
}

type BodyState = (Body, ParseState);
fn parse_body(state: ParseState, body: &mut Body) -> ParseResult<BodyState> {
    let peek = state.peek();

    match peek {
        Some(Token::Symbol(Symbol::BraceClose)) => return Ok((body.to_vec(), state.advance())),
        Some(Token::Symbol(Symbol::Newline)) => return parse_body(state.advance(), body),
        _ => (),
    }

    let (expression, state) = on_expression(state)?;

    body.push(expression);

    parse_body(state, body)
}

fn on_body(state: ParseState) -> ParseResult<ExpressionState> {
    let (body, state) = parse_body(state, &mut vec![])?;
    Ok((Expression::Body(body), state))
}

fn on_assignment(
    state: ParseState,
    ident: &str,
    type_assertion: Option<NativeType>,
) -> ParseResult<ExpressionState> {
    let (t, state) = state.next();
    match t {
        Some(Token::Literal(l)) => {
            let sv = l.to_single_value();
            Ok((
                Expression::Operation {
                    lhs: Expression::SingleValue(SingleValue::Identifier(ident.to_string()))
                        .boxed(),
                    rhs: Box::new(Expression::SingleValue(sv)),
                    operation: TwoSideOperation::Assignment(Assignment::new(
                        None,
                        type_assertion,
                        false,
                    )),
                },
                state,
            ))
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // TODO: Expression assignment
            todo!()
        }
        Some(Token::Ident(i)) => {
            let lhs = SingleValue::new_identifier_expression(ident);
            let (rhs, state) = on_identifier(state, &i)?;
            Ok((
                Expression::Operation {
                    lhs: lhs.boxed(),
                    rhs: rhs.boxed(),
                    operation: TwoSideOperation::Assignment(Assignment::new(
                        None,
                        type_assertion,
                        false,
                    )),
                },
                state,
            ))
        }
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected a valid assignment: literal value, ".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens),
    }
}

// Identifier options
//
// Variable usage
// y = *x* + 3
//      |- here is usage
fn on_identifier(state: ParseState, ident: &str) -> ParseResult<ExpressionState> {
    // TODO: Expression (empty statement) e.g. x + y
    // TODO: Variable Usage e.g. y = x + 3
    let t = state.peek();

    if let Some(Token::Kwd(Kwd::In)) = t {
        return Ok((
            Expression::SingleValue(SingleValue::Identifier(ident.to_string())),
            state,
        ));
    }
    let (t, state) = state.next();
    match t {
        // Assignment
        Some(Token::Symbol(Symbol::Equals)) => Ok(on_assignment(state, ident, None)?),

        Some(Token::Symbol(Symbol::Math(t))) => {
            let operation = MathOperation::from_token(&t);
            Ok(on_math_expression(
                state,
                operation,
                SingleValue::new_identifier_expression(ident),
            )?)
        }

        // Operations
        // Function call if value literal or string
        Some(Token::Literal(l)) => Ok(on_function_call(state, ident, &l)?),
        Some(Token::Symbol(Symbol::Range)) => Ok(on_range(
            state,
            SingleValue::new_identifier_expression(ident),
            false,
        )?),
        Some(Token::Symbol(Symbol::RangeEq)) => Ok(on_range(
            state,
            SingleValue::new_identifier_expression(ident),
            true,
        )?),
        Some(Token::Symbol(Symbol::ParenOpen)) => Ok(on_function(state, ident)?),
        Some(Token::Symbol(Symbol::Colon)) => Ok(on_variable_type_assertion(state, ident)?),
        _ => Ok((SingleValue::new_identifier_expression(ident), state)),
    }
}

fn on_variable_type_assertion(state: ParseState, ident: &str) -> ParseResult<ExpressionState> {
    let (var_type, state) = state.next();
    match var_type {
        Some(Token::Kwd(Kwd::DataType(d))) => {
            let kwd = NativeType::from_datatype_kwd(&d);
            let (next, state) = state.next();
            match next {
                Some(Token::Symbol(Symbol::Equals)) => { /* Continue */ }
                Some(t) => {
                    return Err(ParseError::InvalidSyntax {
                        message: "Expected an equals sign".to_string(),
                        token_context: t,
                    });
                }
                None => return Err(ParseError::NoMoreTokens),
            }
            Ok(on_assignment(state, ident, Some(kwd))?)
        }
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected a data type".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens),
    }
}

fn on_function_call(
    state: ParseState,
    ident: &str,
    first_arg: &Literal,
) -> ParseResult<ExpressionState> {
    let (args, state) = on_arguments(state, first_arg.to_single_value())?;
    Ok((
        Expression::Operation {
            lhs: SingleValue::new_identifier_expression(ident).boxed(),
            rhs: Expression::MultipleValues(args).boxed(),
            operation: TwoSideOperation::FunctionCall,
        },
        state,
    ))
}

fn on_math_expression(
    state: ParseState,
    operation: MathOperation,
    lhs: Expression,
) -> ParseResult<ExpressionState> {
    let (rhs, state) = on_expression(state)?;
    Ok((
        Expression::Operation {
            lhs: lhs.boxed(),
            rhs: rhs.boxed(),
            operation: TwoSideOperation::Math(operation),
        },
        state,
    ))
}

fn on_single_value(state: ParseState) -> ParseResult<ExpressionState> {
    let (next, state) = state.next();
    let single_value = match next {
        Some(Token::Literal(l)) => Expression::SingleValue(l.to_single_value()),
        Some(Token::Ident(i)) => SingleValue::new_identifier_expression(&i),
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a value literal or identifier".to_string(),
                token_context: t,
            })
        }
        None => return Err(ParseError::NoMoreTokens),
    };
    Ok((single_value, state))
}

fn on_evaluation(state: ParseState) -> ParseResult<ExpressionState> {
    let (lhs, state) = on_single_value(state)?;

    let peek = state.peek();
    // truthy value e.g. if x { ... }
    if is_new_body(peek) {
        return Ok((
            Expression::Evaluation(Evaluation::new(
                lhs,
                None,
                EvaluationOperator::BooleanTruthy,
            )),
            state,
        ));
    }
    let (next, state) = state.next();

    let evaluation_operator = match next {
        Some(Token::Symbol(Symbol::Evaluation(e))) => {
            EvaluationOperator::from_evaluation_symbol(&e)
        }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an evaluation symbol".to_string(),
                token_context: t,
            })
        }
        None => return Err(ParseError::NoMoreTokens),
    };

    let (rhs, state) = on_single_value(state)?;
    Ok((
        Expression::Evaluation(Evaluation::new(lhs, Some(rhs), evaluation_operator)),
        state,
    ))
}

fn on_range(state: ParseState, lhs: Expression, inclusive: bool) -> ParseResult<ExpressionState> {
    let (rhs, state) = on_single_value(state)?;
    Ok((
        Expression::Range(Range::new(lhs.boxed(), rhs.boxed(), inclusive)),
        state,
    ))
}

fn on_expression(state: ParseState) -> ParseResult<ExpressionState> {
    let (next, state) = state.next();

    let expr = match next.ok_or::<ParseError>(ParseError::NoMoreTokens)? {
        Token::Ident(ident) => on_identifier(state, &ident)?,
        Token::Kwd(k) => on_keyword(state, &k)?,
        Token::Literal(l) => (Expression::SingleValue(l.to_single_value()), state),
        Token::Symbol(Symbol::BraceOpen) => on_body(state)?,
        Token::Symbol(Symbol::Newline) => on_expression(state)?,

        t => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an identifier, keyword, literal, body open or newline"
                    .to_string(),
                token_context: t,
            })
        }
    };
    Ok(expr)
}

fn on_include(state: ParseState) -> ParseResult<ExpressionState> {
    let (package, state) = state.next();
    match package {
        Some(Token::Literal(Literal::String(s))) => Ok((
            Expression::Statement {
                expression: Some(
                    SingleValue::new_value_literal_expression(ValueLiteral::new_string(&s)).boxed(),
                ),
                operation: OneSideOperation::Include,
            },
            state,
        )),
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected a string for the package name".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens),
    }
}

fn on_keyword(state: ParseState, k: &Kwd) -> ParseResult<ExpressionState> {
    let expr = match k {
        Kwd::Return => on_return(state)?,
        Kwd::While => on_while(state)?,
        Kwd::For => on_for(state)?,
        Kwd::If => on_if(state)?,
        Kwd::Break => (
            Expression::Statement {
                expression: None,
                operation: OneSideOperation::Break,
            },
            state,
        ),
        Kwd::Include => on_include(state)?,
        Kwd::Else => on_else(state)?,
        Kwd::Const => on_const(state)?,
        t => {
            return Err(ParseError::InvalidSyntax {
                message: "Invalid use of keyword, this should be used in for loop".to_string(),
                token_context: Token::Kwd(t.clone()),
            })
        }
    };

    Ok(expr)
}

fn on_constant_type_assertion(state: ParseState) -> ParseResult<(NativeType, ParseState)> {
    let (var_type, state) = state.next();
    match var_type {
        Some(Token::Kwd(Kwd::DataType(d))) => Ok((NativeType::from_datatype_kwd(&d), state)),
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected datatype".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens),
    }
}

fn on_const(state: ParseState) -> ParseResult<ExpressionState> {
    let (next, state) = state.next();
    let identifier = match next {
        Some(Token::Ident(ident)) => SingleValue::new_identifier_expression(&ident),
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                token_context: t,
            })
        }
        None => return Err(ParseError::NoMoreTokens),
    };
    let peek = state.peek();
    let (type_assertion, state) = if let Some(Token::Symbol(Symbol::Colon)) = peek {
        let state = state.advance();
        let (expr, state) = on_constant_type_assertion(state)?;
        (Some(expr), state)
    } else {
        (None, state)
    };
    let (next, state) = state.next();

    match next {
        Some(Token::Symbol(Symbol::Equals)) => { /* Continue */ }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an equals sign".to_string(),
                token_context: t,
            });
        }
        None => return Err(ParseError::NoMoreTokens),
    }

    let (expr, state) = on_expression(state)?;
    Ok((
        Expression::Operation {
            lhs: identifier.boxed(),
            rhs: expr.boxed(),
            operation: TwoSideOperation::Assignment(Assignment::new(None, type_assertion, true)),
        },
        state,
    ))
}

fn on_while(state: ParseState) -> ParseResult<ExpressionState> {
    let peeked = state.peek();

    if is_new_body(peeked) {
        // handle infinite loops
        let state = state.advance();

        let (body, state) = on_body(state)?;
        return Ok((
            Expression::While {
                condition: None,
                body: body.boxed(),
            },
            state,
        ));
    }

    let (evaluation, state) = on_evaluation(state)?;

    let (next, state) = state.next();

    assert_eq!(Some(Token::Symbol(Symbol::BraceOpen)), next);

    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => { /* Continue */ }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a brace open sign".to_string(),
                token_context: t,
            });
        }
        None => return Err(ParseError::NoMoreTokens),
    }

    let (body, state) = on_body(state)?;
    let while_expr = Expression::While {
        condition: Some(evaluation.boxed()),
        body: body.boxed(),
    };
    Ok((while_expr, state))
}

fn on_iterable(state: ParseState) -> ParseResult<ExpressionState> {
    let (lhs, state) = on_single_value(state)?;
    let peek = state.peek();
    if is_new_body(peek) {
        return Ok((lhs, state));
    };

    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::Range)) => on_range(state, lhs, false),
        Some(Token::Symbol(Symbol::RangeEq)) => on_range(state, lhs, true),

        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected Range or Range Equals operator".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens),
    }
}

fn on_for(state: ParseState) -> ParseResult<ExpressionState> {
    let (scoped_variable, state) = on_single_value(state)?;
    let (next, state) = state.next();

    match next {
        Some(Token::Kwd(Kwd::In)) => { /* Continue */ }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an In keyword".to_string(),
                token_context: t,
            });
        }
        None => return Err(ParseError::NoMoreTokens),
    }

    let (iterable, state) = on_iterable(state)?;
    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => { /* Continue */ }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a opening brace".to_string(),
                token_context: t,
            });
        }
        None => return Err(ParseError::NoMoreTokens),
    }
    let (body, state) = on_body(state)?;

    Ok((
        Expression::For {
            scoped_variable: Box::new(scoped_variable),
            iterable: Box::new(iterable),
            body: Box::new(body),
        },
        state,
    ))
}

fn on_else(state: ParseState) -> ParseResult<ExpressionState> {
    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => on_body(state),
        Some(Token::Kwd(Kwd::If)) => on_if(state),
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected a body or if else".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens),
    }
}

fn on_if(state: ParseState) -> ParseResult<ExpressionState> {
    let (evaluation, state) = on_evaluation(state)?;
    let (next, state) = state.next();

    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => { /* Continue */ }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an In keyword".to_string(),
                token_context: t,
            });
        }
        None => return Err(ParseError::NoMoreTokens),
    }

    let (on_true_evaluation, state) = on_body(state)?;
    let (next, state) = state.next();
    let (on_false_evaluation, state) = if let Some(Token::Kwd(Kwd::Else)) = next {
        let (expr, state) = on_else(state)?;
        (Some(expr), state)
    } else {
        (None, state)
    };
    Ok((
        Expression::If(If::new_boxed(
            evaluation,
            on_true_evaluation,
            on_false_evaluation,
        )),
        state,
    ))
}

fn on_return(state: ParseState) -> ParseResult<ExpressionState> {
    let (e, state) = on_expression(state)?;
    Ok((
        Expression::Statement {
            expression: Some(e.boxed()),
            operation: OneSideOperation::Return,
        },
        state,
    ))
}

pub fn parse<I>(tokens: I) -> ParseResult<Expression>
where
    I: std::iter::IntoIterator<Item = Token>,
{
    let parse_state = ParseState::new(tokens.into_iter().collect::<Vec<Token>>(), 0);
    let (expr, _) = on_expression(parse_state)?;
    Ok(expr)
}

#[cfg(test)]
mod tests {

    use super::*;

    use pretty_assertions::assert_eq;

    fn test(input: Vec<Token>, expected: Expression) {
        let actual = parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn break_statements() {
        // break
        test(
            vec![Token::Kwd(Kwd::Break)],
            Expression::Statement {
                expression: None,
                operation: OneSideOperation::Break,
            },
        );

        // for i in numbers  {
        //     break
        // }

        test(
            vec![
                Token::Kwd(Kwd::For),
                Token::Ident("i".to_string()),
                Token::Kwd(Kwd::In),
                Token::Ident("numbers".to_string()),
                Token::Symbol(Symbol::BraceOpen),
                Token::Kwd(Kwd::Break),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::For {
                scoped_variable: SingleValue::new_identifier_expression("i").boxed(),
                iterable: SingleValue::new_identifier_expression("numbers").boxed(),
                body: Expression::Body(vec![Expression::Statement {
                    expression: None,
                    operation: OneSideOperation::Break,
                }])
                .boxed(),
            },
        );

        // for i in numbers  {
        //     if i < 0 {
        //         break
        //     } else {
        //         print "hi"
        //     }
        // }
        test(
            vec![
                Token::Kwd(Kwd::For),
                Token::Ident("i".to_string()),
                Token::Kwd(Kwd::In),
                Token::Ident("numbers".to_string()),
                Token::Symbol(Symbol::BraceOpen),
                Token::Kwd(Kwd::If),
                Token::Ident("i".to_string()),
                Token::Symbol(Symbol::Evaluation(EvaluationSymbol::Lz)),
                Token::Literal(Literal::Int(0)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Kwd(Kwd::Break),
                Token::Symbol(Symbol::BraceClose),
                Token::Kwd(Kwd::Else),
                Token::Symbol(Symbol::BraceOpen),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("hi".to_string())),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::For {
                scoped_variable: SingleValue::new_identifier_expression("i").boxed(),
                iterable: SingleValue::new_identifier_expression("numbers").boxed(),
                body: Expression::Body(vec![Expression::If(Box::new(If::new(
                    Expression::Evaluation(Evaluation::new(
                        SingleValue::new_identifier_expression("i"),
                        Some(SingleValue::new_value_literal_expression(
                            ValueLiteral::new(NativeType::Int, "0"),
                        )),
                        EvaluationOperator::Lz,
                    )),
                    Expression::Body(vec![Expression::Statement {
                        expression: None,
                        operation: OneSideOperation::Break,
                    }]),
                    Some(Expression::Body(vec![Expression::Operation {
                        lhs: SingleValue::new_identifier_expression("print").boxed(),
                        rhs: Expression::MultipleValues(vec![
                            SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                                "hi",
                            )),
                        ])
                        .boxed(),
                        operation: TwoSideOperation::FunctionCall,
                    }])),
                )))])
                .boxed(),
            },
        );
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
                    operation: TwoSideOperation::FunctionCall,
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
                    operation: TwoSideOperation::FunctionCall,
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
                    operation: TwoSideOperation::FunctionCall,
                }]),
                Some(Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Goodbye",
                        )),
                    ])
                    .boxed(),
                    operation: TwoSideOperation::FunctionCall,
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
                    operation: TwoSideOperation::FunctionCall,
                }]),
                Some(Expression::Body(vec![Expression::Operation {
                    lhs: SingleValue::new_identifier_expression("print").boxed(),
                    rhs: Expression::MultipleValues(vec![
                        SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                            "Goodbye",
                        )),
                    ])
                    .boxed(),
                    operation: TwoSideOperation::FunctionCall,
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
                    operation: TwoSideOperation::FunctionCall,
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
                    operation: TwoSideOperation::FunctionCall,
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
                    operation: TwoSideOperation::FunctionCall,
                }])
                .boxed(),
            },
        );
    }

    #[test]
    fn for_loops() {
        // for x in 0..5 {
        //
        // }
        test(
            vec![
                Token::Kwd(Kwd::For),
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::In),
                Token::Literal(Literal::Int(0)),
                Token::Symbol(Symbol::Range),
                Token::Literal(Literal::Int(5)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::For {
                scoped_variable: SingleValue::new_identifier_expression("x").boxed(),
                iterable: Expression::Range(Range::new(
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Int,
                        "0",
                    ))
                    .boxed(),
                    SingleValue::new_value_literal_expression(ValueLiteral::new(
                        NativeType::Int,
                        "5",
                    ))
                    .boxed(),
                    false,
                ))
                .boxed(),
                body: Expression::Body(Body::new()).boxed(),
            },
        );
        // for x in y..z {
        //
        // }
        test(
            vec![
                Token::Kwd(Kwd::For),
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::In),
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Range),
                Token::Ident("z".to_string()),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::For {
                scoped_variable: SingleValue::new_identifier_expression("x").boxed(),
                iterable: Expression::Range(Range::new(
                    SingleValue::new_identifier_expression("y").boxed(),
                    SingleValue::new_identifier_expression("z").boxed(),
                    false,
                ))
                .boxed(),
                body: Expression::Body(Body::new()).boxed(),
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
                operation: TwoSideOperation::Assignment(Assignment::new(None, None, false)),
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
                    operation: TwoSideOperation::Math(MathOperation::Add),
                }),
                operation: TwoSideOperation::Assignment(Assignment::new(None, None, false)),
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
                    operation: TwoSideOperation::Math(MathOperation::Add),
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
                operation: TwoSideOperation::FunctionCall,
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
                operation: TwoSideOperation::Assignment(Assignment::new(None, None, false)),
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
                operation: TwoSideOperation::FunctionCall,
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
                operation: TwoSideOperation::FunctionCall,
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
                operation: TwoSideOperation::FunctionCall,
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
            operation: TwoSideOperation::Assignment(Assignment::new(None, None, false)),
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

        let expected = Expression::Operation {
            lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                "x".to_string(),
            ))),
            rhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                ValueLiteral::new(NativeType::Int, "5"),
            ))),
            operation: TwoSideOperation::Assignment(Assignment::new(
                None,
                Some(NativeType::Int),
                false,
            )),
        };
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Colon),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected,
        );
        // const x = 5
        let expected = Expression::Operation {
            lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                "x".to_string(),
            ))),
            rhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                ValueLiteral::new(NativeType::Int, "5"),
            ))),
            operation: TwoSideOperation::Assignment(Assignment::new(None, None, true)),
        };
        test(
            vec![
                Token::Kwd(Kwd::Const),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected,
        );

        // const x : Int = 5
        let expected = Expression::Operation {
            lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                "x".to_string(),
            ))),
            rhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                ValueLiteral::new(NativeType::Int, "5"),
            ))),
            operation: TwoSideOperation::Assignment(Assignment::new(
                None,
                Some(NativeType::Int),
                true,
            )),
        };
        test(
            vec![
                Token::Kwd(Kwd::Const),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Colon),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected,
        );
    }
    #[test]
    fn include_kwd() {
        // include "my_package"
        test(
            vec![
                Token::Kwd(Kwd::Include),
                Token::Literal(Literal::String("my_package".to_string())),
            ],
            Expression::Statement {
                expression: Some(
                    SingleValue::new_value_literal_expression(ValueLiteral::new_string(
                        "my_package",
                    ))
                    .boxed(),
                ),
                operation: OneSideOperation::Include,
            },
        )
    }
}
