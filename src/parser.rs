use std::{collections::HashMap, os::macos::raw::stat};

use pretty_assertions::assert_eq;
use For;
use FunctionCall;
use Statement;

use crate::{
    language::{self, *},
    lexer::*,
};

type ExpressionState = (Expression, ParseState);

type ParseResult<T> = Result<T, ParseError>;

use crate::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseState {
    tokens: Vec<Token>,
    position: usize,
}

impl ParseState {
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }
    pub fn advance(self) -> Self {
        ParseState::new(self.tokens, self.position + 1)
    }

    pub fn next(self) -> (Option<Token>, Self) {
        let next = self.tokens.get(self.position).cloned();
        let new_position = self.position + 1;
        let new_state = ParseState::new(self.tokens, new_position);
        println!("Parser state next: {:?}", next);

        (next, new_state)
    }

    pub fn new(tokens: Vec<Token>, position: usize) -> Self {
        Self { tokens, position }
    }
}

#[derive(Debug, PartialEq)]
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
        Some(Token::Literal(l)) => args.push(Expression::new_from_literal(&l)),
        Some(Token::Ident(i)) => args.push(Expression::new_identifier(&i)),
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

fn on_arguments(state: ParseState, first_arg: Expression) -> ParseResult<ArgumentsState> {
    parse_arguments(state, &mut vec![first_arg])
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
            let f = Expression::new_function(ident, params, return_type, body);
            Ok((f, state))
        }
        Some(Token::Symbol(Symbol::Equals)) => {
            let (body, state) = on_expression(state)?;
            let function = Expression::new_function(ident, params, return_type, body);
            Ok((function, state))
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

fn parse_array_literal(
    state: ParseState,
    array: &mut Vec<Expression>,
) -> ParseResult<ExpressionState> {
    let t = state.peek();
    match t {
        Some(Token::Symbol(Symbol::AngClose)) => {
            let (_, state) = state.next();
            Ok((Expression::new_array(array.to_vec()), state))
        }
        Some(Token::Symbol(Symbol::Comma)) => {
            let (_, state) = state.next();
            let peek = state.peek();
            if let Some(Token::Symbol(Symbol::Comma)) = peek {
                return Err(ParseError::InvalidSyntax {
                    message: "Expected an element or a closing angle bracket but found comma."
                        .to_string(),
                    token_context: Token::Symbol(Symbol::Comma),
                });
            }
            parse_array_literal(state, array)
        }
        Some(_) => {
            let (expr, state) = on_expression(state)?;
            array.push(expr);
            parse_array_literal(state, array)
        }
        None => Err(ParseError::NoMoreTokens),
    }
}

fn on_array_literal(state: ParseState) -> ParseResult<ExpressionState> {
    parse_array_literal(state, &mut vec![])
}

fn parse_dict_key(literal: Literal) -> ParseResult<DictionaryKey> {
    match literal {
        Literal::Bool(b) => Ok(DictionaryKey::Bool(b)),
        Literal::Int(i) => Ok(DictionaryKey::Int(i)),
        Literal::String(s) => Ok(DictionaryKey::String(s)),
        Literal::Float(f) => Err(ParseError::InvalidSyntax {
            message: "Float cannot be used as key to Dictionary".to_string(),
            token_context: Token::Literal(Literal::Float(f)),
        }),
    }
}
fn parse_dict_literal(
    state: ParseState,
    map: &mut HashMap<DictionaryKey, Expression>,
) -> ParseResult<ExpressionState> {
    let (t, state) = state.next();

    match t {
        Some(Token::Ident(i)) => {
            let (t, state) = state.next();
            match t {
                Some(Token::Symbol(Symbol::Colon)) => {
                    let (rhs, state) = on_expression(state)?;

                    map.insert(DictionaryKey::Identifier(i), rhs);

                    let (t, state) = state.next();
                    match t {
                        Some(Token::Symbol(Symbol::Comma)) => parse_dict_literal(state, map),
                        Some(t) => Err(ParseError::InvalidSyntax {
                            message: "Expected a comma".to_string(),
                            token_context: t,
                        }),
                        None => Err(ParseError::NoMoreTokens),
                    }
                }
                Some(t) => Err(ParseError::InvalidSyntax {
                    message: "Expected a colon".to_string(),
                    token_context: t,
                }),
                None => Err(ParseError::NoMoreTokens),
            }
        }
        Some(Token::Literal(l)) => {
            let key = parse_dict_key(l)?;

            let (t, state) = state.next();
            match t {
                Some(Token::Symbol(Symbol::Colon)) => {
                    let (rhs, state) = on_expression(state)?;
                    map.insert(key, rhs);
                    let t = state.peek();
                    match t {
                        Some(Token::Symbol(Symbol::Comma)) => parse_dict_literal(state, map),
                        Some(Token::Symbol(Symbol::BraceClose)) => {
                            Ok((Expression::new_dict(map.to_owned()), state))
                        }

                        Some(t) => Err(ParseError::InvalidSyntax {
                            message: "Expected a comma".to_string(),
                            token_context: t.clone(),
                        }),
                        None => Err(ParseError::NoMoreTokens),
                    }
                }
                Some(t) => Err(ParseError::InvalidSyntax {
                    message: "Expected a colon".to_string(),
                    token_context: t,
                }),
                None => Err(ParseError::NoMoreTokens),
            }
        }
        Some(Token::Symbol(Symbol::BraceClose)) => {
            Ok((Expression::new_dict(map.to_owned()), state))
        }

        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected an identifier, literal, or closing brace".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens),
    }
}

fn on_dict_literal(state: ParseState) -> ParseResult<ExpressionState> {
    parse_dict_literal(state, &mut HashMap::new())
}

fn on_assignment(
    state: ParseState,
    ident: &str,
    type_assertion: Option<NativeType>,
) -> ParseResult<ExpressionState> {
    let (t, state) = state.next();
    match t {
        Some(Token::Literal(l)) => {
            let sv = Expression::new_from_literal(&l);
            Ok((
                Expression::new_assignment(ident, sv, None, type_assertion, false),
                state,
            ))
        }
        Some(Token::Symbol(Symbol::AngOpen)) => {
            let (expr, state) = on_array_literal(state)?;
            Ok((
                Expression::new_assignment(ident, expr, None, type_assertion, false),
                state,
            ))
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            let (expr, state) = on_dict_literal(state)?;
            Ok((
                Expression::new_assignment(ident, expr, None, type_assertion, false),
                state,
            ))
        }
        Some(Token::Ident(i)) => {
            let (rhs, state) = on_identifier(state, &i)?;
            Ok((
                Expression::new_assignment(ident, rhs, None, type_assertion, false),
                state,
            ))
        }
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected a valid assignment: literal value, identifier, or array syntax"
                .to_string(),
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

    match t {
        Some(Token::Kwd(Kwd::In)) | Some(Token::Symbol(Symbol::Comma)) => {
            return Ok((Expression::Identifier(ident.to_string()), state));
        }
        _ => { /* Continue */ }
    };
    let (t, state) = state.next();
    match t {
        // Assignment
        Some(Token::Symbol(Symbol::Equals)) => Ok(on_assignment(state, ident, None)?),

        Some(Token::Symbol(Symbol::Math(t))) => {
            let operation = Math::from_token(&t);
            Ok(on_math_expression(
                state,
                operation,
                Expression::new_identifier(ident),
            )?)
        }

        // Operations
        // Function call if value literal or string
        Some(Token::Literal(l)) => Ok(on_function_call(state, ident, &l)?),
        Some(Token::Symbol(Symbol::Range)) => {
            Ok(on_range(state, Expression::new_identifier(ident), false)?)
        }
        Some(Token::Symbol(Symbol::RangeEq)) => {
            Ok(on_range(state, Expression::new_identifier(ident), true)?)
        }
        Some(Token::Symbol(Symbol::ParenOpen)) => Ok(on_function(state, ident)?),
        Some(Token::Kwd(Kwd::DataType(d))) => Ok(on_variable_type_assertion(state, ident, d)?),
        _ => Ok((Expression::new_identifier(ident), state)),
    }
}

fn on_variable_type_assertion(
    state: ParseState,
    ident: &str,
    d: DataTypeKwd,
) -> ParseResult<ExpressionState> {
    let kwd = NativeType::from_datatype_kwd(&d);
    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::Equals)) => Ok(on_assignment(state, ident, Some(kwd))?),
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected an equals sign".to_string(),
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
    let (args, state) = on_arguments(state, Expression::new_from_literal(first_arg))?;
    Ok((
        Expression::FunctionCall(FunctionCall {
            identifier: ident.to_string(),
            args,
        }),
        state,
    ))
}

fn on_math_expression(
    state: ParseState,
    operation: Math,
    lhs: Expression,
) -> ParseResult<ExpressionState> {
    let (rhs, state) = on_expression(state)?;
    Ok((Expression::new_math_expression(lhs, rhs, operation), state))
}

fn on_single_value(state: ParseState) -> ParseResult<ExpressionState> {
    let (next, state) = state.next();
    let single_value = match next {
        Some(Token::Literal(l)) => Expression::new_from_literal(&l),
        Some(Token::Ident(i)) => Expression::new_identifier(&i),
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
            Expression::new_evaluation(lhs, None, EvaluationOperator::BooleanTruthy),
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
        Expression::new_evaluation(lhs, Some(rhs), evaluation_operator),
        state,
    ))
}

fn on_range(state: ParseState, lhs: Expression, inclusive: bool) -> ParseResult<ExpressionState> {
    let (rhs, state) = on_single_value(state)?;
    Ok((Expression::new_range(lhs, rhs, inclusive), state))
}

fn on_expression(state: ParseState) -> ParseResult<ExpressionState> {
    let (next, state) = state.next();

    let expr = match next.ok_or::<ParseError>(ParseError::NoMoreTokens)? {
        Token::Ident(ident) => on_identifier(state, &ident)?,
        Token::Kwd(k) => on_keyword(state, &k)?,
        Token::Literal(l) => (Expression::new_from_literal(&l), state),
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
            Expression::new_statement(Some(Expression::new_string(&s)), StatementOperator::Include),
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
            Expression::Statement(Statement {
                expression: None,
                operation: StatementOperator::Break,
            }),
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

fn on_const(state: ParseState) -> ParseResult<ExpressionState> {
    let (next, state) = state.next();
    let identifier = match next {
        Some(Token::Ident(ident)) => ident,
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                token_context: t,
            })
        }
        None => return Err(ParseError::NoMoreTokens),
    };
    let peek = state.peek();
    let (type_assertion, state) = if let Some(Token::Kwd(Kwd::DataType(d))) = peek {
        let expr = NativeType::from_datatype_kwd(d);
        let state = state.advance();
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
        Expression::new_assignment(&identifier, expr, None, type_assertion, true),
        state,
    ))
}

fn on_while(state: ParseState) -> ParseResult<ExpressionState> {
    let peeked = state.peek();

    if is_new_body(peeked) {
        // handle infinite loops
        let state = state.advance();

        let (body, state) = on_body(state)?;
        return Ok((Expression::new_while(None, body), state));
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
    let while_expr = Expression::new_while(Some(evaluation), body);
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
        Expression::For(For {
            scoped_variable: Box::new(scoped_variable),
            iterable: Box::new(iterable),
            body: Box::new(body),
        }),
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
        Expression::new_if(evaluation, on_true_evaluation, on_false_evaluation),
        state,
    ))
}

fn on_return(state: ParseState) -> ParseResult<ExpressionState> {
    let (e, state) = on_expression(state)?;
    Ok((
        Expression::new_statement(Some(e), StatementOperator::Return),
        state,
    ))
}

pub fn parse<I>(tokens: I) -> ParseResult<Expression>
where
    I: std::iter::IntoIterator<Item = Token>,
{
    let tokens = tokens
        .into_iter()
        .filter(|each| each != &Token::whitespace())
        .collect::<Vec<Token>>();
    let parse_state = ParseState::new(tokens, 0);
    let (expr, _) = on_expression(parse_state)?;
    Ok(expr)
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use super::*;

    use pretty_assertions::assert_eq;

    fn test(input: Vec<Token>, expected: Expression) {
        let actual = parse(input).unwrap();
        assert_eq!(expected, actual);
    }
    fn test_err(input: Vec<Token>, expected: ParseError) {
        let actual = parse(input).unwrap_err();
        assert_eq!(expected, actual);
    }

    #[test]
    fn break_statements() {
        // break
        test(
            vec![Token::Kwd(Kwd::Break)],
            Expression::new_statement(None, StatementOperator::Break),
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
            Expression::new_for(
                Expression::new_identifier("i"),
                Expression::new_identifier("numbers"),
                Expression::new_body(vec![Expression::new_statement(
                    None,
                    StatementOperator::Break,
                )]),
            ),
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
            Expression::new_for(
                Expression::new_identifier("i"),
                Expression::new_identifier("numbers"),
                Expression::Body(vec![Expression::new_if(
                    Expression::new_evaluation(
                        Expression::new_identifier("i"),
                        Some(Expression::new_int(0)),
                        EvaluationOperator::Lz,
                    ),
                    Expression::new_body(vec![Expression::new_statement(
                        None,
                        StatementOperator::Break,
                    )]),
                    Some(Expression::new_body(vec![Expression::new_function_call(
                        "print",
                        vec![Expression::new_string("hi")],
                    )])),
                )]),
            ),
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
            Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_bool(true),
                    None,
                    EvaluationOperator::BooleanTruthy,
                ),
                Expression::Body(vec![
                    // empty body
                ]),
                None,
            ),
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
            Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_int(3),
                    Some(Expression::new_int(3)),
                    EvaluationOperator::Eq,
                ),
                Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
                None,
            ),
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
            Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_identifier("x"),
                    None,
                    EvaluationOperator::BooleanTruthy,
                ),
                Expression::Body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
                None,
            ),
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
            Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_bool(true),
                    None,
                    EvaluationOperator::BooleanTruthy,
                ),
                Expression::Body(vec![
                    // empty body
                ]),
                Some(Expression::Body(vec![])),
            ),
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
            Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_int(3),
                    Some(Expression::new_int(3)),
                    EvaluationOperator::Eq,
                ),
                Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
                Some(Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Goodbye")],
                )])),
            ),
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
            Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_identifier("x"),
                    None,
                    EvaluationOperator::BooleanTruthy,
                ),
                Expression::Body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
                Some(Expression::Body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Goodbye")],
                )])),
            ),
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
            Expression::new_while(None, Expression::new_body(vec![])),
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
            Expression::new_while(
                None,
                Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
            ),
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
            Expression::new_while(
                Some(Expression::new_evaluation(
                    Expression::new_int(3),
                    Some(Expression::new_int(3)),
                    EvaluationOperator::Eq,
                )),
                Expression::Body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
            ),
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
            Expression::new_while(
                Some(Expression::new_evaluation(
                    Expression::new_identifier("x"),
                    None,
                    EvaluationOperator::BooleanTruthy,
                )),
                Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
            ),
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
            Expression::new_for(
                Expression::new_identifier("x"),
                Expression::new_range(Expression::new_int(0), Expression::new_int(5), false),
                Expression::Body(Body::new()),
            ),
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
            Expression::new_for(
                Expression::new_identifier("x"),
                Expression::new_range(
                    Expression::new_identifier("y"),
                    Expression::new_identifier("z"),
                    false,
                ),
                Expression::Body(Body::new()),
            ),
        );
    }

    #[test]
    fn variable_assignment_with_another_variable() {
        // y = x
        test(
            vec![
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
            ],
            Expression::new_assignment("y", Expression::new_identifier("x"), None, None, false),
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
            Expression::new_assignment(
                "y",
                Expression::MathOperation(MathOperation {
                    lhs: Box::new(Expression::new_identifier("x")),
                    rhs: Box::new(Expression::new_int(3)),
                    operation: Math::Add,
                }),
                None,
                None,
                false,
            ),
        );

        test(
            vec![
                Token::Ident("y".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Math(MathSymbol::Plus)),
                Token::Literal(Literal::Int(3)),
            ],
            Expression::new_assignment(
                "y",
                Expression::MathOperation(MathOperation {
                    lhs: Box::new(Expression::new_identifier("x")),
                    rhs: Box::new(Expression::new_int(3)),
                    operation: Math::Add,
                }),
                None,
                Some(NativeType::Int),
                false,
            ),
        );
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
            Expression::new_function("do_nothing", vec![], None, Expression::Body(vec![])),
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
            Expression::new_function(
                "do_nothing",
                vec![],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ),
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
            Expression::new_function(
                "do_nothing",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                None,
                Expression::Body(vec![]),
            ),
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
            Expression::new_function(
                "do_nothing",
                vec![
                    Parameter::new("numOne", Some(NativeType::Int)),
                    Parameter::new("numTwo", Some(NativeType::Int)),
                ],
                None,
                Expression::Body(vec![]),
            ),
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
            Expression::new_function(
                "do_nothing",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ),
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
            Expression::new_function(
                "do_nothing",
                vec![
                    Parameter::new("numOne", Some(NativeType::Int)),
                    Parameter::new("numTwo", Some(NativeType::Int)),
                ],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ),
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
            Expression::new_function(
                "add",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                None,
                Expression::new_math_expression(
                    Expression::new_identifier("numOne"),
                    Expression::new_identifier("numTwo"),
                    Math::Add,
                ),
            ),
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
            Expression::FunctionCall(FunctionCall {
                identifier: "print".to_string(),
                args: vec![Expression::new_string("Hello")],
            }),
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
            Expression::new_assignment("x", Expression::new_float(3.5), None, None, false),
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
            Expression::new_function_call(
                "print",
                vec![
                    Expression::new_string("Hello"),
                    Expression::new_string("World"),
                ],
            ),
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
            Expression::FunctionCall(FunctionCall {
                identifier: "print".to_string(),
                args: vec![
                    Expression::new_string("Hello"),
                    Expression::new_identifier("x"),
                ],
            }),
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
            Expression::new_function_call(
                "print",
                vec![Expression::new_string("Hello"), Expression::new_bool(true)],
            ),
        );
    }

    #[test]
    fn literal_assignment() {
        // "x=5"
        let expected = Expression::new_assignment("x", Expression::new_int(5), None, None, false);
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
                Token::whitespace(),
                Token::Symbol(Symbol::Equals),
                Token::whitespace(),
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

        let expected = Expression::new_assignment(
            "x",
            Expression::new_int(5),
            None,
            Some(NativeType::Int),
            false,
        );
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected,
        );
        // const x = 5
        let expected = Expression::new_assignment("x", Expression::new_int(5), None, None, true);
        test(
            vec![
                Token::Kwd(Kwd::Const),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected,
        );

        // const x Int = 5
        let expected = Expression::new_assignment(
            "x",
            Expression::new_int(5),
            None,
            Some(NativeType::Int),
            true,
        );
        test(
            vec![
                Token::Kwd(Kwd::Const),
                Token::Ident("x".to_string()),
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
            Expression::new_statement(
                Some(Expression::new_string("my_package")),
                StatementOperator::Include,
            ),
        )
    }

    #[test]
    fn array_literals() {
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Arr)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::AngOpen),
                Token::Symbol(Symbol::AngClose),
            ],
            Expression::new_assignment(
                "x",
                Expression::new_array(vec![]),
                None,
                Some(NativeType::Array),
                false,
            ),
        );

        // x = [3]
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Arr)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::AngOpen),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::AngClose),
            ],
            Expression::new_assignment(
                "x",
                Expression::new_array(vec![Expression::new_from_literal(&Literal::Int(3))]),
                None,
                Some(NativeType::Array),
                false,
            ),
        );

        // x = [3, 4]
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Arr)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::AngOpen),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Comma),
                Token::Literal(Literal::Int(4)),
                Token::Symbol(Symbol::AngClose),
            ],
            Expression::new_assignment(
                "x",
                Expression::new_array(vec![
                    Expression::new_from_literal(&Literal::Int(3)),
                    Expression::new_from_literal(&Literal::Int(4)),
                ]),
                None,
                Some(NativeType::Array),
                false,
            ),
        );

        // x = [3, 4,]
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Arr)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::AngOpen),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Comma),
                Token::Literal(Literal::Int(4)),
                Token::Symbol(Symbol::Comma),
                Token::Symbol(Symbol::AngClose),
            ],
            Expression::new_assignment(
                "x",
                Expression::new_array(vec![
                    Expression::new_from_literal(&Literal::Int(3)),
                    Expression::new_from_literal(&Literal::Int(4)),
                ]),
                None,
                Some(NativeType::Array),
                false,
            ),
        );

        // x = [3,,4,] == invalid
        test_err(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Arr)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::AngOpen),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Comma),
                Token::Symbol(Symbol::Comma),
                Token::Literal(Literal::Int(4)),
                Token::Symbol(Symbol::Comma),
                Token::Symbol(Symbol::AngClose),
            ],
            ParseError::InvalidSyntax {
                message: "Expected an element or a closing angle bracket but found comma."
                    .to_string(),
                token_context: Token::Symbol(Symbol::Comma),
            },
        );
    }
    #[test]
    fn dictionary_literals() {
        // x dict = {}
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Dict)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_assignment(
                "x",
                Expression::new_dict(HashMap::new()),
                None,
                Some(NativeType::Dictionary),
                false,
            ),
        );

        // x dict = {
        //  "hello": "world"
        // }
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Dict)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::BraceOpen),
                Token::Literal(Literal::String("hello".to_string())),
                Token::Symbol(Symbol::Colon),
                Token::Literal(Literal::String("world".to_string())),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_assignment(
                "x",
                Expression::new_dict(HashMap::from([(
                    DictionaryKey::String("hello".to_string()),
                    Expression::new_string("world"),
                )])),
                None,
                Some(NativeType::Dictionary),
                false,
            ),
        );
        // x = {"hello": "world",,}
        test_err(
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Dict)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::BraceOpen),
                Token::Literal(Literal::String("hello".to_string())),
                Token::Symbol(Symbol::Colon),
                Token::Literal(Literal::String("world".to_string())),
                Token::Symbol(Symbol::Comma),
                Token::Symbol(Symbol::Comma), // invalid
                Token::Symbol(Symbol::BraceClose),
            ],
            ParseError::InvalidSyntax {
                message: "Expected an identifier, literal, or closing brace".to_string(),
                token_context: Token::Symbol(Symbol::Comma),
            },
        );
    }
}
