use std::collections::HashMap;

use token::*;
use For;
use FunctionCall;
use Statement;

use crate::dbg;
use crate::{language::*, lexer::*};

type ParseResult<T> = Result<T, ParseError>;

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

        (next, new_state)
    }
    pub fn step_back(self) -> Self {
        ParseState::new(self.tokens, self.position - 1)
    }
    pub fn new(tokens: Vec<Token>, position: usize) -> Self {
        Self { tokens, position }
    }
    pub fn end(&self) -> bool {
        self.peek().is_none()
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    InvalidSyntax {
        message: String,
        token_context: Token,
    },
    NoMoreTokens {
        context: String,
    },
    ExpectedLhs,
}

pub fn parse<I>(tokens: I) -> ParseResult<Expression>
where
    I: std::iter::IntoIterator<Item = Token>,
{
    let tokens: Vec<Token> = tokens
        .into_iter()
        .filter(|each| each != &Token::whitespace())
        .filter(|each| {
            !matches!(each, Token::Symbol(Symbol::Comment(_)))
                && !matches!(each, Token::Symbol(Symbol::MultilineComment(_)))
        })
        .collect();

    let tokens: Vec<Token> = tokens
        .into_iter()
        .rev()
        .skip_while(|t| t == &Token::Symbol(Symbol::Newline))
        .collect();

    let tokens = tokens.into_iter().rev().collect::<Vec<Token>>();

    let mut top_level_body: Body = vec![];
    let mut parse_state = ParseState::new(tokens, 0);
    while !parse_state.end() {
        let (expr, new_state) = parse_expression(parse_state, None, None)?;
        top_level_body.push(expr);
        parse_state = new_state;
    }

    Ok(Expression::Body(top_level_body))
}

type ArgumentsState = (Vec<Expression>, ParseState);

fn parse_arguments(state: ParseState, args: &mut Vec<Expression>) -> ParseResult<ArgumentsState> {
    let peek = state.peek();

    match peek {
        Some(Token::Symbol(Symbol::Newline)) => return Ok((args.to_vec(), state.advance())),
        Some(Token::Symbol(Symbol::BraceClose)) => return Ok((args.to_vec(), state)),
        None => return Ok((args.to_vec(), state.advance())),
        _ => (),
    }

    let (next, state) = state.next();
    let state = match next {
        Some(Token::Literal(l)) => {
            args.push(Expression::new_from_literal(&l));
            state
        }
        Some(Token::Ident(i)) => {
            args.push(Expression::new_identifier(&i));
            state
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            let (body, state) = parse_body(state, &mut vec![], None)?;
            args.push(Expression::new_body(body));
            state
        }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected parameter notation".to_string(),
                token_context: t,
            })
        }
        None => return Ok((args.to_vec(), state)),
    };
    parse_arguments(state, args)
}
fn parse_parameter_identifier(state: ParseState, ident: &str) -> (Parameter, ParseState) {
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
        Some(Token::Ident(ident)) => parse_parameter_identifier(state, &ident),
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a parameter identifier".to_string(),
                token_context: t,
            })
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "Parse parse_parameters".to_string(),
            })
        }
    };
    parameters.push(parameter);

    parse_parameters(state, parameters)
}

type ParametersState = (FunctionParameters, ParseState);

fn parse_function(state: ParseState, ident: &str) -> ParseResult<(Expression, ParseState)> {
    let (params, state) = parse_parameters(state, &mut vec![])?;
    let (return_type, state) = parse_return_type(state)?;

    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // Body
            let (body, state) = parse_body(state, &mut vec![], None)?;
            let f =
                Expression::new_function(ident, params, return_type, Expression::new_body(body));
            Ok((f, state))
        }
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected a function body or expression".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens {
            context: "on_function".to_string(),
        }),
    }
}

type ReturnState = (Option<NativeType>, ParseState);

fn parse_return_type(state: ParseState) -> ParseResult<ReturnState> {
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
                None => {
                    return Err(ParseError::NoMoreTokens {
                        context: "on_return_type".to_string(),
                    })
                }
            };
            Ok((ret_type, state))
        }
        _ => Ok((None, state)),
    }
}
type BodyState = (Body, ParseState);

fn parse_body(
    state: ParseState,
    body: &mut Body,
    previous: Option<Expression>,
) -> ParseResult<BodyState> {
    let peek = state.peek();

    match peek {
        Some(Token::Symbol(Symbol::BraceClose)) => return Ok((body.to_vec(), state.advance())),
        // WARNING: This might be a bug passing previous through
        Some(Token::Symbol(Symbol::Newline)) => return parse_body(state.advance(), body, previous),
        _ => (),
    }

    let (expression, state) = parse_expression(state, previous, None)?;
    dbg!("Body statement", &expression);
    match state.peek() {
        Some(Token::Symbol(Symbol::Op(_))) => parse_body(state, body, Some(expression)),
        _ => {
            body.push(expression);
            parse_body(state, body, None)
        }
    }
}

fn parse_array_literal(
    state: ParseState,
    array: &mut Vec<Expression>,
) -> ParseResult<(Expression, ParseState)> {
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
            let (expr, state) = parse_expression(state, None, None)?;
            array.push(expr);
            parse_array_literal(state, array)
        }
        None => Err(ParseError::NoMoreTokens {
            context: "Parse array literal".to_string(),
        }),
    }
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
    dict: &mut HashMap<DictionaryKey, Expression>,
) -> ParseResult<(Expression, ParseState)> {
    let (t, state) = state.next();
    match t {
        Some(Token::Ident(i)) => {
            let (t, state) = state.next();
            match t {
                Some(Token::Symbol(Symbol::Colon)) => {
                    let (rhs, state) = parse_expression(state, None, None)?;

                    dict.insert(DictionaryKey::Identifier(i), rhs);

                    let (t, state) = state.next();
                    match t {
                        Some(Token::Symbol(Symbol::Comma)) => parse_dict_literal(state, dict),
                        Some(Token::Symbol(Symbol::Newline)) => parse_dict_literal(state, dict),
                        Some(t) => Err(ParseError::InvalidSyntax {
                            message: "Expected a comma".to_string(),
                            token_context: t,
                        }),
                        None => Err(ParseError::NoMoreTokens {
                            context: "parse_dict_literal".to_string(),
                        }),
                    }
                }
                Some(t) => Err(ParseError::InvalidSyntax {
                    message: "Expected a colon".to_string(),
                    token_context: t,
                }),
                None => Err(ParseError::NoMoreTokens {
                    context: "parse_dict_literal".to_string(),
                }),
            }
        }
        Some(Token::Literal(l)) => {
            let (t, state) = state.next();
            match t {
                Some(Token::Symbol(Symbol::Colon)) => {
                    let key = parse_dict_key(l)?;
                    let (rhs, state) = parse_expression(state, None, None)?;
                    dict.insert(key, rhs);
                    let (t, state) = state.next();
                    match t {
                        Some(Token::Symbol(Symbol::Comma))
                        | Some(Token::Symbol(Symbol::Newline)) => parse_dict_literal(state, dict),
                        Some(Token::Symbol(Symbol::BraceClose)) => {
                            Ok((Expression::new_dict(dict.to_owned()), state))
                        }

                        Some(t) => Err(ParseError::InvalidSyntax {
                            message: "Expected a comma".to_string(),
                            token_context: t.clone(),
                        }),
                        None => Err(ParseError::NoMoreTokens {
                            context: "parse_dict_literal".to_string(),
                        }),
                    }
                }
                Some(Token::Symbol(Symbol::Newline)) => parse_dict_literal(state, dict),
                Some(t) => Err(ParseError::InvalidSyntax {
                    message: "Expected a colon".to_string(),
                    token_context: t,
                }),
                None => Err(ParseError::NoMoreTokens {
                    context: "parse_dict_literal".to_string(),
                }),
            }
        }
        Some(Token::Symbol(Symbol::Newline)) => parse_dict_literal(state, dict),
        Some(Token::Symbol(Symbol::BraceClose)) => {
            Ok((Expression::new_dict(dict.to_owned()), state))
        }

        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected an identifier, literal, or closing brace".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens {
            context: "parse_dict_literal".to_string(),
        }),
    }
}

fn parse_minus(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    let (next, state) = state.next();
    let (neg_num, state) = match next {
        Some(Token::Literal(Literal::Int(i))) => (Expression::new_int(-i), state),
        Some(Token::Literal(Literal::Float(f))) => (Expression::new_float(-f), state),
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an integer literal".to_string(),
                token_context: t,
            })
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_minus".to_string(),
            })
        }
    };
    let peek = state.peek();
    match peek {
        Some(Token::Symbol(Symbol::Op(op))) => {
            let op = op.clone();
            let state = state.advance();
            parse_operator(state, Operator::from_op_symbol(&op), None, neg_num)
        }
        _ => Ok((neg_num, state)),
    }
}

fn parse_assignment(
    state: ParseState,
    ident: &str,
    type_assertion: Option<NativeType>,
) -> ParseResult<(Expression, ParseState)> {
    let (t, state) = state.next();

    let (rhs, state) = match t {
        Some(Token::Symbol(Symbol::Op(OpSymbol::Minus))) => parse_minus(state)?,
        Some(Token::Literal(l)) => parse_literal(state, l, None)?,
        Some(Token::Symbol(Symbol::AngOpen)) => parse_array_literal(state, &mut vec![])?,
        Some(Token::Symbol(Symbol::BraceOpen)) => parse_brace_open(state)?,
        Some(Token::Ident(i)) => {
            let is_evaluation = matches!(state.peek(), Some(Token::Symbol(Symbol::Evaluation(_))));

            if is_evaluation {
                parse_evaluation(state, Expression::new_identifier(&i))?
            } else {
                parse_identifier(state, &i)?
            }
        }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a valid assignment: literal value, identifier, or array syntax"
                    .to_string(),
                token_context: t,
            })
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_assignment".to_string(),
            })
        }
    };
    dbg!("{:?}", &rhs);
    let (rhs, state) = match state.peek().cloned() {
        Some(Token::Symbol(Symbol::Op(op))) => {
            parse_operator(state.advance(), Operator::from_op_symbol(&op), None, rhs)?
        }
        _ => (rhs, state),
    };
    Ok((
        Expression::new_assignment(ident, rhs, None, type_assertion, false),
        state,
    ))
}

fn parse_brace_open(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    let peek = state.peek();
    if let Some(Token::Symbol(Symbol::Newline)) = peek {
        return parse_brace_open(state.advance());
    }
    let (next, state) = state.next();
    if let Some(Token::Symbol(Symbol::Colon)) = next {
        return Ok((Expression::new_dict(HashMap::new()), state.advance()));
    }

    match state.peek() {
        Some(Token::Symbol(Symbol::Newline)) => parse_brace_open(state),
        Some(Token::Symbol(Symbol::Colon)) => {
            parse_dict_literal(state.step_back(), &mut HashMap::new())
        }
        Some(t) => {
            // previous is part of a body expression, so handle that...
            parse_body(state.step_back(), &mut vec![], None)
                .map(|(expr, state)| (Expression::new_body(expr), state))
        }
        None => parse_body(state.step_back(), &mut vec![], None)
            .map(|(expr, state)| (Expression::new_body(expr), state)),
    }
}

fn parse_identifier(state: ParseState, ident: &str) -> ParseResult<(Expression, ParseState)> {
    let t = state.peek();

    match t {
        Some(Token::Kwd(Kwd::In)) | Some(Token::Symbol(Symbol::Comma)) => {
            return Ok((Expression::Identifier(ident.to_string()), state));
        }
        Some(Token::Symbol(Symbol::AngClose)) => {
            return Ok((Expression::new_identifier(ident), state))
        }
        _ => { /* Continue */ }
    };
    let (t, state) = state.next();
    match t {
        // Assignment
        Some(Token::Symbol(Symbol::Equals)) => parse_assignment(state, ident, None),
        Some(Token::Symbol(Symbol::Op(t))) => parse_operator(
            state,
            Operator::from_op_symbol(&t),
            None,
            Expression::new_identifier(ident),
        ),
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            let (body, state) = parse_body(state, &mut vec![], None)?;
            parse_function_call(state, ident, Expression::new_body(body))
        }
        // Operations
        // Function call if value literal or string
        Some(Token::Literal(l)) => {
            parse_function_call(state, ident, Expression::new_from_literal(&l))
        }
        Some(Token::Symbol(Symbol::Range)) => {
            let state = state.advance();
            parse_range(state, Expression::new_identifier(ident), false)
        }
        Some(Token::Symbol(Symbol::RangeEq)) => {
            let state = state.advance();
            parse_range(state, Expression::new_identifier(ident), true)
        }
        Some(Token::Symbol(Symbol::ParenOpen)) => parse_function(state, ident),
        Some(Token::Symbol(Symbol::AngOpen)) => {
            parse_collection_index(state, Expression::new_identifier(ident))
        }
        Some(Token::Ident(i)) => parse_function_call(state, ident, Expression::new_identifier(&i)),
        Some(Token::Kwd(Kwd::DataType(d))) => parse_variable_type_assertion(state, ident, d),
        _ => Ok((Expression::new_identifier(ident), state)),
    }
}

fn parse_collection_index(
    state: ParseState,
    collection: Expression,
) -> ParseResult<(Expression, ParseState)> {
    let (next, state) = state.next();
    let (index, state) = match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            let (body, state) = parse_body(state, &mut vec![], None)?;
            (Expression::new_body(body), state)
        }
        Some(Token::Ident(i)) => parse_identifier(state, &i)?,
        Some(Token::Literal(l)) => parse_literal(state, l, None)?,
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a brace open, ident or literal".to_string(),
                token_context: t,
            })
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_collection_index".to_string(),
            })
        }
    };

    dbg!(&index);

    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::AngClose)) => { /* Continue */ }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a closing bracket".to_string(),
                token_context: t,
            })
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_collection_index".to_string(),
            })
        }
    }
    Ok((Expression::new_index(collection, index), state))
}

fn parse_variable_type_assertion(
    state: ParseState,
    ident: &str,
    d: DataTypeKwd,
) -> ParseResult<(Expression, ParseState)> {
    let kwd = NativeType::from_datatype_kwd(&d);
    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::Equals)) => Ok(parse_assignment(state, ident, Some(kwd))?),
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected an equals sign".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens {
            context: "on_variable_type_assertion".to_string(),
        }),
    }
}

fn parse_function_call(
    state: ParseState,
    ident: &str,
    first_arg: Expression,
) -> ParseResult<(Expression, ParseState)> {
    let (args, state) = {
        let first_arg = first_arg;
        parse_arguments(state, &mut vec![first_arg])
    }?;
    Ok((
        Expression::FunctionCall(FunctionCall {
            identifier: ident.to_string(),
            args,
        }),
        state,
    ))
}

fn parse_operator(
    state: ParseState,
    current: Operator,
    previous_op: Option<Operator>,
    lhs: Expression,
) -> ParseResult<(Expression, ParseState)> {
    if let Some(prev) = previous_op.clone() {
        let precedence = current > prev;
        if !precedence {
            dbg!("Precedence Current {:?} < Prev {:?}", &current, prev);
            // the previous op has greater precedence
            // therefore, we want to give it's rhs (or our lhs in this case).
            dbg!("Returning {:?}", &lhs);
            return Ok((lhs, state.step_back()));
        }
    }
    // the current op, if any, is more important than previous op
    // Therefore we want it lower in the tree.
    // So we want the lhs passed in to be our lhs and the rhs should be whatever we get
    // This will then get returned back up to us to construct our expression

    let (rhs, state) = parse_expression(state, Some(lhs.clone()), Some(current.clone()))?;
    let new_lhs = Expression::new_math_expression(lhs, rhs, current.clone());

    let peek = state.peek().cloned();
    match peek {
        Some(Token::Symbol(Symbol::Op(op))) => {
            parse_operator(state, Operator::from_op_symbol(&op), None, new_lhs)
        }
        _ => Ok((new_lhs, state)),
    }
}

fn parse_single_value(state: ParseState) -> ParseResult<(Expression, ParseState)> {
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
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_single_value".to_string(),
            })
        }
    };
    Ok((single_value, state))
}

fn parse_evaluation(state: ParseState, lhs: Expression) -> ParseResult<(Expression, ParseState)> {
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
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_evaluation".to_string(),
            })
        }
    };

    let (rhs, state) = parse_single_value(state)?;
    Ok((
        Expression::new_evaluation(lhs, Some(rhs), evaluation_operator),
        state,
    ))
}

fn parse_range(
    state: ParseState,
    lhs: Expression,
    inclusive: bool,
) -> ParseResult<(Expression, ParseState)> {
    let (rhs, state) = parse_single_value(state)?;
    Ok((Expression::new_range(lhs, rhs, inclusive), state))
}

fn parse_expression(
    state: ParseState,
    lhs: Option<Expression>,
    previous_op: Option<Operator>,
) -> ParseResult<(Expression, ParseState)> {
    let (next, state) = state.next();
    dbg!("Expr {:?}", &next);
    let (expr, state) = match next {
        Some(Token::Ident(ident)) => parse_identifier(state, &ident)?,
        Some(Token::Kwd(k)) => parse_keyword(state, &k)?,
        Some(Token::Literal(l)) => parse_literal(state, l, previous_op.clone())?,
        Some(Token::Symbol(Symbol::BraceOpen)) => parse_body(state, &mut vec![], lhs)
            .map(|(body, state)| (Expression::new_body(body), state))?,
        Some(Token::Symbol(Symbol::Newline)) => parse_expression(state, lhs, previous_op.clone())?,
        Some(Token::Symbol(Symbol::Op(op))) => parse_operator(
            state,
            Operator::from_op_symbol(&op),
            previous_op.clone(),
            lhs.ok_or(ParseError::ExpectedLhs)?,
        )?,

        Some(Token::Symbol(Symbol::BraceClose)) => return Ok((lhs.unwrap(), state.step_back())),
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an identifier, keyword, literal, body open or newline"
                    .to_string(),
                token_context: t,
            })
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_expression".to_string(),
            })
        }
    };

    match state.peek() {
        Some(Token::Symbol(Symbol::BraceClose)) => Ok((expr, state)),
        _ => Ok((expr, state)),
    }
}

fn parse_literal(
    state: ParseState,
    l: Literal,
    previous_op: Option<Operator>,
) -> ParseResult<(Expression, ParseState)> {
    let peek = state.peek().cloned();
    match peek {
        Some(Token::Symbol(Symbol::AngOpen)) => {
            parse_collection_index(state, Expression::new_from_literal(&l))
        }
        Some(Token::Symbol(Symbol::Range)) => {
            parse_range(state.advance(), Expression::new_from_literal(&l), false)
        }
        Some(Token::Symbol(Symbol::RangeEq)) => {
            parse_range(state.advance(), Expression::new_from_literal(&l), true)
        }
        Some(Token::Symbol(Symbol::Op(m))) => parse_operator(
            state.advance(),
            Operator::from_op_symbol(&m),
            previous_op,
            Expression::new_from_literal(&l),
        ),
        _ => Ok((Expression::new_from_literal(&l), state)),
    }
}

fn parse_include(state: ParseState) -> ParseResult<(Expression, ParseState)> {
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
        None => Err(ParseError::NoMoreTokens {
            context: "on_include".to_string(),
        }),
    }
}

fn parse_keyword(state: ParseState, k: &Kwd) -> ParseResult<(Expression, ParseState)> {
    let (expr, state) = match k {
        Kwd::Return => parse_return(state)?,
        Kwd::While => parse_while(state)?,
        Kwd::For => parse_for(state)?,
        Kwd::If => parse_if(state)?,
        Kwd::Break => (
            Expression::Statement(Statement {
                expression: None,
                operation: StatementOperator::Break,
            }),
            state,
        ),
        Kwd::Include => parse_include(state)?,
        Kwd::Else => parse_else(state)?,
        Kwd::Const => parse_const(state)?,
        t => {
            return Err(ParseError::InvalidSyntax {
                message: "Invalid use of keyword, this should be used in for loop".to_string(),
                token_context: Token::Kwd(t.clone()),
            })
        }
    };

    dbg!("Parse keyword expr {}", &expr);

    Ok((expr, state))
}

fn parse_const(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    let (next, state) = state.next();
    let identifier = match next {
        Some(Token::Ident(ident)) => ident,
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                token_context: t,
            })
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_const".to_string(),
            })
        }
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
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_const".to_string(),
            })
        }
    }

    let (expr, state) = parse_expression(state, None, None)?;
    Ok((
        Expression::new_assignment(&identifier, expr, None, type_assertion, true),
        state,
    ))
}

fn parse_while(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    let peeked = state.peek();

    if is_new_body(peeked) {
        // handle infinite loops
        let state = state.advance();

        let (body, state) = parse_body(state, &mut vec![], None)?;
        return Ok((Expression::new_while(None, body), state));
    }

    let (lhs, state) = parse_single_value(state)?;

    let (evaluation, state) = parse_evaluation(state, lhs)?;

    let (next, state) = state.next();

    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => { /* Continue */ }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a brace open sign".to_string(),
                token_context: t,
            });
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_while".to_string(),
            });
        }
    }

    let (body, state) = parse_body(state, &mut vec![], None)?;
    let while_expr = Expression::new_while(Some(evaluation), body);
    Ok((while_expr, state))
}

fn parse_iterable(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    let (lhs, state) = parse_single_value(state)?;
    let peek = state.peek();
    if is_new_body(peek) {
        return Ok((lhs, state));
    };

    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::Range)) => parse_range(state, lhs, false),
        Some(Token::Symbol(Symbol::RangeEq)) => parse_range(state, lhs, true),

        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected Range or Range Equals operator".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens {
            context: "on_iterable".to_string(),
        }),
    }
}

fn parse_scoped_identifier(
    state: ParseState,
    previous: &mut Vec<String>,
) -> ParseResult<(ScopedVariable, ParseState)> {
    let (next, state) = state.next();
    let ident = match next {
        Some(Token::Ident(i)) => i,
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an identifier".to_string(),
                token_context: t,
            });
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_scoped_identifier".to_string(),
            })
        }
    };

    let (next, state) = state.next();

    match next {
        Some(Token::Kwd(Kwd::In)) => {
            if previous.is_empty() {
                Ok((ScopedVariable::new_single(&ident), state))
            } else {
                previous.push(ident);
                Ok((ScopedVariable::new_multiple(previous.to_vec()), state))
            }
        }
        Some(Token::Symbol(Symbol::Comma)) => {
            previous.push(ident);
            parse_scoped_identifier(state, previous)
        }
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected In keyword".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens {
            context: "on_scoped_identifier".to_string(),
        }),
    }
}

fn parse_for(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    let (scoped_variable, state) = parse_scoped_identifier(state, &mut vec![])?;

    let (iterable, state) = parse_iterable(state)?;
    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => { /* Continue */ }
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected a opening brace".to_string(),
                token_context: t,
            });
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_for".to_string(),
            })
        }
    }
    let (body, state) = parse_body(state, &mut vec![], None)?;

    Ok((
        Expression::For(For {
            scoped_variable,
            iterable: Box::new(iterable),
            body,
        }),
        state,
    ))
}

fn parse_else(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    let (next, state) = state.next();
    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            parse_body(state, &mut vec![], None).map(|(b, state)| (Expression::new_body(b), state))
        }
        Some(Token::Kwd(Kwd::If)) => parse_if(state),
        Some(t) => Err(ParseError::InvalidSyntax {
            message: "Expected a body or if else".to_string(),
            token_context: t,
        }),
        None => Err(ParseError::NoMoreTokens {
            context: "on_else".to_string(),
        }),
    }
}

fn parse_if(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    // let (t, state) = state.next();
    let (lhs, state) = parse_expression(state, None, None)?;
    // let (lhs, state) = match t {
    //     Some(Token::Symbol(Symbol::BraceOpen)) => parse_body(state, &mut vec![], None)
    //         .map(|(body, state)| (Expression::new_body(body), state))?,
    //     Some(Token::Ident(i)) => (Expression::new_identifier(&i), state),
    //     Some(Token::Literal(l)) => (Expression::new_from_literal(&l), state),
    //     Some(t) => {
    //         return Err(ParseError::InvalidSyntax {
    //             message: "Expected a symbol, ident or literal".to_string(),
    //             token_context: t.to_owned(),
    //         })
    //     }
    //     None => {
    //         return Err(ParseError::NoMoreTokens {
    //             context: "on_if".to_string(),
    //         })
    //     }
    // };

    let (evaluation, state) = parse_evaluation(state, lhs)?;
    let (next, state) = state.next();

    match next {
        Some(Token::Symbol(Symbol::BraceOpen)) | Some(Token::Symbol(Symbol::Newline)) => (),
        Some(t) => {
            return Err(ParseError::InvalidSyntax {
                message: "Expected an opening brace".to_string(),
                token_context: t,
            });
        }
        None => {
            return Err(ParseError::NoMoreTokens {
                context: "on_if".to_string(),
            })
        }
    }

    let (on_true_evaluation, state) = parse_body(state, &mut vec![], None)?;
    let (next, state) = state.next();
    let (on_false_evaluation, state) = if let Some(Token::Kwd(Kwd::Else)) = next {
        let (expr, state) = parse_else(state)?;
        (Some(expr), state)
    } else {
        (None, state)
    };
    Ok((
        Expression::new_if(
            evaluation,
            Expression::Body(on_true_evaluation),
            on_false_evaluation,
        ),
        state,
    ))
}

fn parse_return(state: ParseState) -> ParseResult<(Expression, ParseState)> {
    let (return_expression, state) = parse_expression(state, None, None)?;
    let statement = match state.peek() {
        Some(Token::Symbol(Symbol::Op(_))) => {
            parse_expression(state, Some(return_expression), None).map(|(expr, state)| {
                (
                    Expression::new_statement(Some(expr), StatementOperator::Return),
                    state,
                )
            })
        }
        _ => Ok((
            Expression::new_statement(Some(return_expression), StatementOperator::Return),
            state,
        )),
    };

    dbg!("Parse return statement{}", &statement);
    statement
}

fn is_new_body(t: Option<&Token>) -> bool {
    t == Some(&Token::Symbol(Symbol::BraceOpen))
}

#[cfg(test)]
mod tests {

    use core::num;
    use std::{collections::HashMap, vec};

    use super::*;
    use crate::dbg;

    use pretty_assertions::assert_eq;

    fn test(label: &str, input: Vec<Token>, expected: Expression) {
        dbg!("{}", label);
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
            "Break by itself",
            vec![Token::Kwd(Kwd::Break)],
            Expression::new_body(vec![Expression::new_statement(
                None,
                StatementOperator::Break,
            )]),
        );

        // for i in numbers  {
        //     break
        // }

        test(
            "Break in a for loop",
            vec![
                Token::Kwd(Kwd::For),
                Token::Ident("i".to_string()),
                Token::Kwd(Kwd::In),
                Token::Ident("numbers".to_string()),
                Token::Symbol(Symbol::BraceOpen),
                Token::Kwd(Kwd::Break),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_for(
                ScopedVariable::Single("i".to_string()),
                Expression::new_identifier("numbers"),
                vec![Expression::new_statement(None, StatementOperator::Break)],
            )]),
        );

        // for i in numbers  {
        //     if i < 0 {
        //         break
        //     } else {
        //         print "hi"
        //     }
        // }
        test(
            "Break in a for look with an if statement",
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
            Expression::new_body(vec![Expression::new_for(
                ScopedVariable::new_single("i"),
                Expression::new_identifier("numbers"),
                vec![Expression::new_if(
                    Expression::new_evaluation(
                        Expression::new_identifier("i"),
                        Some(Expression::new_int(0)),
                        EvaluationOperator::NumericOnly(EvaluationNumericOnly::Lz),
                    ),
                    Expression::new_body(vec![Expression::new_statement(
                        None,
                        StatementOperator::Break,
                    )]),
                    Some(Expression::new_body(vec![Expression::new_function_call(
                        "print",
                        vec![Expression::new_string("hi")],
                    )])),
                )],
            )]),
        );
    }

    #[test]
    fn if_expression() {
        // if true { }
        test(
            "if expression with empty body",
            vec![
                Token::Kwd(Kwd::If),
                Token::Literal(Literal::Bool(true)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_bool(true),
                    None,
                    EvaluationOperator::BooleanTruthy,
                ),
                Expression::Body(vec![
                    // empty body
                ]),
                None,
            )]),
        );
        test(
            "if statement with comparison and body content",
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
            Expression::new_body(vec![Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_int(3),
                    Some(Expression::new_int(3)),
                    EvaluationOperator::NumericAndString(EvaluationNumericAndString::Eq),
                ),
                Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
                None,
            )]),
        );
        // where x might be true or false
        // if x {
        //     print "Hello"
        // }
        test(
            "if expression with identifier comparison",
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
            Expression::new_body(vec![Expression::new_if(
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
            )]),
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
            "if else boolean statement",
            vec![
                Token::Kwd(Kwd::If),
                Token::Literal(Literal::Bool(true)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
                Token::Kwd(Kwd::Else),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_bool(true),
                    None,
                    EvaluationOperator::BooleanTruthy,
                ),
                Expression::Body(vec![
                    // empty body
                ]),
                Some(Expression::Body(vec![])),
            )]),
        );

        // if 3 == 3 {
        //     print "Hello"
        // } else {
        //     print "Goodbye"
        // }
        test(
            "if else with literal comparison",
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
            Expression::new_body(vec![Expression::new_if(
                Expression::new_evaluation(
                    Expression::new_int(3),
                    Some(Expression::new_int(3)),
                    EvaluationOperator::NumericAndString(EvaluationNumericAndString::Eq),
                ),
                Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )]),
                Some(Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Goodbye")],
                )])),
            )]),
        );
        /*
         * if x {
         *   print "Hello"
         * } else {
         *   print "Goodbye"
         * }
         * */
        test(
            "if else with identifier boolean comparison",
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
            Expression::new_body(vec![Expression::new_if(
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
            )]),
        )
    }

    #[test]
    fn while_loops() {
        // while {
        //
        // }
        test(
            "infinite while loop",
            vec![
                Token::Kwd(Kwd::While),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_while(None, vec![])]),
        );
        // With function call:
        // while {
        //    print "Hello"
        // }
        test(
            "infinite while loop with body",
            vec![
                Token::Kwd(Kwd::While),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_while(
                None,
                vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )],
            )]),
        );

        // With condition
        //
        // while 3 == 3 {
        //     print "Hello"
        // }
        test(
            "while loop with condition",
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
            Expression::new_body(vec![Expression::new_while(
                Some(Expression::new_evaluation(
                    Expression::new_int(3),
                    Some(Expression::new_int(3)),
                    EvaluationOperator::NumericAndString(EvaluationNumericAndString::Eq),
                )),
                vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )],
            )]),
        );

        // while x {
        //     print "Hello"
        // }
        test(
            "while loop with identifier condition",
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
            Expression::new_body(vec![Expression::new_while(
                Some(Expression::new_evaluation(
                    Expression::new_identifier("x"),
                    None,
                    EvaluationOperator::BooleanTruthy,
                )),
                vec![Expression::new_function_call(
                    "print",
                    vec![Expression::new_string("Hello")],
                )],
            )]),
        );

        // while y > 3 {
        //     y = y + 1
        // }
        test(
            "while loop with comparison",
            vec![
                Token::Kwd(Kwd::While),
                Token::new_ident("y"),
                Token::Symbol(Symbol::Evaluation(EvaluationSymbol::GzEq)),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::new_ident("y"),
                Token::Symbol(Symbol::Equals),
                Token::new_ident("y"),
                Token::Symbol(Symbol::Op(OpSymbol::Plus)),
                Token::Literal(Literal::Int(1)),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_while(
                Some(Expression::new_evaluation(
                    Expression::new_identifier("y"),
                    Some(Expression::new_int(3)),
                    EvaluationOperator::NumericOnly(EvaluationNumericOnly::GzEq),
                )),
                vec![Expression::new_assignment(
                    "y",
                    Expression::new_math_expression(
                        Expression::new_identifier("y"),
                        Expression::new_int(1),
                        Operator::Add,
                    ),
                    None,
                    None,
                    false,
                )],
            )]),
        )
    }

    #[test]
    fn for_loops() {
        // for x in 0..5 {
        //
        // }
        test(
            "For loop with range exclusive",
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
            Expression::new_body(vec![Expression::new_for(
                ScopedVariable::new_single("x"),
                Expression::new_range(Expression::new_int(0), Expression::new_int(5), false),
                Body::new(),
            )]),
        );
        // for x in y..z {
        //
        // }
        test(
            "For loop with identifiers",
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
            Expression::new_body(vec![Expression::new_for(
                ScopedVariable::new_single("x"),
                Expression::new_range(
                    Expression::new_identifier("y"),
                    Expression::new_identifier("z"),
                    false,
                ),
                Body::new(),
            )]),
        );
    }

    #[test]
    fn variable_assignment_with_another_variable() {
        // y = x
        test(
            "variable assignment with another variable",
            vec![
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "y",
                Expression::new_identifier("x"),
                None,
                None,
                false,
            )]),
        )
    }

    #[test]
    fn variable_usage_in_expressions() {
        // y = x + 3
        test(
            "varible usage in an expression",
            vec![
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Op(OpSymbol::Plus)),
                Token::Literal(Literal::Int(3)),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "y",
                Expression::MathOperation(Operations {
                    lhs: Box::new(Expression::new_identifier("x")),
                    rhs: Box::new(Expression::new_int(3)),
                    operation: Operator::Add,
                }),
                None,
                None,
                false,
            )]),
        );

        // y int = x + 3
        test(
            "Variable usage with data type kwds in expression",
            vec![
                Token::Ident("y".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Op(OpSymbol::Plus)),
                Token::Literal(Literal::Int(3)),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "y",
                Expression::MathOperation(Operations {
                    lhs: Box::new(Expression::new_identifier("x")),
                    rhs: Box::new(Expression::new_int(3)),
                    operation: Operator::Add,
                }),
                None,
                Some(NativeType::Int),
                false,
            )]),
        );
    }

    #[test]
    fn function_declaration_no_params_no_return() {
        // do_nothing () {\n
        // \n
        // }\n
        test(
            "Function declaration with empty body",
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
            Expression::new_body(vec![Expression::new_function(
                "do_nothing",
                vec![],
                None,
                Expression::Body(vec![]),
            )]),
        );
    }

    #[test]
    fn function_declaration_no_params_return() {
        // do_nothing () -> int {\n
        // \n
        // }\n
        test(
            "Function declaration empty body with return type",
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
            Expression::new_body(vec![Expression::new_function(
                "do_nothing",
                vec![],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            )]),
        );
        // with body
    }

    #[test]
    fn function_declaration_params_no_return() {
        // do_nothing (numOne, numTwo) {\n
        // \n
        // }\n
        test(
            "Function declaration empty body with parameters",
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
            Expression::new_body(vec![Expression::new_function(
                "do_nothing",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                None,
                Expression::Body(vec![]),
            )]),
        );
        // typed arguments
        test(
            "Function declaration empty body with parameters typed",
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
            Expression::new_body(vec![Expression::new_function(
                "do_nothing",
                vec![
                    Parameter::new("numOne", Some(NativeType::Int)),
                    Parameter::new("numTwo", Some(NativeType::Int)),
                ],
                None,
                Expression::Body(vec![]),
            )]),
        )
    }

    #[test]
    fn function_declaration_params_body() {
        test(
            "function_declaration_params_body",
            vec![
                Token::Ident("say_hi".to_string()),
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("name".to_string()),
                Token::Symbol(Symbol::ParenClose),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hi".to_string())),
                Token::Ident("name".to_string()),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_function(
                "say_hi",
                vec![Parameter::new("name", None)],
                None,
                Expression::new_body(vec![Expression::new_function_call(
                    "print",
                    vec![
                        Expression::new_string("Hi"),
                        Expression::new_identifier("name"),
                    ],
                )]),
            )]),
        )
    }
    #[test]
    fn function_declaration_params_return() {
        // do_nothing (numOne, numTwo) -> int {\n
        // \n
        // }\n
        test(
            "Function declaration parameters with return type",
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
            Expression::new_body(vec![Expression::new_function(
                "do_nothing",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            )]),
        );
        // Typed Parameters
        test(
            "Function declaration parameters with return type and parameter types",
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
            Expression::new_body(vec![Expression::new_function(
                "do_nothing",
                vec![
                    Parameter::new("numOne", Some(NativeType::Int)),
                    Parameter::new("numTwo", Some(NativeType::Int)),
                ],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            )]),
        )
    }

    #[test]
    fn function_call_no_values() {
        // print <- produces identifier. Cannot make assumption about what this is
        test(
            "Function call with no values produces identifier",
            vec![Token::Ident("print".to_string())],
            Expression::new_body(vec![Expression::Identifier("print".to_string())]),
        );
    }

    #[test]
    fn floating_point_nums_in_assignment() {
        // x=3.5\n
        test(
            "Floating point numbers in assignment",
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Float(3.5)),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_float(3.5),
                None,
                None,
                false,
            )]),
        );
    }

    #[test]
    fn function_call_multiple_arguments() {
        // print "Hello" "World"
        test(
            "Function call with multiple arguments",
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Literal(Literal::String("World".to_string())),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::new_body(vec![Expression::new_function_call(
                "print",
                vec![
                    Expression::new_string("Hello"),
                    Expression::new_string("World"),
                ],
            )]),
        );
    }

    #[test]
    fn function_call_multiple_arguments_with_identifier() {
        // print "Hello" x
        test(
            "Function call multiple arguments using identifier",
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::new_body(vec![Expression::FunctionCall(FunctionCall {
                identifier: "print".to_string(),
                args: vec![
                    Expression::new_string("Hello"),
                    Expression::new_identifier("x"),
                ],
            })]),
        );
    }

    #[test]
    fn function_call_multiple_values_with_body() {
        // print {"Hello"} x
        test(
            "Function call multiple arguments with body",
            vec![
                Token::Ident("print".to_string()),
                Token::Symbol(Symbol::BraceOpen),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Symbol(Symbol::BraceClose),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::new_body(vec![Expression::FunctionCall(FunctionCall {
                identifier: "print".to_string(),
                args: vec![
                    Expression::new_body(vec![Expression::new_string("Hello")]),
                    Expression::new_identifier("x"),
                ],
            })]),
        );
    }

    #[test]
    fn function_call_multiple_values_with_literal() {
        // print "Hello" true
        test(
            "Function call multiple arguments with literals",
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Literal(Literal::Bool(true)),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::new_body(vec![Expression::new_function_call(
                "print",
                vec![Expression::new_string("Hello"), Expression::new_bool(true)],
            )]),
        );
    }

    #[test]
    fn literal_assignment() {
        // x=5
        let expected = Expression::new_body(vec![Expression::new_assignment(
            "x",
            Expression::new_int(5),
            None,
            None,
            false,
        )]);
        test(
            "Literal assignment",
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected.clone(),
        );

        // Whitespace handling
        // x = 5
        // NOTE: shouldn't matter
        // since we filter it anyway
        test(
            "Literal assigment with whitespace explicitly added",
            vec![
                Token::Ident("x".to_string()),
                Token::whitespace(),
                Token::Symbol(Symbol::Equals),
                Token::whitespace(),
                Token::Literal(Literal::Int(5)),
            ],
            expected.clone(),
        );

        // literal assignment with types
        let expected = Expression::new_body(vec![Expression::new_assignment(
            "x",
            Expression::new_int(5),
            None,
            Some(NativeType::Int),
            false,
        )]);
        test(
            "Literal assigment with int type kwd",
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected,
        );

        // const x = 5
        test(
            "Literal assignment with const keyword",
            vec![
                Token::Kwd(Kwd::Const),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_int(5),
                None,
                None,
                true,
            )]),
        );

        // const x Int = 5
        test(
            "Literal assignment with const and data type kwd",
            vec![
                Token::Kwd(Kwd::Const),
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_int(5),
                None,
                Some(NativeType::Int),
                true,
            )]),
        );
    }
    #[test]
    fn include_kwd() {
        // include "my_package"
        test(
            "Include kwd",
            vec![
                Token::Kwd(Kwd::Include),
                Token::Literal(Literal::String("my_package".to_string())),
            ],
            Expression::new_body(vec![Expression::new_statement(
                Some(Expression::new_string("my_package")),
                StatementOperator::Include,
            )]),
        )
    }

    #[test]
    fn array_literals() {
        test(
            "Array literals empty",
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Arr)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::AngOpen),
                Token::Symbol(Symbol::AngClose),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_array(vec![]),
                None,
                Some(NativeType::Array),
                false,
            )]),
        );

        // x = [3]
        test(
            "Array literals with single value",
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Arr)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::AngOpen),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::AngClose),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_array(vec![Expression::new_from_literal(&Literal::Int(3))]),
                None,
                Some(NativeType::Array),
                false,
            )]),
        );

        // x = [3, 4]
        test(
            "Array literals with multiple values initialized",
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
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_array(vec![
                    Expression::new_from_literal(&Literal::Int(3)),
                    Expression::new_from_literal(&Literal::Int(4)),
                ]),
                None,
                Some(NativeType::Array),
                false,
            )]),
        );

        // x = [3, 4,]
        test(
            "Array literals with multiple values initialized, leading comma",
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
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_array(vec![
                    Expression::new_from_literal(&Literal::Int(3)),
                    Expression::new_from_literal(&Literal::Int(4)),
                ]),
                None,
                Some(NativeType::Array),
                false,
            )]),
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
        // x dict = {"hello": "world"}
        test(
            "Assigment empty dict with type",
            vec![
                Token::Ident("x".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Dict)),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Colon),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_dict(HashMap::new()),
                None,
                Some(NativeType::Dictionary),
                false,
            )]),
        );

        // x dict = {
        //  "hello": "world"
        // }
        test(
            "Assigment populated dict with type",
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
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_dict(HashMap::from([(
                    DictionaryKey::String("hello".to_string()),
                    Expression::new_string("world"),
                )])),
                None,
                Some(NativeType::Dictionary),
                false,
            )]),
        );
        // x = {"hello": "world",,}
        dbg!("Error");
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

    #[test]
    fn fib() {
        test(
            "fibonacci",
            vec![
                Token::new_ident("fib"),
                Token::Symbol(Symbol::ParenOpen),
                Token::new_ident("num"),
                Token::Symbol(Symbol::ParenClose),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // tab
                //
                Token::Kwd(Kwd::If),
                Token::new_ident("num"),
                Token::Symbol(Symbol::Evaluation(EvaluationSymbol::LzEq)),
                Token::Literal(Literal::Int(1)),
                // body
                // if
                Token::Symbol(Symbol::BraceOpen),
                Token::Kwd(Kwd::Return),
                Token::Ident("num".to_string()),
                Token::Symbol(Symbol::BraceClose),
                //
                Token::Kwd(Kwd::Else),
                // body
                // else
                Token::Symbol(Symbol::BraceOpen),
                Token::Kwd(Kwd::Return),
                Token::Symbol(Symbol::BraceOpen),
                Token::new_ident("fib"),
                Token::Symbol(Symbol::BraceOpen),
                Token::new_ident("num"),
                Token::Symbol(Symbol::Op(OpSymbol::Minus)),
                Token::Literal(Literal::Int(1)),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Op(OpSymbol::Plus)),
                Token::Symbol(Symbol::BraceOpen),
                Token::new_ident("fib"),
                Token::Symbol(Symbol::BraceOpen),
                Token::new_ident("num"),
                Token::Symbol(Symbol::Op(OpSymbol::Minus)),
                Token::Literal(Literal::Int(2)),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_function(
                "fib",
                vec![Parameter::new("num", None)],
                None,
                Expression::new_body(vec![Expression::new_if(
                    Expression::new_evaluation(
                        Expression::new_identifier("num"),
                        Some(Expression::new_int(1)),
                        EvaluationOperator::NumericOnly(EvaluationNumericOnly::LzEq),
                    ),
                    Expression::new_body(vec![Expression::Statement(Statement::new(
                        Some(Expression::new_identifier("num")),
                        StatementOperator::Return,
                    ))]),
                    // on false evaluation
                    Some(Expression::new_body(vec![Expression::new_statement(
                        Some(Expression::new_math_expression(
                            Expression::new_body(vec![Expression::new_function_call(
                                "fib",
                                vec![Expression::new_body(vec![Expression::new_math_expression(
                                    Expression::new_identifier("num"),
                                    Expression::new_int(1),
                                    Operator::Subtract,
                                )])],
                            )]),
                            Expression::new_body(vec![Expression::new_function_call(
                                "fib",
                                vec![Expression::new_body(vec![Expression::new_math_expression(
                                    Expression::new_identifier("num"),
                                    Expression::new_int(2),
                                    Operator::Subtract,
                                )])],
                            )]),
                            Operator::Add,
                        )),
                        StatementOperator::Return,
                    )])),
                )]),
            )]),
        );

        test(
            "fibonacci with stored variable",
            vec![
                Token::new_ident("fib"),
                Token::Symbol(Symbol::ParenOpen),
                Token::new_ident("num"),
                Token::Symbol(Symbol::ParenClose),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // if condition
                Token::Kwd(Kwd::If),
                Token::new_ident("num"),
                Token::Symbol(Symbol::Evaluation(EvaluationSymbol::LzEq)),
                Token::Literal(Literal::Int(1)),
                // if body
                Token::Symbol(Symbol::BraceOpen),
                Token::Kwd(Kwd::Return),
                Token::Ident("num".to_string()),
                Token::Symbol(Symbol::BraceClose),
                //
                Token::Kwd(Kwd::Else),
                // else body
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                Token::new_ident("new_num"),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::BraceOpen),
                Token::new_ident("fib"),
                Token::Symbol(Symbol::BraceOpen),
                Token::new_ident("num"),
                Token::Symbol(Symbol::Op(OpSymbol::Minus)),
                Token::Literal(Literal::Int(1)),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Op(OpSymbol::Plus)),
                Token::Symbol(Symbol::BraceOpen),
                Token::new_ident("fib"),
                Token::Symbol(Symbol::BraceOpen),
                Token::new_ident("num"),
                Token::Symbol(Symbol::Op(OpSymbol::Minus)),
                Token::Literal(Literal::Int(2)),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
                Token::Kwd(Kwd::Return),
                Token::new_ident("new_num"),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::BraceClose),
            ],
            Expression::new_body(vec![Expression::new_function(
                "fib",
                vec![Parameter::new("num", None)],
                None,
                Expression::new_body(vec![Expression::new_if(
                    Expression::new_evaluation(
                        Expression::new_identifier("num"),
                        Some(Expression::new_int(1)),
                        EvaluationOperator::NumericOnly(EvaluationNumericOnly::LzEq),
                    ),
                    Expression::new_body(vec![Expression::Statement(Statement::new(
                        Some(Expression::new_identifier("num")),
                        StatementOperator::Return,
                    ))]),
                    // on false evaluation
                    Some(Expression::new_body(vec![
                        Expression::new_assignment(
                            "new_num",
                            Expression::new_math_expression(
                                Expression::new_body(vec![Expression::new_function_call(
                                    "fib",
                                    vec![Expression::new_body(vec![
                                        Expression::new_math_expression(
                                            Expression::new_identifier("num"),
                                            Expression::new_int(1),
                                            Operator::Subtract,
                                        ),
                                    ])],
                                )]),
                                Expression::new_body(vec![Expression::new_function_call(
                                    "fib",
                                    vec![Expression::new_body(vec![
                                        Expression::new_math_expression(
                                            Expression::new_identifier("num"),
                                            Expression::new_int(2),
                                            Operator::Subtract,
                                        ),
                                    ])],
                                )]),
                                Operator::Add,
                            ),
                            None,
                            None,
                            false,
                        ),
                        Expression::new_statement(
                            Some(Expression::new_identifier("new_num")),
                            StatementOperator::Return,
                        ),
                    ])),
                )]),
            )]),
        );
    }

    #[test]
    fn operator_precedence() {
        test(
            "Operator precendence 3 * 4 + 5",
            vec![
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Op(OpSymbol::Asterisk)),
                Token::Literal(Literal::Int(4)),
                Token::Symbol(Symbol::Op(OpSymbol::Plus)),
                Token::Literal(Literal::Int(5)),
            ],
            Expression::new_body(vec![Expression::new_math_expression(
                Expression::new_math_expression(
                    Expression::new_int(3),
                    Expression::new_int(4),
                    Operator::Multiply,
                ),
                Expression::new_int(5),
                Operator::Add,
            )]),
        );

        // x = 3 * 4 + 5
        test(
            "operator_precedence with assignment",
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Op(OpSymbol::Asterisk)),
                Token::Literal(Literal::Int(4)),
                Token::Symbol(Symbol::Op(OpSymbol::Plus)),
                Token::Literal(Literal::Int(5)),
            ],
            Expression::new_body(vec![Expression::new_assignment(
                "x",
                Expression::new_math_expression(
                    Expression::new_math_expression(
                        Expression::new_int(3),
                        Expression::new_int(4),
                        Operator::Multiply,
                    ),
                    Expression::new_int(5),
                    Operator::Add,
                ),
                None,
                None,
                false,
            )]),
        );
    }
}
