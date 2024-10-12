use crate::language::*;
use crate::lexer::*;
use crate::parser::*;

pub fn advance_past_multiple<'a, I>(tokens: &mut I, test: &[Token]) -> Option<&'a Token>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    tokens.find(|t| {
        for t_token in test {
            if *t != t_token {
                return true;
            }
        }
        false
    })
}

pub fn advance_past<'a, I>(tokens: &mut I, test: Token) -> Option<&'a Token>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    tokens.find(|t| **t != test)
}

pub fn advance_past_whitespace<'a, I>(tokens: &mut I) -> Option<&'a Token>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    advance_past(tokens, Token::Symbol(Symbol::Whitespace))
}

impl Literal {
    pub fn to_vl(&self) -> SingleValue {
        match self.clone() {
            Literal::BoolLiteral(b) => SingleValue::new_bool(&b.to_string()),
            Literal::IntLiteral(i) => SingleValue::new_int(&i.to_string()),
        }
    }
}

pub fn parse_string<'a, I>(tokens: &mut I) -> SingleValue
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    SingleValue::new_string(
        &tokens
            .take_while(|t: &&Token| **t != Token::Symbol(Symbol::Quote))
            .fold(String::new(), |acc, e| acc + &e.to_string()),
    )
}

pub fn parse_arguments<'a, I>(tokens: &mut I, t: &Token) -> Option<FunctionArguments>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let mut args: FunctionArguments = vec![];
    match t {
        Token::Literal(l) => args.push(Expression::SingleValue(l.to_vl())),
        Token::Symbol(Symbol::Quote) => args.push(Expression::SingleValue(parse_string(tokens))),
        _ => unreachable!(),
    }

    // Parse remaining arguments
    while let Some(t) = advance_past_whitespace(tokens) {
        let arg: Option<SingleValue> = match t {
            Token::Symbol(Symbol::Quote) => Some(parse_string(tokens)),
            Token::Ident(i) => Some(SingleValue::Identifier(i.to_string())),

            //  literal arg
            // TODO: Review this(Perhaps it's wrong?):
            // We purposefully won't allow 3 + 2
            // As the user may want to pass 3 as an arg, + as an arg and 2  as an arg
            Token::Literal(l) => Some(l.to_vl()),

            Token::Symbol(Symbol::BraceOpen) => {
                //  expression :D
                todo!()
            }
            _ => {
                // Invalid syntax in this case
                unreachable!()
            }
        };
        if let Some(a) = arg {
            args.push(Expression::SingleValue(a))
        } else {
            // TODO: Invalid syntax
        }
    }
    Some(args)
}
