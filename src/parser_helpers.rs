use crate::language::*;
use crate::lexer::*;

impl Literal {
    pub fn to_vl(&self) -> SingleValue {
        match self.clone() {
            Literal::Bool(b) => SingleValue::new_bool(&b.to_string()),
            Literal::Int(i) => SingleValue::new_int(&i.to_string()),
            Literal::String(s) => SingleValue::new_string(&s.to_string()),
        }
    }
}

pub fn parse_string<'a, I>(tokens: &mut I) -> SingleValue
where
    I:  std::iter::Iterator<Item = &'a Token>,
{
    SingleValue::new_string(
        &tokens
            .take_while(|t: &&Token| **t != Token::Symbol(Symbol::Quote))
            .fold(String::new(), |acc, e| acc + &e.to_string()),
    )
}

pub fn parse_arguments<'a, I>(tokens: &mut I, t: &Token) -> Option<FunctionArguments>
where
    I:  std::iter::Iterator<Item = &'a Token>,
{
    let mut args: FunctionArguments = vec![];
    match t {
        Token::Literal(l) => args.push(Expression::SingleValue(l.to_vl())),
        Token::Symbol(Symbol::Quote) => args.push(Expression::SingleValue(parse_string(tokens))),
        _ => unreachable!(),
    }

    // Parse remaining arguments
    while let Some(t) = tokens.next() {
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
