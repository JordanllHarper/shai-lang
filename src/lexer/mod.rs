pub mod token;

use std::collections::HashSet;
use token::*;
/// Lexer component for transforming raw string slice into a [Token] stream.
/// Implements [Iterator], and will provide a token on each `.next()`.
///
/// Usage:
///
/// ```
/// let token_iterator = Lexer::new(input)
/// // Get a Vec of Tokens.
/// let tokens = token_iterator.collect::<Vec<Token>>();
///
/// ```
#[derive(Clone, Debug)]
pub struct Lexer {
    position: usize,
    input: Vec<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let allocated_chars = input.chars().collect::<Vec<char>>();
        Self {
            position: 0,
            input: allocated_chars,
        }
    }
    /// Advances the lexer a single position, returning an optional char in the process.
    fn advance(&mut self) -> Option<char> {
        self.position += 1;
        self.input.get(self.position).copied()
    }

    /// Steps the lexer back one step
    fn step_back(&mut self) {
        if self.position == 0 {
            return;
        }
        self.position -= 1;
    }
}

fn peek_if<M>(lexer: &mut Lexer, test: char, until: char, mapping: M) -> Option<Token>
where
    M: Fn(&[char]) -> Token,
{
    let next = lexer.advance()?;
    if next == test {
        let mut c_collection = Vec::new();

        while let Some(c) = lexer.advance() {
            if c == until {
                break;
            }
            c_collection.push(c);
        }
        Some(mapping(&c_collection))
    } else {
        lexer.step_back();
        None
    }
}

fn take_until<M>(lexer: &mut Lexer, test: char, until: &str, mapping: M) -> Option<Token>
where
    M: Fn(&[char]) -> Token,
{
    let next = lexer.advance()?;

    let until_count = until.len();
    let until = until.chars().collect::<Vec<char>>();

    if next == test {
        let mut c_collection = Vec::new();
        let mut until_collection = Vec::new();

        while let Some(c) = lexer.advance() {
            until_collection.push(c);

            if until == until_collection {
                c_collection.pop();
                // match
                break;
            } else {
                c_collection.push(c);
            }

            if until_collection.len() >= until_count {
                until_collection.remove(0);
            }
        }

        Some(mapping(&c_collection))
    } else {
        lexer.step_back();
        None
    }
}

fn peek_while<M>(lexer: &mut Lexer, test: char, mapping: M) -> Token
where
    M: Fn(&[char]) -> Token,
{
    let mut c_collection = Vec::new();

    while let Some(t) = lexer.advance() {
        if t == test {
            break;
        }
        c_collection.push(t);
    }
    mapping(&c_collection)
}

fn peek_symbol(lexer: &mut Lexer, test: char, matched: Symbol, not_matched: Symbol) -> Token {
    let next = lexer.advance();
    if let Some(c) = next {
        if c == test {
            Token::Symbol(matched)
        } else {
            lexer.step_back();
            Token::Symbol(not_matched)
        }
    } else {
        Token::Symbol(not_matched)
    }
}
fn maybe_peek(lexer: &mut Lexer, test: char, matched: Symbol) -> Option<Token> {
    let next = lexer.advance();
    if let Some(c) = next {
        if c == test {
            Some(Token::Symbol(matched))
        } else {
            lexer.step_back();
            None
        }
    } else {
        None
    }
}

fn peek_on_match<F>(lexer: &mut Lexer, test: char, on_match: F, or_else: Token) -> Token
where
    F: Fn(&mut Lexer) -> Token,
{
    let clone = or_else.clone();
    lexer.advance().map_or_else(
        || clone,
        |c| {
            if c == test {
                on_match(lexer)
            } else {
                or_else
            }
        },
    )
}

fn parse_literal_or_identifier(s: &str) -> Token {
    if let Ok(r) = str::parse::<i32>(s) {
        Token::Literal(Literal::Int(r))
    } else if let Ok(r) = str::parse::<f64>(s) {
        Token::Literal(Literal::Float(r))
    } else {
        Token::Ident(s.to_string())
    }
}

fn is_symbol_applicable(symbol: &char) -> bool {
    let allowed_symbols = HashSet::from(['.', '_']);
    allowed_symbols.contains(symbol)
}

/// Advances the lexer.
///
/// If the next character matches test, step back and return true.
/// If the next character does not match test, step back and return false.
/// If there are no more tokens, returns None.
fn peek_next(lexer: &mut Lexer, test: char) -> Option<bool> {
    let result = lexer.advance().map(|c| c == test);
    lexer.step_back();
    result
}

/// Advances a lexer until a non-alphanumeric delimeter and converts into an optional [Token].
/// Returns Some if a text symbol can be found and None otherwise.
/// e.g. whitespace, which should never happen unless there is a "  " double space.
fn next_non_symbol(lexer: &mut Lexer) -> Option<Token> {
    let current = lexer.input.get(lexer.position)?;
    let mut buf = current.to_string();

    // cover it being the end of the stream as well as delimiter
    while let Some(ch) = lexer.advance() {
        // allow designated symbols through
        if ch.is_alphanumeric() || is_symbol_applicable(&ch) {
            match ch {
                '.' => {
                    let result = peek_next(lexer, '.');
                    if let Some(b) = result {
                        if b {
                            break;
                        } else {
                            buf.push(ch);
                        }
                    }
                }
                _ => buf.push(ch),
            }
        } else {
            break;
        }
    }
    let buf = buf.trim();
    let token = match buf {
        "for" => Token::Kwd(Kwd::For),
        "while" => Token::Kwd(Kwd::While),
        "if" => Token::Kwd(Kwd::If),
        "else" => Token::Kwd(Kwd::Else),
        "return" => Token::Kwd(Kwd::Return),
        "in" => Token::Kwd(Kwd::In),
        "break" => Token::Kwd(Kwd::Break),
        "include" => Token::Kwd(Kwd::Include),
        "const" => Token::Kwd(Kwd::Const),
        "true" => Token::Literal(Literal::Bool(true)),
        "false" => Token::Literal(Literal::Bool(false)),
        "void" => Token::Kwd(Kwd::DataType(DataTypeKwd::Void)),
        s => {
            if let Some(s) = map_datatype_kwd(s) {
                Token::Kwd(Kwd::DataType(s))
            } else {
                parse_literal_or_identifier(s)
            }
        }
    };
    lexer.step_back();
    Some(token)
}

fn map_datatype_kwd(s: &str) -> Option<DataTypeKwd> {
    match s {
        "bool" => Some(DataTypeKwd::Bool),
        "int" => Some(DataTypeKwd::Int),
        "float" => Some(DataTypeKwd::Float),
        "string" => Some(DataTypeKwd::String),
        "dict" => Some(DataTypeKwd::Dict),
        "arr" => Some(DataTypeKwd::Arr),
        _ => None,
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let input = &self.input;
        let position = self.position;
        let current = input.get(position);
        if let Some(c) = current {
            let token = match c {
                '(' => Token::Symbol(Symbol::ParenOpen),
                ')' => Token::Symbol(Symbol::ParenClose),
                '{' => Token::Symbol(Symbol::BraceOpen),
                '}' => Token::Symbol(Symbol::BraceClose),
                '[' => Token::Symbol(Symbol::AngOpen),
                ']' => Token::Symbol(Symbol::AngClose),
                '%' => Token::Symbol(Symbol::Op(OpSymbol::Modulus)),
                '\'' => Token::Symbol(Symbol::Apstr),
                ',' => Token::Symbol(Symbol::Comma),
                '\\' => {
                    if let Some(esc_quote) = maybe_peek(self, '"', Symbol::EscapeQuote) {
                        esc_quote
                    } else if let Some(esc_apos) = maybe_peek(self, '\'', Symbol::EscapeApos) {
                        esc_apos
                    } else {
                        Token::Symbol(Symbol::BckSlash)
                    }
                }
                '$' => Token::Symbol(Symbol::Dollar),
                ':' => Token::Symbol(Symbol::Colon),
                '_' => Token::Symbol(Symbol::Underscore),
                '\n' => Token::Symbol(Symbol::Newline),
                ' ' => Token::Symbol(Symbol::Whitespace),
                '"' => peek_while(self, '"', |characters| {
                    Token::Literal(Literal::String(String::from_iter(characters.iter())))
                }),
                '.' => peek_on_match(
                    self,
                    '.',
                    |lexer| peek_symbol(lexer, '=', Symbol::RangeEq, Symbol::Range),
                    Token::Symbol(Symbol::Period),
                ),
                '<' => peek_symbol(
                    self,
                    '=',
                    Symbol::Evaluation(EvaluationSymbol::LzEq),
                    Symbol::Evaluation(EvaluationSymbol::Lz),
                ),
                '>' => peek_symbol(
                    self,
                    '=',
                    Symbol::Evaluation(EvaluationSymbol::GzEq),
                    Symbol::Evaluation(EvaluationSymbol::Gz),
                ),
                '+' => peek_symbol(self, '=', Symbol::PlusAssign, Symbol::Op(OpSymbol::Plus)),
                '-' => {
                    let final_symbol =
                        peek_symbol(self, '=', Symbol::MinusAssign, Symbol::Op(OpSymbol::Minus));
                    if final_symbol != Token::Symbol(Symbol::MinusAssign) {
                        peek_symbol(self, '>', Symbol::Arrow, Symbol::Op(OpSymbol::Minus))
                    } else {
                        final_symbol
                    }
                }

                '*' => peek_symbol(
                    self,
                    '=',
                    Symbol::MultiplyAssign,
                    Symbol::Op(OpSymbol::Asterisk),
                ),
                '/' => {
                    if let Some(c) = peek_if(self, '/', '\n', |characters| {
                        Token::Symbol(Symbol::Comment(String::from_iter(characters.iter())))
                    }) {
                        c
                    } else if let Some(t) = take_until(self, '*', "*/", |characters| {
                        Token::Symbol(Symbol::MultilineComment(String::from_iter(
                            characters.iter(),
                        )))
                    }) {
                        t
                    } else {
                        peek_symbol(
                            self,
                            '=',
                            Symbol::DivideAssign,
                            Symbol::Op(OpSymbol::FwdSlash),
                        )
                    }
                }
                '=' => peek_symbol(
                    self,
                    '=',
                    Symbol::Evaluation(EvaluationSymbol::Equality),
                    Symbol::Equals,
                ),
                '!' => peek_symbol(
                    self,
                    '=',
                    Symbol::Evaluation(EvaluationSymbol::NotEquality),
                    Symbol::Bang,
                ),
                '&' => peek_symbol(self, '&', Symbol::And, Symbol::Ampsnd),
                '|' => peek_symbol(self, '|', Symbol::Or, Symbol::Pipe),
                _ => next_non_symbol(self)?,
            };
            self.advance();
            Some(token)
        } else {
            None
        }
    }
}

//  ---- TESTS ----

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::fmt::Debug;

    fn test<T>(input: &str, expected: T)
    where
        T: FromIterator<Token> + Debug + PartialEq,
    {
        let actual = Lexer::new(input).collect::<T>();
        assert_eq!(expected, actual);
    }

    fn test_char<T>(input: T, expected: Token)
    where
        T: ToString,
    {
        let mut lexer = Lexer::new(&input.to_string());
        let actual = lexer.next().expect("There should be a character.");
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_number_parsed() {
        test(
            "\"3\"",
            vec![Token::Literal(Literal::String("3".to_string()))],
        )
    }

    #[test]
    fn single_line_comment() {
        test(
            "// hello",
            vec![Token::Symbol(Symbol::Comment(" hello".to_string()))],
        )
    }

    #[test]
    fn multi_line_comment() {
        test(
            r"/*
hello
*/",
            vec![Token::Symbol(Symbol::MultilineComment(
                r"
hello
"
                .to_string(),
            ))],
        )
    }

    #[test]
    fn complex_variable_assignment_with_newline() {
        test(
            "x = if true { return 5 } else { return 4 }\n",
            vec![
                Token::Ident("x".to_string()),
                Token::whitespace(),
                Token::Symbol(Symbol::Equals),
                Token::whitespace(),
                Token::Kwd(Kwd::If),
                Token::whitespace(),
                Token::Literal(Literal::Bool(true)),
                Token::whitespace(),
                Token::Symbol(Symbol::BraceOpen),
                Token::whitespace(),
                Token::Kwd(Kwd::Return),
                Token::whitespace(),
                Token::Literal(Literal::Int(5)),
                Token::whitespace(),
                Token::Symbol(Symbol::BraceClose),
                Token::whitespace(),
                Token::Kwd(Kwd::Else),
                Token::whitespace(),
                Token::Symbol(Symbol::BraceOpen),
                Token::whitespace(),
                Token::Kwd(Kwd::Return),
                Token::whitespace(),
                Token::Literal(Literal::Int(4)),
                Token::whitespace(),
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
        );
    }

    #[test]
    fn read_kwd_data_type() {
        test("bool", vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Bool))]);
        test("float", vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Float))]);
        test("int", vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Int))]);
        test(
            "string",
            vec![Token::Kwd(Kwd::DataType(DataTypeKwd::String))],
        );
    }

    #[test]
    fn read_kwd_non_data_type() {
        test("while", vec![Token::Kwd(Kwd::While)]);
        test("for", vec![Token::Kwd(Kwd::For)]);
        test("if", vec![Token::Kwd(Kwd::If)]);
        test("return", vec![Token::Kwd(Kwd::Return)]);
        test("in", vec![Token::Kwd(Kwd::In)]);
        test("break", vec![Token::Kwd(Kwd::Break)]);
        test("include", vec![Token::Kwd(Kwd::Include)]);
        test("else", vec![Token::Kwd(Kwd::Else)]);
        test("const", vec![Token::Kwd(Kwd::Const)]);
    }

    #[test]
    fn read_literals() {
        test("true", vec![Token::Literal(Literal::Bool(true))]);
        test("false", vec![Token::Literal(Literal::Bool(false))]);
        test("3", vec![Token::Literal(Literal::Int(3))]);
        test("3.13", vec![Token::Literal(Literal::Float(3.13))]);
    }

    #[test]
    fn read_1_char_symbols() {
        test_char('(', Token::Symbol(Symbol::ParenOpen));
        test_char(')', Token::Symbol(Symbol::ParenClose));
        test_char('[', Token::Symbol(Symbol::AngOpen));
        test_char(']', Token::Symbol(Symbol::AngClose));
        test_char('{', Token::Symbol(Symbol::BraceOpen));
        test_char('}', Token::Symbol(Symbol::BraceClose));
        test_char('%', Token::Symbol(Symbol::Op(OpSymbol::Modulus)));
        // test_char('\"', Token::Symbol(Symbol::Quote));
        test_char('\'', Token::Symbol(Symbol::Apstr));
        test_char(',', Token::Symbol(Symbol::Comma));
        test_char('.', Token::Symbol(Symbol::Period));
        test_char('\\', Token::Symbol(Symbol::BckSlash));
        test_char('$', Token::Symbol(Symbol::Dollar));
        test_char(':', Token::Symbol(Symbol::Colon));
        test_char('_', Token::Symbol(Symbol::Underscore));
        test_char('\n', Token::Symbol(Symbol::Newline));
        test_char(' ', Token::Symbol(Symbol::Whitespace));
        test_char('<', Token::Symbol(Symbol::Evaluation(EvaluationSymbol::Lz)));
        test_char('>', Token::Symbol(Symbol::Evaluation(EvaluationSymbol::Gz)));
        test_char('+', Token::Symbol(Symbol::Op(OpSymbol::Plus)));
        test_char('-', Token::Symbol(Symbol::Op(OpSymbol::Minus)));
        test_char('*', Token::Symbol(Symbol::Op(OpSymbol::Asterisk)));
        test_char('/', Token::Symbol(Symbol::Op(OpSymbol::FwdSlash)));
        test_char('=', Token::Symbol(Symbol::Equals));
        test_char('!', Token::Symbol(Symbol::Bang));
        test_char('&', Token::Symbol(Symbol::Ampsnd));
        test_char('|', Token::Symbol(Symbol::Pipe));
    }

    #[test]
    fn function_call() {
        test(
            "add x y",
            vec![
                Token::Ident("add".to_string()),
                Token::Symbol(Symbol::Whitespace),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Whitespace),
                Token::Ident("y".to_string()),
            ],
        );
    }

    #[test]
    fn function_def() {
        test(
            r"
add (x, y) {
    return x + y
}",
            vec![
                Token::Symbol(Symbol::Newline), // for formatting purposes
                Token::Ident("add".to_string()),
                Token::Symbol(Symbol::Whitespace),
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::Symbol(Symbol::Whitespace),
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::ParenClose),
                Token::Symbol(Symbol::Whitespace),
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // start of indentation
                Token::Symbol(Symbol::Whitespace),
                Token::Symbol(Symbol::Whitespace),
                Token::Symbol(Symbol::Whitespace),
                Token::Symbol(Symbol::Whitespace),
                // end of indentation
                Token::Kwd(Kwd::Return),
                Token::Symbol(Symbol::Whitespace),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Whitespace),
                Token::Symbol(Symbol::Op(OpSymbol::Plus)),
                Token::Symbol(Symbol::Whitespace),
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
        );
    }

    #[test]
    fn read_2_char_symbols() {
        test(
            "==",
            vec![Token::Symbol(Symbol::Evaluation(
                EvaluationSymbol::Equality,
            ))],
        );
        test(
            "!=",
            vec![Token::Symbol(Symbol::Evaluation(
                EvaluationSymbol::NotEquality,
            ))],
        );
        test("+=", vec![Token::Symbol(Symbol::PlusAssign)]);
        test("-=", vec![Token::Symbol(Symbol::MinusAssign)]);
        test("*=", vec![Token::Symbol(Symbol::MultiplyAssign)]);
        test("/=", vec![Token::Symbol(Symbol::DivideAssign)]);
        test("->", vec![Token::Symbol(Symbol::Arrow)]);
        test("&&", vec![Token::Symbol(Symbol::And)]);
        test("||", vec![Token::Symbol(Symbol::Or)]);
        test(
            ">=",
            vec![Token::Symbol(Symbol::Evaluation(EvaluationSymbol::GzEq))],
        );
        test(
            "<=",
            vec![Token::Symbol(Symbol::Evaluation(EvaluationSymbol::LzEq))],
        );
        test("..", vec![Token::Symbol(Symbol::Range)]);
    }

    #[test]
    fn read_3_char_symbols() {
        test("..=", vec![Token::Symbol(Symbol::RangeEq)])
    }

    #[test]
    fn hello_world_test() {
        test(
            "print \"Hello World\"",
            vec![
                Token::Ident("print".to_string()),
                Token::Symbol(Symbol::Whitespace),
                Token::Literal(Literal::String("Hello World".to_string())),
            ],
        );
    }

    #[test]
    fn read_escaped_characters() {
        test("\\\"", vec![Token::Symbol(Symbol::EscapeQuote)]);

        test("\\'", vec![Token::Symbol(Symbol::EscapeApos)]);
    }
}
