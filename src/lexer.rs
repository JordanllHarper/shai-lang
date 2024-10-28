//   _____ __ __   ____  ____      _        ___  __ __    ___  ____
//  / ___/|  |  | /    ||    |    | |      /  _]|  |  |  /  _]|    \
// (   \_ |  |  ||  o  | |  |     | |     /  [_ |  |  | /  [_ |  D  )
//  \__  ||  _  ||     | |  |     | |___ |    _]|_   _||    _]|    /
//  /  \ ||  |  ||  _  | |  |     |     ||   [_ |     ||   [_ |    \
//  \    ||  |  ||  |  | |  |     |     ||     ||  |  ||     ||  .  \
//   \___||__|__||__|__||____|    |_____||_____||__|__||_____||__|\_|

// Translates raw text into a stream of tokens
//
//
//
// DATA STRUCTS

const WHITESPACE: char = ' ';

use std::{
    collections::HashSet,
    fmt::{Display, Formatter},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Kwd(Kwd),
    Ident(String),
    Symbol(Symbol),
    Literal(Literal),
}

impl Token {
    pub fn whitespace() -> Token {
        Token::Symbol(Symbol::Whitespace)
    }
}

impl Display for DataTypeKwd {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let representation = match self {
            DataTypeKwd::Bool => "bool",
            DataTypeKwd::Char => "char",
            DataTypeKwd::Float => "float",
            DataTypeKwd::Int => "int",
            DataTypeKwd::String => "string",
            DataTypeKwd::Void => "void",
            DataTypeKwd::Array(a) => &format!("{}[]", &a),
        };
        f.write_str(representation)
    }
}

impl Display for Kwd {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Kwd::DataType(d) => &d.to_string(),
            Kwd::While => "while",
            Kwd::For => "for",
            Kwd::If => "if",
            Kwd::Return => "return",
            Kwd::In => "in",
            Kwd::Break => "break",
            Kwd::Include => "include",
            Kwd::Else => "else",
            Kwd::Const => "const",
        };
        f.write_str(str)
    }
}
impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Symbol::ParenOpen => "(",
            Symbol::ParenClose => ")",
            Symbol::ChevOpen => "<",
            Symbol::ChevClose => ">",
            Symbol::AngOpen => "[",
            Symbol::AngClose => "]",
            Symbol::BraceOpen => "{",
            Symbol::BraceClose => "}",
            Symbol::Equals => "=",
            Symbol::Modulus => "%",
            Symbol::Quote => "\"",
            Symbol::Apstr => "\'",
            Symbol::Comma => ",",
            Symbol::Minus => "-",
            Symbol::Period => ".",
            Symbol::Plus => "+",
            Symbol::Asterisk => "*",
            Symbol::FwdSlash => "/",
            Symbol::BckSlash => "\\",
            Symbol::Dollar => "$",
            Symbol::Colon => ":",
            Symbol::Underscore => "_",
            Symbol::Ampsnd => "&",
            Symbol::Pipe => "|",
            Symbol::Whitespace => " ",
            Symbol::Bang => "!",
            Symbol::Newline => "\n",
            Symbol::Equality => "==",
            Symbol::NotEquality => "!=",
            Symbol::PlusAssign => "+=",
            Symbol::MinusAssign => "-=",
            Symbol::MultiplyAssign => "*=",
            Symbol::DivideAssign => "/=",
            Symbol::Arrow => "->",
            Symbol::And => "&&",
            Symbol::Or => "||",
            Symbol::LzEq => "<=",
            Symbol::GzEq => ">=",
            Symbol::EscapeQuote => "\\\"",
            Symbol::EscapeApos => "\\\'",
            Symbol::Comment(c) => c,
            Symbol::MultilineComment(c) => c,
        };
        f.write_str(str)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str_rep = match self {
            Token::Kwd(k) => &k.to_string(),
            Token::Ident(i) => i,
            Token::Symbol(s) => &s.to_string(),
            Token::Literal(l) => &l.to_string(),
        };
        f.write_str(str_rep)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str_rep = match self {
            Literal::Bool(b) => &b.to_string(),
            Literal::Int(i) => &i.to_string(),
            Literal::String(s) => &s.to_string(),
            Literal::Float(f) => &f.to_string(),
        };
        f.write_str(str_rep)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Symbol {
    ParenOpen,
    ParenClose,
    ChevOpen,
    ChevClose,
    AngOpen,
    AngClose,
    BraceOpen,
    BraceClose,
    Equals,
    Modulus,
    Quote,
    Apstr,
    Comma,
    Minus,
    Period,
    Plus,
    Asterisk,
    FwdSlash,
    BckSlash,
    Dollar,
    Colon,
    Underscore,
    Ampsnd,
    Pipe,
    Whitespace,
    Bang,
    Newline,
    // 2 char symbols
    Equality,
    NotEquality,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    Arrow,
    And,
    Or,
    LzEq,
    GzEq,
    MultilineComment(String),
    Comment(String),
    // Escape chars
    EscapeQuote,
    EscapeApos,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DataTypeKwd {
    Bool,
    Char,
    Float,
    Int,
    String,
    Void,
    Array(Box<DataTypeKwd>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Kwd {
    DataType(DataTypeKwd),
    While,
    For,
    If,
    Return,
    In,
    Break,
    Include,
    Else,
    Const,
}
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
fn parse_literal_or_identifier(s: &str) -> Token {
    if let Ok(r) = str::parse::<i32>(s) {
        Token::Literal(Literal::Int(r))
    } else if let Ok(r) = str::parse::<f32>(s) {
        Token::Literal(Literal::Float(r))
    } else {
        Token::Ident(s.to_string())
    }
}

fn is_symbol_applicable(symbol: &char) -> bool {
    let allowed_symbols = HashSet::from(['[', ']', '.']);
    allowed_symbols.contains(symbol)
}

/// Advances a lexer until a non-alphanumeric delimeter and converts into an optional [Token].
/// Returns Some if a text symbol can be found and None otherwise.
/// e.g. whitespace, which should never happen unless there is a "  " double space.
fn peek_non_symbol(lexer: &mut Lexer) -> Option<Token> {
    let current = lexer.input.get(lexer.position)?;
    let mut buf = current.to_string();

    // cover it being the end of the stream as well as delimiter
    while let Some(ch) = lexer.advance() {
        // allow designated symbols through
        if ch.is_alphanumeric() || is_symbol_applicable(&ch) {
            buf.push(ch);
        } else {
            break;
        }
    }

    let token = match buf.as_str() {
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
            if let Some(s) = get_datatype(s) {
                s
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
        "char" => Some(DataTypeKwd::Char),
        "float" => Some(DataTypeKwd::Float),
        "string" => Some(DataTypeKwd::String),
        _ => None,
    }
}

fn get_datatype(s: &str) -> Option<Token> {
    let s = s.trim();
    let s_components = s.split("[]").collect::<Vec<&str>>();
    let datatype = s_components.first()?.trim();
    let dt_kwd = map_datatype_kwd(datatype)?;

    if s_components.len() == 1 {
        Some(Token::Kwd(Kwd::DataType(dt_kwd)))
    } else {
        Some(Token::Kwd(Kwd::DataType(DataTypeKwd::Array(Box::new(
            dt_kwd,
        )))))
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
                '[' => Token::Symbol(Symbol::AngOpen),
                ']' => Token::Symbol(Symbol::AngClose),
                '{' => Token::Symbol(Symbol::BraceOpen),
                '}' => Token::Symbol(Symbol::BraceClose),
                '%' => Token::Symbol(Symbol::Modulus),
                '\'' => Token::Symbol(Symbol::Apstr),
                ',' => Token::Symbol(Symbol::Comma),
                '.' => Token::Symbol(Symbol::Period),
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
                '<' => peek_symbol(self, '=', Symbol::LzEq, Symbol::ChevOpen),
                '>' => peek_symbol(self, '=', Symbol::GzEq, Symbol::ChevClose),
                '+' => peek_symbol(self, '=', Symbol::PlusAssign, Symbol::Plus),
                '-' => {
                    let final_symbol = peek_symbol(self, '=', Symbol::MinusAssign, Symbol::Minus);
                    if final_symbol != Token::Symbol(Symbol::MinusAssign) {
                        self.step_back();
                        peek_symbol(self, '>', Symbol::Arrow, Symbol::Minus)
                    } else {
                        final_symbol
                    }
                }

                '*' => peek_symbol(self, '=', Symbol::MultiplyAssign, Symbol::Asterisk),
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
                        peek_symbol(self, '=', Symbol::DivideAssign, Symbol::FwdSlash)
                    }

                    // if let Some(c) = peek_while('/', self, |c| {
                    //     Token::Symbol(Symbol::Comment(String::from_iter(c.iter())))
                    // }) {
                    //     c
                    // } else if let Some(c) = maybe_peek(self, '*', Symbol::MultilineComment) {
                    //     c
                    // } else {
                    //     peek_symbol(self, '=', Symbol::DivideAssign, Symbol::FwdSlash)
                    // }
                }
                '=' => peek_symbol(self, '=', Symbol::Equality, Symbol::Equals),
                '!' => peek_symbol(self, '=', Symbol::NotEquality, Symbol::Bang),
                '&' => peek_symbol(self, '&', Symbol::And, Symbol::Ampsnd),
                '|' => peek_symbol(self, '|', Symbol::Or, Symbol::Pipe),
                _ => peek_non_symbol(self)?,
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
        let actual = lexer.next().unwrap();
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
            vec![
                Token::Symbol(Symbol::MultilineComment(
                    r"
hello
".to_string(),
                )),
            ],
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
        test("char", vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Char))]);
        test("float", vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Float))]);
        test("int", vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Int))]);
        test(
            "string",
            vec![Token::Kwd(Kwd::DataType(DataTypeKwd::String))],
        );
    }
    #[test]
    fn read_kwd_data_type_arrays() {
        test(
            "bool[]",
            vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Array(Box::new(
                DataTypeKwd::Bool,
            ))))],
        );

        test(
            "string[]",
            vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Array(Box::new(
                DataTypeKwd::String,
            ))))],
        );

        test(
            "int[]",
            vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Array(Box::new(
                DataTypeKwd::Int,
            ))))],
        );

        test(
            "float[]",
            vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Array(Box::new(
                DataTypeKwd::Float,
            ))))],
        );

        test(
            "char[]",
            vec![Token::Kwd(Kwd::DataType(DataTypeKwd::Array(Box::new(
                DataTypeKwd::Char,
            ))))],
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
        test_char('%', Token::Symbol(Symbol::Modulus));
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
        test_char('<', Token::Symbol(Symbol::ChevOpen));
        test_char('>', Token::Symbol(Symbol::ChevClose));
        test_char('+', Token::Symbol(Symbol::Plus));
        test_char('-', Token::Symbol(Symbol::Minus));
        test_char('*', Token::Symbol(Symbol::Asterisk));
        test_char('/', Token::Symbol(Symbol::FwdSlash));
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
                Token::Symbol(Symbol::Plus),
                Token::Symbol(Symbol::Whitespace),
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Newline),
                Token::Symbol(Symbol::BraceClose),
            ],
        );
    }

    #[test]
    fn read_2_char_symbols() {
        test("==", vec![Token::Symbol(Symbol::Equality)]);
        test("!=", vec![Token::Symbol(Symbol::NotEquality)]);
        test("+=", vec![Token::Symbol(Symbol::PlusAssign)]);
        test("-=", vec![Token::Symbol(Symbol::MinusAssign)]);
        test("*=", vec![Token::Symbol(Symbol::MultiplyAssign)]);
        test("/=", vec![Token::Symbol(Symbol::DivideAssign)]);
        test("->", vec![Token::Symbol(Symbol::Arrow)]);
        test("&&", vec![Token::Symbol(Symbol::And)]);
        test("||", vec![Token::Symbol(Symbol::Or)]);
        test(">=", vec![Token::Symbol(Symbol::GzEq)]);
        test("<=", vec![Token::Symbol(Symbol::LzEq)]);
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
