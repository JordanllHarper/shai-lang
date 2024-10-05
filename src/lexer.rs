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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Kwd(Kwd),
    Ident(String),
    Symbol(Symbol),
    Literal(Literal),
}

impl Token {
    pub fn whitespace() -> Token{
        Token::Symbol(Symbol::Whitespace)
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    BoolLiteral(bool),
    IntLiteral(i32),
    // TODO: Consider Float literal
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
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DataTypeKwd {
    Bool,
    Char,
    Float,
    Int,
    String,
    Void,
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

/// Advances a lexer until a non-alphanumeric delimeter and converts into an optional [Token].
/// Returns Some if a text symbol can be found and None otherwise.
/// e.g. whitespace, which should never happen unless there is a "  " double space.
fn peek_non_symbol(lexer: &mut Lexer) -> Option<Token> {
    let current = lexer.input.get(lexer.position)?;
    let mut buf = current.to_string();

    // cover it being the end of the stream as well as delimiter
    while let Some(ch) = lexer.advance() {
        if !ch.is_alphanumeric() {
            break;
        }
        buf.push(ch);
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
        "bool" => Token::Kwd(Kwd::DataType(DataTypeKwd::Bool)),
        "void" => Token::Kwd(Kwd::DataType(DataTypeKwd::Void)),
        "int" => Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
        "char" => Token::Kwd(Kwd::DataType(DataTypeKwd::Char)),
        "float" => Token::Kwd(Kwd::DataType(DataTypeKwd::Float)),
        "string" => Token::Kwd(Kwd::DataType(DataTypeKwd::String)),
        "true" => Token::Literal(Literal::BoolLiteral(true)),
        "false" => Token::Literal(Literal::BoolLiteral(false)),
        s => {
            // WARNING: This does not mean this is a 3 alone
            // This could be "3" still, we would figure that out in the next stage
            if let Ok(i) = str::parse::<i32>(s) {
                Token::Literal(Literal::IntLiteral(i))
            } else {
                Token::Ident(s.to_string())
            }
        }
    };
    lexer.step_back();
    Some(token)
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
                '\"' => Token::Symbol(Symbol::Quote),
                '\'' => Token::Symbol(Symbol::Apstr),
                ',' => Token::Symbol(Symbol::Comma),
                '.' => Token::Symbol(Symbol::Period),
                '\\' => Token::Symbol(Symbol::BckSlash),
                '$' => Token::Symbol(Symbol::Dollar),
                ':' => Token::Symbol(Symbol::Colon),
                '_' => Token::Symbol(Symbol::Underscore),
                '\n' => Token::Symbol(Symbol::Newline),
                ' ' => Token::Symbol(Symbol::Whitespace),
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
                '/' => peek_symbol(self, '=', Symbol::DivideAssign, Symbol::FwdSlash),
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
    use std::fmt::Debug;
    use pretty_assertions::assert_eq;

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
    fn read_kwd_data_type() {
        test(
            "bool",
            vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::Bool))],
        );
        test(
            "char",
            vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::Char))],
        );
        test(
            "float",
            vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::Float))],
        );
        test(
            "int",
            vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::Int))],
        );
        test(
            "string",
            vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::String))],
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
    }
    #[test]
    fn read_literals() {
        test("true", vec![Token::Literal(Literal::BoolLiteral(true))]);
        test("false", vec![Token::Literal(Literal::BoolLiteral(false))]);
        test("3", vec![Token::Literal(Literal::IntLiteral(3))]);
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
        test_char('\"', Token::Symbol(Symbol::Quote));
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
        let input = "print \"Hello World\"";

        let expected = vec![
            Token::Ident("print".to_string()),
            Token::Symbol(Symbol::Whitespace),
            Token::Symbol(Symbol::Quote),
            Token::Ident("Hello".to_string()),
            Token::Symbol(Symbol::Whitespace),
            Token::Ident("World".to_string()),
            Token::Symbol(Symbol::Quote),
        ];

        let actual = Lexer::new(input).collect::<Vec<Token>>();

        assert_eq!(expected, actual);
    }

    #[test]
    fn complex_variable_assignment_with_newline() {
        test("x = if true { return 5 } else { return 4 }\n", vec![
            Token::Ident("x".to_string()),
            Token::whitespace(),
            Token::Symbol(Symbol::Equals),
            Token::whitespace(),
            Token::Kwd(Kwd::If),
            Token::whitespace(),
            Token::Literal(Literal::BoolLiteral(true)),
            Token::whitespace(),
            Token::Symbol(Symbol::BraceOpen),
            Token::whitespace(),
            Token::Kwd(Kwd::Return),
            Token::whitespace(),
            Token::Literal(Literal::IntLiteral(5)),
            Token::whitespace(),
            Token::Symbol(Symbol::BraceClose),
            Token::whitespace(),
            Token::Kwd(Kwd::Else),
            Token::whitespace(),
            Token::Symbol(Symbol::BraceOpen),
            Token::whitespace(),
            Token::Kwd(Kwd::Return),
            Token::whitespace(),
            Token::Literal(Literal::IntLiteral(4)),
            Token::whitespace(),
            Token::Symbol(Symbol::BraceClose),
            Token::Symbol(Symbol::Newline)
        ]);
    }
}
