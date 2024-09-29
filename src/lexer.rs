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


#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Kwd(Kwd),
    Ident(String),
    Symbol(Symbol),
    Literal(Literal),
}
#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    BoolLiteral(bool),
    IntLiteral(i32),
    // TODO: Consider Float literal
}

#[derive(Debug, PartialEq, Eq)]
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum DataTypeKwd {
    Bool,
    Char,
    Float,
    Int,
    String,
}

#[derive(Debug, PartialEq, Eq)]
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
// ------------------------------------------------------------
///
/// API
///
/// Usage:
///
/// ```
/// let token_interator = Lexer::new(input)
/// let tokens = token_interator.collect::<Vec<Token>>(); // Stream of tokens
///
/// ```
// ------------------------------------------------------------

/// API consists of a lexer struct with the Iterator trait implemented
struct Lexer {
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
    fn advance(&mut self) -> Option<char> {
        self.position += 1;
        self.input.get(self.position).copied()
    }

    fn step_back(&mut self) {
        self.position -= 1;
    }
}

fn peek_symbol(lexer: &mut Lexer, test: char, matched: Symbol, not_matched: Symbol) -> Token {
    let next = lexer.advance();
    if let Some(c) = next {
        if c == test {
            Token::Symbol(matched)
        } else {
            Token::Symbol(not_matched)
        }
    } else {
        Token::Symbol(not_matched)
    }
}

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
                '<' => Token::Symbol(Symbol::ChevOpen),
                '>' => Token::Symbol(Symbol::ChevClose),
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
                '+' => peek_symbol(self, '=', Symbol::PlusAssign, Symbol::Plus),
                '-' => {
                    let final_symbol =
                        peek_symbol(self, '=', Symbol::MinusAssign, Symbol::Minus);
                    if final_symbol != Token::Symbol(Symbol::MinusAssign) {
                        self.step_back();
                        peek_symbol(self, '>', Symbol::Arrow, Symbol::Minus)
                    } else {
                        final_symbol
                    }
                }

                '*' => peek_symbol(self, '=', Symbol::MultiplyAssign, Symbol::Asterisk),
                '/' => peek_symbol(self, '=', Symbol::DivideAssign, Symbol::Asterisk),
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

// TODO: Single symbol tests

#[cfg(test)]
mod test {
    use super::{DataTypeKwd, Lexer, Token};

    #[test]
    fn read_kwd_data_type() {
        let input = "bool";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::Bool))];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "char";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::Char))];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "float";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::Float))];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "int";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::Int))];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "string";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::DataType(DataTypeKwd::String))];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);
    }

    #[test]
    fn read_kwd_non_data_type() {
        let input = "while";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::While)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "for";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::For)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "if";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::If)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "return";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::Return)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "in";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::In)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "break";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::Break)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "include";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::Include)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "else";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Kwd(super::Kwd::Else)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);
    }
    #[test]
    fn read_literals() {
        let input = "true";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Literal(super::Literal::BoolLiteral(true))];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "false";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Literal(super::Literal::BoolLiteral(false))];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "3";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Literal(super::Literal::IntLiteral(3))];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);
    }
    #[test]
    fn read_2_char_symbols() {
        let input = "==";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::Equality)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "!=";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::NotEquality)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "+=";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::PlusAssign)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "-=";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::MinusAssign)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "*=";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::MultiplyAssign)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "/=";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::DivideAssign)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "->";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::Arrow)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "&&";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::And)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);

        let input = "||";
        let lexer = Lexer::new(input);
        let expected = vec![Token::Symbol(crate::lexer::Symbol::Or)];
        let actual = lexer.collect::<Vec<Token>>();
        assert_eq!(expected, actual);
    }
}
