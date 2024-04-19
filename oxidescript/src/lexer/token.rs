#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Illegal,
    EOF,

    Ident(String),
    // Literals
    StringLiteral(String),
    NumberLiteral(String), // why not f64? because we can't Eq f64s
    BooleanLiteral(bool),

    // Operators
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Not,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Assign,

    // Punctuation
    Period,
    Comma,
    Colon,
    SemiColon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    LBracket,
    RBracket,

    // Keywords
    Const,
    Let,
    Function,
    Return,
    If,
    Else,
    While,
    For,
    In,
    Break,
    Continue,
    Struct,
    Enum,
    Type,
    Trait,
    Impl,
    Import,
    Export,
}
