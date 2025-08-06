pub mod token;
pub mod tokens;

use combinator::all_consuming;
use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take};
use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0};
use nom::character::none_of;
use nom::combinator::{map, recognize};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair};
use nom::*;
use nom_locate::LocatedSpan;

use std::collections::HashMap;
use std::str;

use crate::error::ParserError;
use crate::loader::{LexedSourceTree, SourceTree};

use self::token::Token;

// Inspired by: https://github.com/Rydgel/monkey-rust

macro_rules! syntax {
    ($fn_name: ident, $tag_string: literal, $output_token: expr) => {
        fn $fn_name(s: Span) -> IResult<Span, Token> {
            map(tag($tag_string), |_| $output_token).parse(s)
        }
    };
}

// operators
syntax!(equal_operator, "==", Token::Equal);
syntax!(not_equal_operator, "!=", Token::NotEqual);
syntax!(greater_than_operator, ">", Token::GreaterThan);
syntax!(less_than_operator, "<", Token::LessThan);
syntax!(logical_not_operator, "!", Token::LogicalNot);
syntax!(logical_and_operator, "&&", Token::LogicalAnd);
syntax!(logical_or_operator, "||", Token::LogicalOr);
syntax!(bitwise_not_operator, "~", Token::BitwiseNot);
syntax!(bitwise_and_operator, "&", Token::BitwiseAnd);
syntax!(bitwise_or_operator, "|", Token::BitwiseOr);
syntax!(bitwise_xor_operator, "^", Token::BitwiseXor);
syntax!(bitwise_left_shift_operator, "<<", Token::BitwiseLeftShift);
syntax!(bitwise_right_shift_operator, ">>", Token::BitwiseRightShift);
syntax!(greater_than_equal_operator, ">=", Token::GreaterThanEqual);
syntax!(less_than_equal_operator, "<=", Token::LessThanEqual);
syntax!(plus_operator, "+", Token::Plus);
syntax!(minus_operator, "-", Token::Minus);
syntax!(multiply_operator, "*", Token::Multiply);
syntax!(divide_operator, "/", Token::Divide);
syntax!(modulo_operator, "%", Token::Modulo);
syntax!(assign_operator, "=", Token::Assign);

pub fn lex_operator(input: Span) -> IResult<Span, Token> {
    alt((
        equal_operator,
        not_equal_operator,
        assign_operator,
        logical_not_operator,
        logical_and_operator,
        logical_or_operator,
        bitwise_not_operator,
        bitwise_and_operator,
        bitwise_or_operator,
        bitwise_xor_operator,
        bitwise_left_shift_operator,
        bitwise_right_shift_operator,
        greater_than_operator,
        less_than_operator,
        greater_than_equal_operator,
        less_than_equal_operator,
        plus_operator,
        minus_operator,
        multiply_operator,
        divide_operator,
        modulo_operator,
    ))
    .parse(input)
}

// punctuation
syntax!(comma_punctuation, ",", Token::Comma);
syntax!(period_punctuation, ".", Token::Period);
syntax!(colon_punctuation, ":", Token::Colon);
syntax!(semi_colon_punctuation, ";", Token::SemiColon);
syntax!(l_paren_punctuation, "(", Token::LParen);
syntax!(r_paren_punctuation, ")", Token::RParen);
syntax!(l_bracket_punctuation, "[", Token::LBracket);
syntax!(r_bracket_punctuation, "]", Token::RBracket);
syntax!(l_squirly_punctuation, "{", Token::LSquirly);
syntax!(r_squirly_punctuation, "}", Token::RSquirly);

pub fn lex_punctuation(input: Span) -> IResult<Span, Token> {
    alt((
        comma_punctuation,
        period_punctuation,
        colon_punctuation,
        semi_colon_punctuation,
        l_paren_punctuation,
        r_paren_punctuation,
        l_bracket_punctuation,
        r_bracket_punctuation,
        l_squirly_punctuation,
        r_squirly_punctuation,
    ))
    .parse(input)
}

fn lex_in_string(input: Span) -> IResult<Span, Span> {
    escaped(
        none_of(r#"\""#),
        '\\',
        alt((tag(r#"""#), tag("n"), tag("r"), tag("t"), tag("\\"))),
    )
    .parse(input)
}

fn string(input: Span) -> IResult<Span, Span> {
    delimited(tag("\""), lex_in_string, tag("\"")).parse(input)
}

fn lex_string(input: Span) -> IResult<Span, Token> {
    map(string, |s| Token::StringLiteral(s.fragment())).parse(input)
}

// Identifiers
fn lex_keyword_or_ident(input: Span) -> IResult<Span, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: Span| match *s.fragment() {
            "const" => Token::Const,
            "let" => Token::Let,
            "fn" => Token::Function,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "in" => Token::In,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "true" => Token::BooleanLiteral(true),
            "false" => Token::BooleanLiteral(false),
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "type" => Token::Type,
            "trait" => Token::Trait,
            "impl" => Token::Impl,
            "mod" => Token::Mod,
            "use" => Token::Use,
            "pub" => Token::Pub,
            "extern" => Token::Extern,
            s => Token::Ident(s),
        },
    )
    .parse(input)
}

// Numbers
fn lex_float_literal(input: Span) -> IResult<Span, Token> {
    map(
        recognize(many1(alt((digit1, tag("."), tag("_"))))),
        |s: Span| Token::NumberLiteral(s.fragment()),
    )
    .parse(input)
}

// Illegal
fn lex_illegal(input: Span) -> IResult<Span, Token> {
    // This just matches anything to Token::Illegal, because it is the last parser to be called in lex_token
    map(take(1usize), |_| Token::Illegal).parse(input)
}

fn lex_token(input: Span) -> IResult<Span, Token> {
    alt((
        lex_operator,
        lex_punctuation,
        lex_string,
        lex_keyword_or_ident,
        lex_float_literal,
        lex_illegal,
    ))
    .parse(input)
}

fn lex_tokens(input: Span) -> IResult<Span, Vec<Token>> {
    all_consuming(many0(delimited(multispace0, lex_token, multispace0))).parse(input)
}

pub type Span<'src> = LocatedSpan<&'src str, ()>;

pub struct Lexer {
    input: SourceTree,
    errors: Vec<ParserError>,
}

impl Lexer {
    pub fn tokenize(tree: &'_ SourceTree) -> LexedSourceTree<'_> {
        let mut output = HashMap::new();
        for (path, source) in tree {
            let path = path.iter().flat_map(|p| p.to_str());
            let path = path
                .map(|p| {
                    let (rest, parsed) = lex_module_identifier(p).unwrap();
                    assert!(rest.is_empty());
                    parsed
                })
                .collect::<Vec<_>>();
            let span = Span::new(source.as_str());
            let (rest, tokens) = Lexer::lex_tokens(span).unwrap();
            assert!(rest.fragment().is_empty());
            output.insert(path, tokens);
        }
        output
    }

    pub fn lex_tokens(input: Span) -> IResult<Span, Vec<Token>> {
        lex_tokens(input)
            .map(|(slice, result)| (slice, [&result[..], &vec![Token::EOF][..]].concat()))
    }
}

fn lex_module_identifier(input: &str) -> IResult<&str, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: &str| Token::Ident(s),
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_line() {
        let input = "let x = 1;";
        let input = Span::new(input);
        let (rest, tokens) = Lexer::lex_tokens(input).unwrap();
        assert_eq!(*rest.fragment(), "");
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Ident("x"),
                Token::Assign,
                Token::NumberLiteral("1"),
                Token::SemiColon,
                Token::EOF
            ]
        )
    }

    #[test]
    fn operators_punctuation() {
        let input = "=+/*%-()[]{},;:.<>!<<>>|&^||&&~";
        let input = Span::new(input);
        let (rest, tokens) = Lexer::lex_tokens(input).unwrap();
        assert_eq!(*rest.fragment(), "");
        assert_eq!(
            tokens,
            vec![
                Token::Assign,
                Token::Plus,
                Token::Divide,
                Token::Multiply,
                Token::Modulo,
                Token::Minus,
                Token::LParen,
                Token::RParen,
                Token::LBracket,
                Token::RBracket,
                Token::LSquirly,
                Token::RSquirly,
                Token::Comma,
                Token::SemiColon,
                Token::Colon,
                Token::Period,
                Token::LessThan,
                Token::GreaterThan,
                Token::LogicalNot,
                Token::BitwiseLeftShift,
                Token::BitwiseRightShift,
                Token::BitwiseOr,
                Token::BitwiseAnd,
                Token::BitwiseXor,
                Token::LogicalOr,
                Token::LogicalAnd,
                Token::BitwiseNot,
                Token::EOF,
            ]
        )
    }

    #[test]
    fn code_snippet() {
        let input = "
        const stuff = 1.0;
        let hello123 = 2.23;
        fn test(a: number, b: number) {
            const things = stuff + hello123 * a - b;
            return things;
        }
        test(12, 34);";

        let input = Span::new(input);
        let (rest, tokens) = Lexer::lex_tokens(input).unwrap();
        assert_eq!(*rest.fragment(), "");
        assert_eq!(
            tokens,
            vec![
                Token::Const,
                Token::Ident("stuff"),
                Token::Assign,
                Token::NumberLiteral("1.0"),
                Token::SemiColon,
                Token::Let,
                Token::Ident("hello123"),
                Token::Assign,
                Token::NumberLiteral("2.23"),
                Token::SemiColon,
                Token::Function,
                Token::Ident("test"),
                Token::LParen,
                Token::Ident("a"),
                Token::Colon,
                Token::Ident("number"),
                Token::Comma,
                Token::Ident("b"),
                Token::Colon,
                Token::Ident("number"),
                Token::RParen,
                Token::LSquirly,
                Token::Const,
                Token::Ident("things"),
                Token::Assign,
                Token::Ident("stuff"),
                Token::Plus,
                Token::Ident("hello123"),
                Token::Multiply,
                Token::Ident("a"),
                Token::Minus,
                Token::Ident("b"),
                Token::SemiColon,
                Token::Return,
                Token::Ident("things"),
                Token::SemiColon,
                Token::RSquirly,
                Token::Ident("test"),
                Token::LParen,
                Token::NumberLiteral("12"),
                Token::Comma,
                Token::NumberLiteral("34"),
                Token::RParen,
                Token::SemiColon,
                Token::EOF
            ]
        )
    }

    #[test]
    fn string_literals() {
        let (_, result) = Lexer::lex_tokens(Span::new("\"foobar\"")).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foobar"), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(Span::new("\"foo bar\"")).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo bar"), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(Span::new("\"foo\nbar\"")).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo\nbar"), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(Span::new("\"foo\tbar\"")).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo\tbar"), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(Span::new(r#""foo\"bar""#)).unwrap();
        assert_eq!(
            result,
            vec![Token::StringLiteral(r#"foo\"bar"#), Token::EOF]
        );

        let (_, result) = Lexer::lex_tokens(Span::new(r#""foo\"bar with 💖 emojis""#)).unwrap();
        assert_eq!(
            result,
            vec![
                Token::StringLiteral(r#"foo\"bar with 💖 emojis"#),
                Token::EOF
            ]
        );
    }

    #[test]
    fn keywords() {
        let input = "
        const
        let
        fn
        return
        if
        else
        while
        for
        in
        break
        continue
        true
        false
        ";

        let input = Span::new(input);
        let (rest, tokens) = Lexer::lex_tokens(input).unwrap();
        assert_eq!(*rest.fragment(), "");
        assert_eq!(
            tokens,
            vec![
                Token::Const,
                Token::Let,
                Token::Function,
                Token::Return,
                Token::If,
                Token::Else,
                Token::While,
                Token::For,
                Token::In,
                Token::Break,
                Token::Continue,
                Token::BooleanLiteral(true),
                Token::BooleanLiteral(false),
                Token::EOF
            ]
        )
    }
}
