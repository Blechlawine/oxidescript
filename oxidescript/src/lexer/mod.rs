pub mod token;
pub mod tokens;
pub mod utils;

use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0};
use nom::combinator::{map, map_res, recognize};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair};
use nom::*;

use std::str;
use std::str::FromStr;

use self::token::Token;
use self::utils::{concat_slice_vec, convert_vec_utf8};

// Inspired by: https://github.com/Rydgel/monkey-rust

macro_rules! syntax {
    ($fn_name: ident, $tag_string: literal, $output_token: expr) => {
        fn $fn_name(s: &[u8]) -> IResult<&[u8], Token> {
            map(tag($tag_string), |_| $output_token)(s)
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

pub fn lex_operator(input: &[u8]) -> IResult<&[u8], Token> {
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
    ))(input)
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

pub fn lex_punctuation(input: &[u8]) -> IResult<&[u8], Token> {
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
    ))(input)
}

fn parse_inside_string(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (rest1, found1) = take(1usize)(input)?;
    match found1.as_bytes() {
        b"\"" => Ok((input, vec![])),
        b"\\" => {
            // We found an \ escape character
            let (rest2, found2) = take(1usize)(rest1)?;
            parse_inside_string(rest2).map(|(slice, done)| (slice, concat_slice_vec(found2, done)))
        }
        c => parse_inside_string(rest1).map(|(slice, done)| (slice, concat_slice_vec(c, done))),
    }
}

fn string(input: &[u8]) -> IResult<&[u8], String> {
    delimited(
        tag("\""),
        map_res(parse_inside_string, convert_vec_utf8),
        tag("\""),
    )(input)
}

fn lex_string(input: &[u8]) -> IResult<&[u8], Token> {
    map(string, Token::StringLiteral)(input)
}

// Identifiers
fn lex_keyword_or_ident(input: &[u8]) -> IResult<&[u8], Token> {
    map_res(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: &[u8]| {
            str::from_utf8(s).map(|syntax| match syntax {
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
                // "enum" => Token::Enum,
                // "type" => Token::Type,
                // "trait" => Token::Trait,
                // "impl" => Token::Impl,
                // "import" => Token::Import,
                // "export" => Token::Export,
                _ => Token::Ident(syntax.to_string()),
            })
        },
    )(input)
}

// Numbers
fn lex_number(input: &[u8]) -> IResult<&[u8], Token> {
    map(
        map_res(
            map_res(
                recognize(many1(alt((digit1, tag("."), tag("_"))))),
                str::from_utf8,
            ),
            FromStr::from_str,
        ),
        Token::NumberLiteral,
    )(input)
}

// Illegal
fn lex_illegal(input: &[u8]) -> IResult<&[u8], Token> {
    // This just matches anything to Token::Illegal, because it is the last parser to be called in lex_token
    map(take(1usize), |_| Token::Illegal)(input)
}

fn lex_token(input: &[u8]) -> IResult<&[u8], Token> {
    alt((
        lex_operator,
        lex_punctuation,
        lex_string,
        lex_keyword_or_ident,
        lex_number,
        lex_illegal,
    ))(input)
}

fn lex_tokens(input: &[u8]) -> IResult<&[u8], Vec<Token>> {
    many0(delimited(multispace0, lex_token, multispace0))(input)
}

pub struct Lexer;

impl Lexer {
    pub fn lex_tokens(bytes: &[u8]) -> IResult<&[u8], Vec<Token>> {
        lex_tokens(bytes)
            .map(|(slice, result)| (slice, [&result[..], &vec![Token::EOF][..]].concat()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_line() {
        let input = b"let x = 1;";
        let (rest, tokens) = Lexer::lex_tokens(input).unwrap();
        assert_eq!(rest, b"");
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Ident("x".to_string()),
                Token::Assign,
                Token::NumberLiteral("1".to_string()),
                Token::SemiColon,
                Token::EOF
            ]
        )
    }

    #[test]
    fn operators_punctuation() {
        let input = b"=+/*%-()[]{},;:.<>!<<>>|&^||&&~";
        let (rest, tokens) = Lexer::lex_tokens(input).unwrap();
        assert_eq!(rest, b"");
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
        test(12, 34);"
            .as_bytes();

        let (rest, tokens) = Lexer::lex_tokens(input).unwrap();
        assert_eq!(rest, b"");
        assert_eq!(
            tokens,
            vec![
                Token::Const,
                Token::Ident("stuff".to_string()),
                Token::Assign,
                Token::NumberLiteral("1.0".to_string()),
                Token::SemiColon,
                Token::Let,
                Token::Ident("hello123".to_string()),
                Token::Assign,
                Token::NumberLiteral("2.23".to_string()),
                Token::SemiColon,
                Token::Function,
                Token::Ident("test".to_string()),
                Token::LParen,
                Token::Ident("a".to_string()),
                Token::Colon,
                Token::Ident("number".to_string()),
                Token::Comma,
                Token::Ident("b".to_string()),
                Token::Colon,
                Token::Ident("number".to_string()),
                Token::RParen,
                Token::LSquirly,
                Token::Const,
                Token::Ident("things".to_string()),
                Token::Assign,
                Token::Ident("stuff".to_string()),
                Token::Plus,
                Token::Ident("hello123".to_string()),
                Token::Multiply,
                Token::Ident("a".to_string()),
                Token::Minus,
                Token::Ident("b".to_string()),
                Token::SemiColon,
                Token::Return,
                Token::Ident("things".to_string()),
                Token::SemiColon,
                Token::RSquirly,
                Token::Ident("test".to_string()),
                Token::LParen,
                Token::NumberLiteral("12".to_string()),
                Token::Comma,
                Token::NumberLiteral("34".to_string()),
                Token::RParen,
                Token::SemiColon,
                Token::EOF
            ]
        )
    }

    #[test]
    fn string_literals() {
        let (_, result) = Lexer::lex_tokens(&b"\"foobar\""[..]).unwrap();
        assert_eq!(
            result,
            vec![Token::StringLiteral("foobar".to_owned()), Token::EOF]
        );

        let (_, result) = Lexer::lex_tokens(&b"\"foo bar\""[..]).unwrap();
        assert_eq!(
            result,
            vec![Token::StringLiteral("foo bar".to_owned()), Token::EOF]
        );

        let (_, result) = Lexer::lex_tokens(&b"\"foo\nbar\""[..]).unwrap();
        assert_eq!(
            result,
            vec![Token::StringLiteral("foo\nbar".to_owned()), Token::EOF]
        );

        let (_, result) = Lexer::lex_tokens(&b"\"foo\tbar\""[..]).unwrap();
        assert_eq!(
            result,
            vec![Token::StringLiteral("foo\tbar".to_owned()), Token::EOF]
        );

        let (_, result) = Lexer::lex_tokens(&b"\"foo\\\"bar\""[..]).unwrap();
        assert_eq!(
            result,
            vec![Token::StringLiteral("foo\"bar".to_owned()), Token::EOF]
        );

        let (_, result) =
            Lexer::lex_tokens(&b"\"foo\\\"bar with \xf0\x9f\x92\x96 emojis\""[..]).unwrap();
        assert_eq!(
            result,
            vec![
                Token::StringLiteral("foo\"bar with 💖 emojis".to_owned()),
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
        "
        .as_bytes();

        let (rest, tokens) = Lexer::lex_tokens(input).unwrap();
        assert_eq!(rest, b"");
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
