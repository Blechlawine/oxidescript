use crate::lexer::token::Token;
use crate::lexer::tokens::Tokens;
use nom::bytes::complete::take;
use nom::combinator::verify;
use nom::IResult;

use super::ast::{InfixOperator, Precedence};

fn take1(input: Tokens) -> IResult<Tokens, Tokens> {
    take(1usize)(input)
}

macro_rules! tag_token (
    ($func_name: ident, $tag: expr) => (
        pub fn $func_name(tokens: Tokens) -> IResult<Tokens, Tokens> {
            // println!("{:?} == {:?}", $tag, &tokens.tokens[0]);
            verify(take1, |t: &Tokens| {
                t.tokens[0] == $tag
            })(tokens)
        }
    )
);

tag_token!(let_tag, Token::Let);
tag_token!(const_tag, Token::Const);
tag_token!(function_tag, Token::Function);
tag_token!(return_tag, Token::Return);

tag_token!(assign_tag, Token::Assign);
tag_token!(plus_tag, Token::Plus);
tag_token!(minus_tag, Token::Minus);
tag_token!(multiply_tag, Token::Multiply);
tag_token!(divide_tag, Token::Divide);
tag_token!(modulo_tag, Token::Modulo);

tag_token!(logical_not_tag, Token::LogicalNot);
tag_token!(logical_and_tag, Token::LogicalAnd);
tag_token!(logical_or_tag, Token::LogicalOr);

tag_token!(bitwise_not_tag, Token::BitwiseNot);
tag_token!(bitwise_and_tag, Token::BitwiseAnd);
tag_token!(bitwise_or_tag, Token::BitwiseOr);
tag_token!(bitwise_xor_tag, Token::BitwiseXor);
tag_token!(bitwise_left_shift_tag, Token::BitwiseLeftShift);
tag_token!(bitwise_right_shift_tag, Token::BitwiseRightShift);

tag_token!(equal_tag, Token::Equal);
tag_token!(not_equal_tag, Token::NotEqual);
tag_token!(greater_than_tag, Token::GreaterThan);
tag_token!(less_than_tag, Token::LessThan);
tag_token!(greater_than_equal_tag, Token::GreaterThanEqual);
tag_token!(less_than_equal_tag, Token::LessThanEqual);

tag_token!(l_paren_tag, Token::LParen);
tag_token!(r_paren_tag, Token::RParen);
tag_token!(l_bracket_tag, Token::LBracket);
tag_token!(r_bracket_tag, Token::RBracket);
tag_token!(l_squirly_tag, Token::LSquirly);
tag_token!(r_squirly_tag, Token::RSquirly);

tag_token!(comma_tag, Token::Comma);
tag_token!(colon_tag, Token::Colon);
tag_token!(period_tag, Token::Period);

tag_token!(semicolon_tag, Token::SemiColon);
tag_token!(eof_tag, Token::EOF);

pub fn infix_operator(token: &Token) -> (Precedence, Option<InfixOperator>) {
    match token {
        Token::Equal => (Precedence::PEquals, Some(InfixOperator::Equal)),
        Token::NotEqual => (Precedence::PEquals, Some(InfixOperator::NotEqual)),
        Token::LessThanEqual => (Precedence::PLessGreater, Some(InfixOperator::LessThanEqual)),
        Token::GreaterThanEqual => (
            Precedence::PLessGreater,
            Some(InfixOperator::GreaterThanEqual),
        ),
        Token::LessThan => (Precedence::PLessGreater, Some(InfixOperator::LessThan)),
        Token::GreaterThan => (Precedence::PLessGreater, Some(InfixOperator::GreaterThan)),
        Token::Plus => (Precedence::PSum, Some(InfixOperator::Plus)),
        Token::Minus => (Precedence::PSum, Some(InfixOperator::Minus)),
        Token::Multiply => (Precedence::PProduct, Some(InfixOperator::Multiply)),
        Token::Divide => (Precedence::PProduct, Some(InfixOperator::Divide)),
        Token::Modulo => (Precedence::PProduct, Some(InfixOperator::Modulo)),
        Token::LogicalAnd => (Precedence::PLogicalAnd, Some(InfixOperator::LogicalAnd)),
        Token::LogicalOr => (Precedence::PLogicalOr, Some(InfixOperator::LogicalOr)),
        Token::BitwiseAnd => (Precedence::PBitwiseAnd, Some(InfixOperator::BitwiseAnd)),
        Token::BitwiseOr => (Precedence::PBitwiseOr, Some(InfixOperator::BitwiseOr)),
        Token::BitwiseXor => (Precedence::PBitwiseXor, Some(InfixOperator::BitwiseXor)),
        Token::BitwiseLeftShift => (
            Precedence::PBitwiseShift,
            Some(InfixOperator::BitwiseLeftShift),
        ),
        Token::BitwiseRightShift => (
            Precedence::PBitwiseShift,
            Some(InfixOperator::BitwiseRightShift),
        ),
        Token::LParen => (Precedence::PCall, None),
        Token::Period => (Precedence::PMemberAccess, None),
        Token::LBracket => (Precedence::PIndex, None),
        _ => (Precedence::PLowest, None),
    }
}
