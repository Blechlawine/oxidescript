use crate::lexer::token::Token;
use crate::lexer::tokens::Tokens;
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded};
use nom::Err;
use nom::{branch::alt, combinator::map, error_position, IResult};

use super::ast::{Precedence, UnaryOperator};
use super::function::parse_block;
use super::pratt_expression::parse_pratt_expression;
use super::{ast::Expression, atoms::*, parse_identifier, parse_literal};

pub fn parse_expression(input: Tokens) -> IResult<Tokens, Expression> {
    parse_pratt_expression(input, Precedence::PLowest)
}

pub fn parse_expressions(input: Tokens) -> IResult<Tokens, Vec<Expression>> {
    map(
        pair(
            parse_expression,
            many0(preceded(comma_tag, parse_expression)),
        ),
        |(first, second)| [&vec![first][..], &second[..]].concat(),
    )(input)
}

pub fn parse_atom_expression(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        parse_literal_expression,
        parse_identifier_expression,
        parse_unary_expression,
        parse_paren_expression,
        parse_array_expression,
        parse_block_expression,
    ))(input)
}

fn parse_literal_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(parse_literal, |literal| {
        Expression::LiteralExpression(literal)
    })(input)
}

fn parse_identifier_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(parse_identifier, |identifier| {
        Expression::IdentifierExpression(identifier)
    })(input)
}

fn parse_unary_expression(input: Tokens) -> IResult<Tokens, Expression> {
    let (rest1, unary) = alt((plus_tag, minus_tag, logical_not_tag, bitwise_not_tag))(input)?;
    if unary.tokens.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let (rest2, expression) = parse_atom_expression(rest1)?;
        match unary.tokens[0] {
            Token::Plus => Ok((
                rest2,
                Expression::UnaryExpression(UnaryOperator::Plus, Box::new(expression)),
            )),
            Token::Minus => Ok((
                rest2,
                Expression::UnaryExpression(UnaryOperator::Minus, Box::new(expression)),
            )),
            Token::LogicalNot => Ok((
                rest2,
                Expression::UnaryExpression(UnaryOperator::LogicalNot, Box::new(expression)),
            )),
            Token::BitwiseNot => Ok((
                rest2,
                Expression::UnaryExpression(UnaryOperator::BitwiseNot, Box::new(expression)),
            )),
            _ => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
        }
    }
}

fn parse_paren_expression(input: Tokens) -> IResult<Tokens, Expression> {
    delimited(l_paren_tag, parse_expression, r_paren_tag)(input)
}

fn parse_array_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        delimited(
            l_bracket_tag,
            alt((parse_expressions, empty_boxed_vec)),
            r_bracket_tag,
        ),
        Expression::ArrayExpression,
    )(input)
}

fn parse_block_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        delimited(l_squirly_tag, parse_block, r_squirly_tag),
        |block| Expression::BlockExpression(Box::new(block)),
    )(input)
}

fn empty_boxed_vec(input: Tokens) -> IResult<Tokens, Vec<Expression>> {
    Ok((input, vec![]))
}
