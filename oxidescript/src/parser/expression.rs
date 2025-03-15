use core::panic;

use nom::combinator::opt;
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded};
use nom::{branch::alt, combinator::map, error_position, IResult};
use nom::{Err, Parser};

use crate::lexer::token::Token;
use crate::lexer::tokens::Tokens;

use super::ast::{ElseIfExpr, ForExpr, IfExpr, Precedence, UnaryExpr, UnaryOperator};
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
    )
    .parse(input)
}

pub fn parse_atom_expression(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        parse_literal_expression,
        parse_identifier_expression,
        parse_unary_expression,
        parse_paren_expression,
        parse_array_expression,
        parse_block_expression,
        parse_if_expression,
        parse_for_expression,
    ))
    .parse(input)
}

fn parse_literal_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(parse_literal, |literal| {
        Expression::LiteralExpression(literal)
    })
    .parse(input)
}

fn parse_identifier_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(parse_identifier, |identifier| {
        Expression::IdentifierExpression(identifier)
    })
    .parse(input)
}

fn parse_unary_expression(input: Tokens) -> IResult<Tokens, Expression> {
    let (rest1, unary) =
        alt((plus_tag, minus_tag, logical_not_tag, bitwise_not_tag)).parse(input)?;
    if unary.tokens.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let (rest2, expression) = parse_atom_expression(rest1)?;
        match unary.tokens[0] {
            Token::Plus => Ok((
                rest2,
                Expression::UnaryExpression(UnaryExpr {
                    op: UnaryOperator::Plus,
                    rhs: Box::new(expression),
                }),
            )),
            Token::Minus => Ok((
                rest2,
                Expression::UnaryExpression(UnaryExpr {
                    op: UnaryOperator::Minus,
                    rhs: Box::new(expression),
                }),
            )),
            Token::LogicalNot => Ok((
                rest2,
                Expression::UnaryExpression(UnaryExpr {
                    op: UnaryOperator::LogicalNot,
                    rhs: Box::new(expression),
                }),
            )),
            Token::BitwiseNot => Ok((
                rest2,
                Expression::UnaryExpression(UnaryExpr {
                    op: UnaryOperator::BitwiseNot,
                    rhs: Box::new(expression),
                }),
            )),
            _ => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
        }
    }
}

fn parse_paren_expression(input: Tokens) -> IResult<Tokens, Expression> {
    delimited(l_paren_tag, parse_expression, r_paren_tag).parse(input)
}

fn parse_array_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        delimited(
            l_bracket_tag,
            alt((parse_expressions, empty_boxed_vec)),
            r_bracket_tag,
        ),
        Expression::ArrayExpression,
    )
    .parse(input)
}

fn parse_block_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        delimited(l_squirly_tag, parse_block, r_squirly_tag),
        |block| Expression::BlockExpression(Box::new(block)),
    )
    .parse(input)
}

fn parse_if_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        (
            if_tag,
            parse_expression,
            parse_block_expression,
            many0((
                else_tag,
                if_tag,
                parse_expression,
                parse_block_expression,
            )),
            opt((else_tag, parse_block_expression)),
        ),
        |(_if, condition, then_block_expr, else_ifs, else_)| {
            if let Expression::BlockExpression(then_block) = then_block_expr {
                Expression::IfExpression(IfExpr {
                    condition: Box::new(condition),
                    then_block: Box::new(*then_block),
                    else_if_blocks: else_ifs
                        .into_iter()
                        .map(|(_else, _if, condition, block_expr)| {
                            if let Expression::BlockExpression(block) = block_expr {
                                ElseIfExpr {
                                    condition: Box::new(condition),
                                    then_block: *block
                                }
                            } else {
                                panic!("parse_block_expression parsed something other than a block expression");
                            }
                        })
                        .collect(),
                    else_block: else_
                        .map(|(_, block_expr)| {
                            if let Expression::BlockExpression(block) = block_expr {
                                block
                            } else {
                                panic!("parse_block_expression parsed something other than a block expression");
                            }
                        }),
                })
            } else {
                panic!("parse_block_expression parsed something other than a block expression");
            }
        },
    ).parse(input)
}

fn parse_for_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        (
            for_tag,
            parse_identifier,
            in_tag,
            parse_expression,
            parse_block_expression,
        ),
        |(_for, lhs, _in, rhs, body)| {
            if let Expression::BlockExpression(body) = body {
                Expression::ForExpression(ForExpr {
                    lhs,
                    rhs: Box::new(rhs),
                    body,
                })
            } else {
                panic!("parse_block_expression parsed something other than a block expression");
            }
        },
    )
    .parse(input)
}

fn empty_boxed_vec(input: Tokens) -> IResult<Tokens, Vec<Expression>> {
    Ok((input, vec![]))
}
