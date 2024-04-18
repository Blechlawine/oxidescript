use crate::lexer::token::Token;
use crate::lexer::tokens::Tokens;
use nom::bytes::complete::take;
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded};
use nom::Err;
use nom::{branch::alt, combinator::map, error_position, IResult};

use super::ast::{Precedence, UnaryOperator};
use super::{ast::Expression, atoms::*, parse_identifier, parse_literal};

fn parse_expression_with_precedence(
    input: Tokens,
    precedence: Precedence,
) -> IResult<Tokens, Expression> {
    let (rest, atom) = parse_atom_expression(input)?;
    parse_pratt_expression(rest, precedence, atom)
}

pub fn parse_expression(input: Tokens) -> IResult<Tokens, Expression> {
    parse_expression_with_precedence(input, Precedence::PLowest)
}

fn parse_pratt_expression(
    input: Tokens,
    precedence: Precedence,
    left: Expression,
) -> IResult<Tokens, Expression> {
    let (rest, found) = take(1usize)(input)?;
    if found.tokens.is_empty() {
        Ok((rest, left))
    } else {
        let preview = &found.tokens[0];
        let p = infix_operator(preview);
        match p {
            (Precedence::PCall, _) if precedence < Precedence::PCall => {
                // parse call expression
                todo!()
            }
            (Precedence::PIndex, _) if precedence < Precedence::PIndex => {
                // parse index expression
                todo!()
            }
            (peek_precedence, _) if precedence < peek_precedence => {
                let (rest2, left2) = parse_infix_expression(input, left)?;
                parse_pratt_expression(rest2, precedence, left2)
            }
            _ => Ok((input, left)),
        }
    }
}

fn parse_expressions(input: Tokens) -> IResult<Tokens, Vec<Expression>> {
    map(
        pair(
            parse_expression,
            many0(preceded(comma_tag, parse_expression)),
        ),
        |(first, second)| [&vec![first][..], &second[..]].concat(),
    )(input)
}

fn parse_atom_expression(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        parse_literal_expression,
        parse_identifier_expression,
        parse_unary_expression,
        parse_paren_expression,
        parse_array_expression,
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
    let (rest1, unary) = alt((plus_tag, minus_tag, not_tag))(input)?;
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
            Token::Not => Ok((
                rest2,
                Expression::UnaryExpression(UnaryOperator::Not, Box::new(expression)),
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

fn parse_infix_expression(input: Tokens, left: Expression) -> IResult<Tokens, Expression> {
    let (rest, operator) = take(1usize)(input)?;
    if operator.tokens.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let operator_token = &operator.tokens[0];
        let (precedence, maybe_infix_op) = infix_operator(operator_token);
        match maybe_infix_op {
            None => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
            Some(op) => {
                let (rest2, right) = parse_expression_with_precedence(rest, precedence)?;
                Ok((
                    rest2,
                    Expression::InfixExpression(op, Box::new(left), Box::new(right)),
                ))
            }
        }
    }
}

fn parse_if_expression(input: Tokens) -> IResult<Tokens, Expression> {
    todo!()
}

fn empty_boxed_vec(input: Tokens) -> IResult<Tokens, Vec<Expression>> {
    Ok((input, vec![]))
}
