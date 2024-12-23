use crate::parser::Err;
use nom::combinator::{map, opt};
use nom::error::ErrorKind;
use nom::error_position;
use nom::sequence::{delimited, tuple};
use nom::{bytes::complete::take, IResult};

use crate::lexer::tokens::Tokens;

use super::ast::{CallExpr, IndexExpr, InfixExpr, MemberAccessExpr};
use super::atoms::{l_bracket_tag, l_paren_tag, period_tag, r_bracket_tag, r_paren_tag};
use super::expression::{parse_expression, parse_expressions};
use super::parse_identifier;
use super::{
    ast::{Expression, Precedence},
    atoms::infix_operator,
    expression::parse_atom_expression,
};

pub fn parse_pratt_expression(
    input: Tokens,
    precedence: Precedence,
) -> IResult<Tokens, Expression> {
    let (rest, atom) = parse_atom_expression(input)?;
    parse_pratt_expression1(rest, precedence, atom)
}

fn parse_pratt_expression1(
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
                let (rest2, left2) = parse_pratt_call_expression(input, left)?;
                parse_pratt_expression1(rest2, precedence, left2)
            }
            (Precedence::PMemberAccess, _) if precedence < Precedence::PMemberAccess => {
                let (rest2, left2) = parse_pratt_member_access_expression(input, left)?;
                parse_pratt_expression1(rest2, precedence, left2)
            }
            (Precedence::PIndex, _) if precedence < Precedence::PIndex => {
                let (rest2, left2) = parse_pratt_index_expression(input, left)?;
                parse_pratt_expression1(rest2, precedence, left2)
            }
            (peek_precedence, _) if precedence < peek_precedence => {
                let (rest2, left2) = parse_infix_expression(input, left)?;
                parse_pratt_expression1(rest2, precedence, left2)
            }
            _ => Ok((input, left)),
        }
    }
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
                let (rest2, right) = parse_pratt_expression(rest, precedence)?;
                Ok((
                    rest2,
                    Expression::InfixExpression(InfixExpr {
                        op,
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    }),
                ))
            }
        }
    }
}

fn parse_pratt_member_access_expression(
    input: Tokens,
    left: Expression,
) -> IResult<Tokens, Expression> {
    map(tuple((period_tag, parse_identifier)), |(_, right)| {
        Expression::MemberAccessExpression(MemberAccessExpr {
            lhs: Box::new(left.clone()),
            ident: right,
        })
    })(input)
}

fn parse_pratt_call_expression(input: Tokens, left: Expression) -> IResult<Tokens, Expression> {
    map(
        delimited(l_paren_tag, opt(parse_expressions), r_paren_tag),
        |args| {
            Expression::CallExpression(CallExpr {
                lhs: Box::new(left.clone()),
                arguments: args.unwrap_or_default(),
            })
        },
    )(input)
}

fn parse_pratt_index_expression(input: Tokens, left: Expression) -> IResult<Tokens, Expression> {
    map(
        delimited(l_bracket_tag, parse_expression, r_bracket_tag),
        |index_expr| {
            Expression::IndexExpression(IndexExpr {
                lhs: Box::new(left.clone()),
                index: Box::new(index_expr),
            })
        },
    )(input)
}
