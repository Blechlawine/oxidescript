use crate::parser::Err;
use nom::error::ErrorKind;
use nom::error_position;
use nom::{bytes::complete::take, IResult};

use crate::lexer::tokens::Tokens;

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
            (Precedence::PIndex, _) if precedence < Precedence::PIndex => {
                // parse index expression
                todo!()
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
                    Expression::InfixExpression(op, Box::new(left), Box::new(right)),
                ))
            }
        }
    }
}
