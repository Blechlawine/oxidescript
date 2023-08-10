use nom::{
    combinator::{map, opt},
    multi::many0,
    sequence::tuple,
    IResult,
};

use crate::lexer::tokens::Tokens;

use super::{
    ast::{Block, Parameter},
    atoms::colon_tag,
    expression::parse_expression,
    parse_identifier,
    statement::parse_statement,
};

pub fn parse_parameter(input: Tokens) -> IResult<Tokens, Parameter> {
    map(
        tuple((parse_identifier, colon_tag, parse_identifier)),
        |(name, _, type_)| Parameter { name, type_ },
    )(input)
}

pub fn parse_block(input: Tokens) -> IResult<Tokens, Block> {
    map(
        tuple((many0(parse_statement), opt(parse_expression))),
        |(statements, return_value)| Block {
            statements,
            return_value,
        },
    )(input)
}
