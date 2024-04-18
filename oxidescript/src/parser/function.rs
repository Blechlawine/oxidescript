use nom::{
    combinator::{map, opt},
    multi::many0,
    sequence::tuple,
    IResult,
};

use crate::lexer::tokens::Tokens;

use super::{
    ast::{Block, Parameter, Statement},
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
        |(mut statements, return_value)| {
            // Automatically select last expression statement as return value if no return value exists
            if return_value.is_none() {
                if let Some(last) = statements.last() {
                    if let Statement::ExpressionStatement(last) = last.clone() {
                        return Block {
                            statements: statements.drain(..statements.len() - 1).collect(),
                            return_value: Some(last),
                        };
                    }
                }
            }
            Block {
                statements,
                return_value,
            }
        },
    )(input)
}
