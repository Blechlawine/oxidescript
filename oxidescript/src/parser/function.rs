use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

use crate::lexer::tokens::Tokens;

use super::{
    ast::{Block, Expression, Parameter, Statement},
    atoms::{colon_tag, return_tag, semicolon_tag},
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

fn parse_return_expression(input: Tokens) -> IResult<Tokens, Expression> {
    map(
        delimited(return_tag, parse_expression, semicolon_tag),
        |expr| expr,
    )(input)
}

pub fn parse_block(input: Tokens) -> IResult<Tokens, Block> {
    map(
        tuple((
            many0(parse_statement),
            opt(alt((parse_return_expression, parse_expression))),
        )),
        |(mut statements, return_value)| {
            // Automatically select last expression statement as return value if no return value exists
            if return_value.is_none() {
                if let Some(last) = statements.last() {
                    if let Statement::ExpressionStatement {
                        expression: last,
                        has_semicolon: false,
                    } = last.clone()
                    {
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
