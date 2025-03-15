use nom::{branch::alt, combinator::map, IResult, Parser};

use crate::lexer::tokens::Tokens;

use super::{ast::TypeExpression, parse_identifier};

pub fn parse_type_expression(input: Tokens) -> IResult<Tokens, TypeExpression> {
    alt((parse_identifier_type_expression,)).parse(input)
}

fn parse_identifier_type_expression(input: Tokens) -> IResult<Tokens, TypeExpression> {
    map(parse_identifier, TypeExpression::Ident).parse(input)
}
