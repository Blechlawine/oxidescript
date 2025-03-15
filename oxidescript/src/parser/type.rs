use nom::{branch::alt, combinator::map, IResult, Parser};

use crate::lexer::tokens::Tokens;

use super::{ast::TypeExpression, modules::parse_path};

pub fn parse_type_expression(input: Tokens) -> IResult<Tokens, TypeExpression> {
    alt((parse_path_type_expression,)).parse(input)
}

fn parse_path_type_expression(input: Tokens) -> IResult<Tokens, TypeExpression> {
    map(parse_path, TypeExpression::Path).parse(input)
}
