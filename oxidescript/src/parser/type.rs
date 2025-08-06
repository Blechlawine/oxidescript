use nom::{IResult, Parser, branch::alt, combinator::map};

use crate::lexer::tokens::Tokens;

use super::{ast::TypeExpression, modules::parse_path};

pub fn parse_type_expression<'t, 'src>(
    input: Tokens<'t, 'src>,
) -> IResult<Tokens<'t, 'src>, TypeExpression<'src>> {
    alt((parse_path_type_expression,)).parse(input)
}

fn parse_path_type_expression<'t, 'src>(
    input: Tokens<'t, 'src>,
) -> IResult<Tokens<'t, 'src>, TypeExpression<'src>> {
    map(parse_path, TypeExpression::Path).parse(input)
}
