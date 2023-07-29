use nom::{combinator::map, branch::alt, IResult, sequence::tuple};

use crate::lexer::tokens::Tokens;

use super::{ast::Declaration, atoms::*, expression::parse_expression, parse_identifier};

pub fn parse_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_declaration");
    alt((parse_let_declaration, parse_const_declaration))(input)
}

fn parse_const_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_const_declaration");
    map(
        tuple((
            const_tag,
            parse_identifier,
            assign_tag,
            parse_expression,
        )),
        |(_, name, _, expression)| Declaration::ConstDeclaration(name, expression),
    )(input)
}

fn parse_let_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_let_declaration");
    map(
        tuple((
            let_tag,
            parse_identifier,
            assign_tag,
            parse_expression,
        )),
        |(_, name, _, expression)| Declaration::LetDeclaration(name, expression),
    )(input)
}
