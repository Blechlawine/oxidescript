use nom::{
    branch::alt,
    combinator::map,
    multi::many0,
    sequence::{terminated, tuple},
    IResult,
};

use crate::lexer::tokens::Tokens;

use super::{
    ast::Declaration,
    atoms::*,
    expression::parse_expression,
    function::{parse_block, parse_parameter, parse_parameters},
    parse_identifier,
};

pub fn parse_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_declaration");
    alt((
        parse_let_declaration,
        parse_const_declaration,
        parse_function_declaration,
    ))(input)
}

fn parse_const_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_const_declaration");
    terminated(
        map(
            tuple((const_tag, parse_identifier, assign_tag, parse_expression)),
            |(_, name, _, expression)| {
                // dbg!(&name, &expression);
                Declaration::ConstDeclaration(name, expression)
            },
        ),
        semicolon_tag,
    )(input)
}

fn parse_let_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_let_declaration");
    terminated(
        map(
            tuple((let_tag, parse_identifier, assign_tag, parse_expression)),
            |(_, name, _, expression)| {
                // dbg!(&name, &expression);
                Declaration::LetDeclaration(name, expression)
            },
        ),
        semicolon_tag,
    )(input)
}

fn parse_function_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_function_declaration");
    map(
        tuple((
            function_tag,
            parse_identifier,
            l_paren_tag,
            parse_parameters,
            r_paren_tag,
            l_squirly_tag,
            parse_block,
            r_squirly_tag,
        )),
        |(_, name, _, parameters, _, _, body, _)| {
            // dbg!(&name, &parameters, &body);
            Declaration::FunctionDeclaration {
                name,
                parameters,
                body,
            }
        },
    )(input)
}
