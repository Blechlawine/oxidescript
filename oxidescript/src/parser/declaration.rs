use nom::{
    branch::alt,
    combinator::map,
    multi::many0,
    sequence::{terminated, tuple},
    IResult, Parser,
};

use crate::lexer::tokens::Tokens;

use super::{
    ast::{Declaration, FunctionDecl, StructDecl, StructField},
    atoms::*,
    expression::parse_expression,
    function::{parse_block, parse_parameters},
    parse_identifier,
    r#type::parse_type_expression,
};

pub fn parse_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_declaration");
    alt((
        parse_let_declaration,
        parse_const_declaration,
        parse_function_declaration,
        parse_struct_declaration,
    ))
    .parse(input)
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
    )
    .parse(input)
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
    )
    .parse(input)
}

fn parse_function_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    // println!("parse_function_declaration");
    map(
        (
            function_tag,
            parse_identifier,
            l_paren_tag,
            parse_parameters,
            r_paren_tag,
            l_squirly_tag,
            parse_block,
            r_squirly_tag,
        ),
        |(_, name, _, parameters, _, _, body, _)| {
            // dbg!(&name, &parameters, &body);
            Declaration::FunctionDeclaration(FunctionDecl {
                name,
                parameters,
                body,
            })
        },
    )
    .parse(input)
}

fn parse_struct_declaration(input: Tokens) -> IResult<Tokens, Declaration> {
    map(
        (
            struct_tag,
            parse_identifier,
            l_squirly_tag,
            many0(parse_struct_field),
            r_squirly_tag,
        ),
        |(_, ident, _, fields, _)| Declaration::StructDeclaration(StructDecl { ident, fields }),
    )
    .parse(input)
}

fn parse_struct_field(input: Tokens) -> IResult<Tokens, StructField> {
    map(
        (
            parse_identifier,
            colon_tag,
            parse_type_expression,
            comma_tag,
        ),
        |(ident, _, r#type, _)| StructField { ident, r#type },
    )
    .parse(input)
}
