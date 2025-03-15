use nom::{
    combinator::{map, opt},
    multi::separated_list1,
    IResult, Parser,
};

use crate::lexer::tokens::Tokens;

use super::{
    ast::{Module, Path, Use},
    colon_tag, l_squirly_tag, mod_tag, parse_identifier, parse_program, r_squirly_tag,
    semicolon_tag, use_tag,
};

pub fn parse_mod(input: Tokens) -> IResult<Tokens, Module> {
    map(
        (
            mod_tag,
            parse_path,
            opt((l_squirly_tag, parse_program, r_squirly_tag)),
        ),
        |(_, path, content)| Module {
            path,
            content: if let Some((_, content, _)) = content {
                Some(content)
            } else {
                None
            },
        },
    )
    .parse(input)
}

pub fn parse_use(input: Tokens) -> IResult<Tokens, Use> {
    map((use_tag, parse_path, semicolon_tag), |(_, path, _)| Use {
        path,
    })
    .parse(input)
}

pub fn parse_path(input: Tokens) -> IResult<Tokens, Path> {
    map(
        separated_list1((colon_tag, colon_tag), parse_identifier),
        |elements| Path { elements },
    )
    .parse(input)
}
