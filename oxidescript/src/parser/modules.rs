use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, separated_list1},
    sequence::delimited,
    IResult, Parser,
};

use crate::lexer::{token::Token, tokens::Tokens};

use super::{
    ast::{Module, Path, Statement, Use},
    colon_tag, extern_tag, l_squirly_tag, mod_tag, parse_identifier, r_squirly_tag, semicolon_tag,
    statement::parse_statement,
    use_tag,
};

pub fn parse_mod(input: Tokens) -> IResult<Tokens, Module> {
    map(
        (
            map(alt((mod_tag, extern_tag)), |t| match t.tokens[0] {
                Token::Mod => false,
                Token::Extern => true,
                _ => unreachable!(),
            }),
            parse_path,
            opt(delimited(
                l_squirly_tag,
                parse_module_content,
                r_squirly_tag,
            )),
        ),
        |(is_extern, path, content)| Module {
            path,
            content,
            is_extern,
        },
    )
    .parse(input)
}

fn parse_module_content(input: Tokens) -> IResult<Tokens, Vec<Statement>> {
    many0(parse_statement).parse(input)
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
