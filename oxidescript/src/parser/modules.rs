use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, separated_list1},
    sequence::delimited,
    IResult, Parser,
};

use crate::lexer::{token::Token, tokens::Tokens};

use super::{
    ast::{ModuleDeclaration, Path, Statement, Use},
    colon_tag, extern_tag, l_squirly_tag, mod_tag, parse_identifier, parse_identifier_reference,
    r_squirly_tag, semicolon_tag,
    statement::parse_statement,
    use_tag,
};

pub fn parse_mod(input: Tokens) -> IResult<Tokens, ModuleDeclaration> {
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
        |(is_extern, path, content)| {
            if let Some(content) = content {
                if is_extern {
                    ModuleDeclaration::Extern { path, content }
                } else {
                    ModuleDeclaration::Intern {
                        path,
                        content: Some(content),
                    }
                }
            } else if is_extern {
                panic!("Extern module needs content");
            } else {
                ModuleDeclaration::Intern {
                    path,
                    content: None,
                }
            }
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
        resolved_module: None,
        imported: None,
    })
    .parse(input)
}

pub fn parse_path(input: Tokens) -> IResult<Tokens, Path> {
    map(
        separated_list1((colon_tag, colon_tag), parse_identifier_reference),
        |elements| Path {
            elements,
            full_path: None,
        },
    )
    .parse(input)
}
