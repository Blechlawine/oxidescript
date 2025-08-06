use nom::{
    IResult, Parser,
    branch::alt,
    combinator::{map, opt},
    multi::{many0, separated_list1},
    sequence::delimited,
};

use crate::lexer::{token::Token, tokens::Tokens};

use super::{
    ast::{ModuleDeclaration, Path, Statement, Use},
    colon_tag, extern_tag, l_squirly_tag, mod_tag, parse_identifier_reference, r_squirly_tag,
    semicolon_tag,
    statement::parse_statement,
    use_tag,
};

pub fn parse_mod<'t, 'src>(
    input: Tokens<'t, 'src>,
) -> IResult<Tokens<'t, 'src>, ModuleDeclaration<'src>> {
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

fn parse_module_content<'t, 'src>(
    input: Tokens<'t, 'src>,
) -> IResult<Tokens<'t, 'src>, Vec<Statement<'src>>> {
    many0(parse_statement).parse(input)
}

pub fn parse_use<'t, 'src>(input: Tokens<'t, 'src>) -> IResult<Tokens<'t, 'src>, Use<'src>> {
    map((use_tag, parse_path, semicolon_tag), |(_, path, _)| Use {
        path,
        resolved_module: None,
        imported: None,
    })
    .parse(input)
}

pub fn parse_path<'t, 'src>(input: Tokens<'t, 'src>) -> IResult<Tokens<'t, 'src>, Path<'src>> {
    map(
        separated_list1((colon_tag, colon_tag), parse_identifier_reference),
        |elements| Path {
            elements,
            full_path: None,
        },
    )
    .parse(input)
}
