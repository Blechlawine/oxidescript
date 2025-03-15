use nom::{
    branch::alt,
    combinator::{map, opt},
    IResult, Parser,
};

use crate::lexer::tokens::Tokens;

use super::{
    ast::Statement, atoms::semicolon_tag, declaration::parse_declaration,
    expression::parse_expression,
};

pub fn parse_statement(input: Tokens) -> IResult<Tokens, Statement> {
    // println!("parse_statement");
    alt((parse_declaration_statement, parse_expression_statement)).parse(input)
}

fn parse_declaration_statement(input: Tokens) -> IResult<Tokens, Statement> {
    // println!("parse_declaration_statement");
    map(parse_declaration, |declaration| {
        Statement::DeclarationStatement(declaration)
    })
    .parse(input)
}

// TODO: do we have expression statements in this language?
fn parse_expression_statement(input: Tokens) -> IResult<Tokens, Statement> {
    // println!("parse_expression_statement");
    map(
        (parse_expression, opt(semicolon_tag)),
        |(expression, semicolon)| Statement::ExpressionStatement {
            expression,
            has_semicolon: semicolon.is_some(),
        },
    )
    .parse(input)
}
