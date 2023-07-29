use nom::{branch::alt, combinator::map, sequence::terminated, IResult};

use crate::lexer::tokens::Tokens;

use super::{
    ast::Statement, atoms::*, declaration::parse_declaration, expression::parse_expression,
};

pub fn parse_statement(input: Tokens) -> IResult<Tokens, Statement> {
    // println!("parse_statement");
    terminated(
        alt((parse_declaration_statement, parse_expression_statement)),
        semicolon_tag,
    )(input)
}

fn parse_declaration_statement(input: Tokens) -> IResult<Tokens, Statement> {
    // println!("parse_declaration_statement");
    map(parse_declaration, |declaration| {
        Statement::DeclarationStatement(declaration)
    })(input)
}

fn parse_expression_statement(input: Tokens) -> IResult<Tokens, Statement> {
    // println!("parse_expression_statement");
    map(parse_expression, |expression| {
        Statement::ExpressionStatement(expression)
    })(input)
}
