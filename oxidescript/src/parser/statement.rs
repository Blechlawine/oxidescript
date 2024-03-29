use nom::{branch::alt, combinator::map, IResult};

use crate::lexer::tokens::Tokens;

use super::{ast::Statement, declaration::parse_declaration, expression::parse_expression};

pub fn parse_statement(input: Tokens) -> IResult<Tokens, Statement> {
    // println!("parse_statement");
    alt((parse_declaration_statement, parse_expression_statement))(input)
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
