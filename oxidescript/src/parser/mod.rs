pub mod ast;
pub mod atoms;
pub mod declaration;
pub mod expression;
pub mod function;
pub mod statement;

use nom::bytes::complete::take;
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::Err;
use nom::{sequence::terminated, IResult};

use crate::lexer::token::Token;
use crate::lexer::tokens::Tokens;

use self::ast::{Identifier, Literal, Program};
use self::atoms::*;
use self::statement::parse_statement;

fn parse_literal(input: Tokens) -> IResult<Tokens, Literal> {
    let (rest, found) = take(1usize)(input)?;
    if found.tokens.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match found.tokens[0].clone() {
            Token::NumberLiteral(val) => Ok((rest, Literal::NumberLiteral(val))),
            Token::StringLiteral(val) => Ok((rest, Literal::StringLiteral(val))),
            Token::BooleanLiteral(val) => Ok((rest, Literal::BooleanLiteral(val))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_identifier(input: Tokens) -> IResult<Tokens, Identifier> {
    let (rest, found) = take(1usize)(input)?;
    // dbg!(rest, found, input);
    if found.tokens.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match found.tokens[0].clone() {
            Token::Ident(name) => Ok((rest, Identifier(name))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_program(input: Tokens) -> IResult<Tokens, Program> {
    // println!("parse_program");
    terminated(many0(parse_statement), eof_tag)(input)
}

pub struct Parser;

impl Parser {
    pub fn parse(tokens: Tokens) -> IResult<Tokens, Program> {
        parse_program(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::{
        ast::{Block, Declaration, Expression, InfixOperator, Statement},
        *,
    };
    use crate::lexer::*;

    fn assert_input_with_program(input: &[u8], expected_results: Program) {
        let (_, r) = Lexer::lex_tokens(input).unwrap();
        let tokens = Tokens::new(&r);
        let (_, result) = Parser::parse(tokens).unwrap();
        assert_eq!(expected_results, result);
    }

    #[test]
    fn empty() {
        assert_input_with_program(b"", vec![]);
    }

    #[test]
    fn declaration_statement() {
        let input = "
            let test = 5;\
        "
        .as_bytes();
        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::LetDeclaration(
                Identifier("test".to_string()),
                Expression::LiteralExpression(Literal::NumberLiteral("5".to_string())),
            ),
        )];
        assert_input_with_program(input, program);
    }

    #[test]
    fn declaration_statements() {
        let input = "
            let test = 5;\
            const stuff = 12;\
            let things = true;\
            const foo = \"bar\";\
        "
        .as_bytes();

        let program: Program = vec![
            Statement::DeclarationStatement(Declaration::LetDeclaration(
                Identifier("test".to_string()),
                Expression::LiteralExpression(Literal::NumberLiteral("5".to_string())),
            )),
            Statement::DeclarationStatement(Declaration::ConstDeclaration(
                Identifier("stuff".to_string()),
                Expression::LiteralExpression(Literal::NumberLiteral("12".to_string())),
            )),
            Statement::DeclarationStatement(Declaration::LetDeclaration(
                Identifier("things".to_string()),
                Expression::LiteralExpression(Literal::BooleanLiteral(true)),
            )),
            Statement::DeclarationStatement(Declaration::ConstDeclaration(
                Identifier("foo".to_string()),
                Expression::LiteralExpression(Literal::StringLiteral("bar".to_string())),
            )),
        ];

        assert_input_with_program(input, program);
    }

    #[test]
    fn function_declaration() {
        let input = "
            fn test() {\
                let variable = 5;\
            }\
        "
        .as_bytes();

        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::FunctionDeclaration {
                name: Identifier("test".to_string()),
                parameters: vec![],
                body: Block {
                    statements: vec![Statement::DeclarationStatement(
                        Declaration::LetDeclaration(
                            Identifier("variable".to_string()),
                            Expression::LiteralExpression(Literal::NumberLiteral("5".to_string())),
                        ),
                    )],
                    return_value: None,
                },
            },
        )];

        assert_input_with_program(input, program);
    }

    #[test]
    fn infix_expression() {
        let input = "let foo = 5 - 10 * 2;".as_bytes();

        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::LetDeclaration(
                Identifier("foo".to_string()),
                Expression::InfixExpression(
                    InfixOperator::Minus,
                    Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                        "5".into(),
                    ))),
                    Box::new(Expression::InfixExpression(
                        InfixOperator::Multiply,
                        Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            "10".into(),
                        ))),
                        Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            "2".into(),
                        ))),
                    )),
                ),
            ),
        )];

        assert_input_with_program(input, program);
    }

    #[test]
    fn function_implicit_return() {
        let input = "\
            fn test() {\
                5 - 10\
            }\
        "
        .as_bytes();
        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::FunctionDeclaration {
                name: Identifier("test".to_string()),
                parameters: vec![],
                body: Block {
                    statements: vec![],
                    return_value: Some(Expression::InfixExpression(
                        InfixOperator::Minus,
                        Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            "5".into(),
                        ))),
                        Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            "10".into(),
                        ))),
                    )),
                },
            },
        )];

        assert_input_with_program(input, program);
    }
}
