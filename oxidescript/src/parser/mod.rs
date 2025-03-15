pub mod ast;
pub mod atoms;
pub mod declaration;
pub mod expression;
pub mod function;
pub mod pratt_expression;
pub mod statement;
pub mod r#type;

use nom::bytes::complete::take;
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::Err;
use nom::Parser as _;
use nom::{sequence::terminated, IResult};

use crate::lexer::token::Token;
use crate::lexer::tokens::Tokens;

use self::ast::{Identifier, Literal, Number, NumberBase, Program};
use self::atoms::*;
use self::statement::parse_statement;

fn parse_literal(input: Tokens) -> IResult<Tokens, Literal> {
    let (rest, found) = take(1usize)(input)?;
    if found.tokens.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match found.tokens[0].clone() {
            Token::NumberLiteral(val) => {
                let parsed: Number = if val.starts_with("0x") {
                    Number::I {
                        base: NumberBase::Hex,
                        value: i32::from_str_radix(&val, 16).unwrap(),
                    }
                } else if val.starts_with("0b") {
                    Number::I {
                        base: NumberBase::Bin,
                        value: i32::from_str_radix(&val, 2).unwrap(),
                    }
                } else if val.starts_with("0o") {
                    Number::I {
                        base: NumberBase::Oct,
                        value: i32::from_str_radix(&val, 8).unwrap(),
                    }
                } else {
                    let int = val.parse::<i32>();
                    if let Ok(int) = int {
                        Number::I {
                            base: NumberBase::Dec,
                            value: int,
                        }
                    } else {
                        Number::F(val.parse().unwrap())
                    }
                };
                Ok((rest, Literal::NumberLiteral(parsed)))
            }
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
    terminated(many0(parse_statement), eof_tag).parse(input)
}

pub struct Parser;

impl Parser {
    pub fn parse(tokens: Tokens) -> IResult<Tokens, Program> {
        parse_program(tokens)
    }
}

#[cfg(test)]
mod tests {
    use ast::{
        CallExpr, ElseIfExpr, FunctionDecl, IfExpr, IndexExpr, InfixExpr, MemberAccessExpr,
        StructDecl, StructField, TypeExpression,
    };

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
                Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                    base: NumberBase::Dec,
                    value: 5,
                })),
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
                Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                    base: NumberBase::Dec,
                    value: 5,
                })),
            )),
            Statement::DeclarationStatement(Declaration::ConstDeclaration(
                Identifier("stuff".to_string()),
                Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                    base: NumberBase::Dec,
                    value: 12,
                })),
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
            Declaration::FunctionDeclaration(FunctionDecl {
                name: Identifier("test".to_string()),
                parameters: vec![],
                body: Block {
                    statements: vec![Statement::DeclarationStatement(
                        Declaration::LetDeclaration(
                            Identifier("variable".to_string()),
                            Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                                base: NumberBase::Dec,
                                value: 5,
                            })),
                        ),
                    )],
                    return_value: None,
                },
                return_type: None,
            }),
        )];

        assert_input_with_program(input, program);
    }

    #[test]
    fn infix_expression() {
        let input = "let foo = 5 - 10 * 2;".as_bytes();

        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::LetDeclaration(
                Identifier("foo".to_string()),
                Expression::InfixExpression(InfixExpr {
                    op: InfixOperator::Minus,
                    lhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                        Number::I {
                            base: NumberBase::Dec,
                            value: 5,
                        },
                    ))),
                    rhs: Box::new(Expression::InfixExpression(InfixExpr {
                        op: InfixOperator::Multiply,
                        lhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            Number::I {
                                base: NumberBase::Dec,
                                value: 10,
                            },
                        ))),
                        rhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            Number::I {
                                base: NumberBase::Dec,
                                value: 2,
                            },
                        ))),
                    })),
                }),
            ),
        )];

        assert_input_with_program(input, program);
    }

    #[test]
    fn function_implicit_return() {
        let input = "\
            fn test() -> Number {\
                5 - 10\
            }\
        "
        .as_bytes();
        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::FunctionDeclaration(FunctionDecl {
                name: Identifier("test".to_string()),
                parameters: vec![],
                body: Block {
                    statements: vec![],
                    return_value: Some(Expression::InfixExpression(InfixExpr {
                        op: InfixOperator::Minus,
                        lhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            Number::I {
                                base: NumberBase::Dec,
                                value: 5,
                            },
                        ))),
                        rhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            Number::I {
                                base: NumberBase::Dec,
                                value: 10,
                            },
                        ))),
                    })),
                },
                return_type: Some(TypeExpression::Ident(Identifier("Number".to_string()))),
            }),
        )];

        assert_input_with_program(input, program);
    }

    #[test]
    fn index_expression() {
        let input = r#"
        array[1];
        array[1 + 2];
        "#;

        let program: Program = vec![
            Statement::ExpressionStatement {
                expression: Expression::IndexExpression(IndexExpr {
                    lhs: Box::new(Expression::IdentifierExpression(Identifier(
                        "array".to_string(),
                    ))),
                    index: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                        Number::I {
                            base: NumberBase::Dec,
                            value: 1,
                        },
                    ))),
                }),
                has_semicolon: true,
            },
            Statement::ExpressionStatement {
                expression: Expression::IndexExpression(IndexExpr {
                    lhs: Box::new(Expression::IdentifierExpression(Identifier(
                        "array".to_string(),
                    ))),
                    index: Box::new(Expression::InfixExpression(InfixExpr {
                        op: InfixOperator::Plus,
                        lhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            Number::I {
                                base: NumberBase::Dec,
                                value: 1,
                            },
                        ))),
                        rhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            Number::I {
                                base: NumberBase::Dec,
                                value: 2,
                            },
                        ))),
                    })),
                }),
                has_semicolon: true,
            },
        ];

        assert_input_with_program(input.as_bytes(), program);
    }

    #[test]
    fn call_expression() {
        let input = "foo(20, 30 - 2);".as_bytes();

        let program: Program = vec![Statement::ExpressionStatement {
            expression: Expression::CallExpression(CallExpr {
                lhs: Box::new(Expression::IdentifierExpression(Identifier(
                    "foo".to_string(),
                ))),
                arguments: vec![
                    Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                        base: NumberBase::Dec,
                        value: 20,
                    })),
                    Expression::InfixExpression(InfixExpr {
                        op: InfixOperator::Minus,
                        lhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            Number::I {
                                base: NumberBase::Dec,
                                value: 30,
                            },
                        ))),
                        rhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                            Number::I {
                                base: NumberBase::Dec,
                                value: 2,
                            },
                        ))),
                    }),
                ],
            }),
            has_semicolon: true,
        }];

        assert_input_with_program(input, program);
    }

    #[test]
    fn block_expression() {
        let input = r#"
            {
                5 - 10
            }
        "#;

        let program: Program = vec![Statement::ExpressionStatement {
            expression: Expression::BlockExpression(Box::new(Block {
                statements: vec![],
                return_value: Some(Expression::InfixExpression(InfixExpr {
                    op: InfixOperator::Minus,
                    lhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                        Number::I {
                            base: NumberBase::Dec,
                            value: 5,
                        },
                    ))),
                    rhs: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                        Number::I {
                            base: NumberBase::Dec,
                            value: 10,
                        },
                    ))),
                })),
            })),
            has_semicolon: false,
        }];

        assert_input_with_program(input.as_bytes(), program);
    }

    #[test]
    fn if_expression() {
        let input = r#"
        if true {
            1
        } else if false {
            2;
        } else {
            3
        };
        "#;
        let program: Program = vec![Statement::ExpressionStatement {
            expression: Expression::IfExpression(IfExpr {
                condition: Box::new(Expression::LiteralExpression(Literal::BooleanLiteral(true))),
                then_block: Box::new(Block {
                    statements: vec![],
                    return_value: Some(Expression::LiteralExpression(Literal::NumberLiteral(
                        Number::I {
                            base: NumberBase::Dec,
                            value: 1,
                        },
                    ))),
                }),
                else_if_blocks: vec![ElseIfExpr {
                    condition: Box::new(Expression::LiteralExpression(Literal::BooleanLiteral(
                        false,
                    ))),
                    then_block: Block {
                        statements: vec![Statement::ExpressionStatement {
                            expression: Expression::LiteralExpression(Literal::NumberLiteral(
                                Number::I {
                                    base: NumberBase::Dec,
                                    value: 2,
                                },
                            )),
                            has_semicolon: true,
                        }],
                        return_value: None,
                    },
                }],
                else_block: Some(Box::new(Block {
                    statements: vec![],
                    return_value: Some(Expression::LiteralExpression(Literal::NumberLiteral(
                        Number::I {
                            base: NumberBase::Dec,
                            value: 3,
                        },
                    ))),
                })),
            }),
            has_semicolon: true,
        }];
        assert_input_with_program(input.as_bytes(), program);
    }

    #[test]
    fn expression_statement_mix() {
        let input = r#"
            fn test() {
                5 - 10 * 2 + foo(20, 30 - 2);
                let variable = 5;
                variable
            }
            test();
            console.log("Hello world");
            array[1];
        "#;

        let program: Program = vec![
            Statement::DeclarationStatement(Declaration::FunctionDeclaration(FunctionDecl {
                name: Identifier("test".to_string()),
                parameters: vec![],
                body: Block {
                    statements: vec![
                        Statement::ExpressionStatement {
                            expression: Expression::InfixExpression(InfixExpr {
                                op: InfixOperator::Plus,
                                lhs: Box::new(Expression::InfixExpression(InfixExpr {
                                    op: InfixOperator::Minus,
                                    lhs: Box::new(Expression::LiteralExpression(
                                        Literal::NumberLiteral(Number::I {
                                            base: NumberBase::Dec,
                                            value: 5,
                                        }),
                                    )),
                                    rhs: Box::new(Expression::InfixExpression(InfixExpr {
                                        op: InfixOperator::Multiply,
                                        lhs: Box::new(Expression::LiteralExpression(
                                            Literal::NumberLiteral(Number::I {
                                                base: NumberBase::Dec,
                                                value: 10,
                                            }),
                                        )),
                                        rhs: Box::new(Expression::LiteralExpression(
                                            Literal::NumberLiteral(Number::I {
                                                base: NumberBase::Dec,
                                                value: 2,
                                            }),
                                        )),
                                    })),
                                })),
                                rhs: Box::new(Expression::CallExpression(CallExpr {
                                    lhs: Box::new(Expression::IdentifierExpression(Identifier(
                                        "foo".to_string(),
                                    ))),
                                    arguments: vec![
                                        Expression::LiteralExpression(Literal::NumberLiteral(
                                            Number::I {
                                                base: NumberBase::Dec,
                                                value: 20,
                                            },
                                        )),
                                        Expression::InfixExpression(InfixExpr {
                                            op: InfixOperator::Minus,
                                            lhs: Box::new(Expression::LiteralExpression(
                                                Literal::NumberLiteral(Number::I {
                                                    base: NumberBase::Dec,
                                                    value: 30,
                                                }),
                                            )),
                                            rhs: Box::new(Expression::LiteralExpression(
                                                Literal::NumberLiteral(Number::I {
                                                    base: NumberBase::Dec,
                                                    value: 2,
                                                }),
                                            )),
                                        }),
                                    ],
                                })),
                            }),
                            has_semicolon: true,
                        },
                        Statement::DeclarationStatement(Declaration::LetDeclaration(
                            Identifier("variable".to_string()),
                            Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                                base: NumberBase::Dec,
                                value: 5,
                            })),
                        )),
                    ],
                    return_value: Some(Expression::IdentifierExpression(Identifier(
                        "variable".to_string(),
                    ))),
                },
                return_type: None,
            })),
            Statement::ExpressionStatement {
                expression: Expression::CallExpression(CallExpr {
                    lhs: Box::new(Expression::IdentifierExpression(Identifier(
                        "test".to_string(),
                    ))),
                    arguments: vec![],
                }),
                has_semicolon: true,
            },
            Statement::ExpressionStatement {
                expression: Expression::CallExpression(CallExpr {
                    lhs: Box::new(Expression::MemberAccessExpression(MemberAccessExpr {
                        lhs: Box::new(Expression::IdentifierExpression(Identifier(
                            "console".into(),
                        ))),
                        ident: Identifier("log".into()),
                    })),
                    arguments: vec![Expression::LiteralExpression(Literal::StringLiteral(
                        "Hello world".into(),
                    ))],
                }),
                has_semicolon: true,
            },
            Statement::ExpressionStatement {
                expression: Expression::IndexExpression(IndexExpr {
                    lhs: Box::new(Expression::IdentifierExpression(Identifier(
                        "array".to_string(),
                    ))),
                    index: Box::new(Expression::LiteralExpression(Literal::NumberLiteral(
                        Number::I {
                            base: NumberBase::Dec,
                            value: 1,
                        },
                    ))),
                }),
                has_semicolon: true,
            },
        ];

        assert_input_with_program(input.as_bytes(), program);
    }

    #[test]
    fn struct_declaration() {
        let input = r#"struct Test {
            foo: String,
            bar: Number,
        }"#;
        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::StructDeclaration(StructDecl {
                ident: Identifier("Test".to_string()),
                fields: vec![
                    StructField {
                        ident: Identifier("foo".to_string()),
                        r#type: TypeExpression::Ident(Identifier("String".to_string())),
                    },
                    StructField {
                        ident: Identifier("bar".to_string()),
                        r#type: TypeExpression::Ident(Identifier("Number".to_string())),
                    },
                ],
            }),
        )];
        assert_input_with_program(input.as_bytes(), program);
    }
}
