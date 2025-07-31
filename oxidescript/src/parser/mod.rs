pub mod ast;
pub mod atoms;
pub mod declaration;
pub mod expression;
pub mod function;
pub mod modules;
pub mod pratt_expression;
pub mod statement;
pub mod r#type;

use std::collections::HashMap;

use ast::IdentifierReference;
use ast::Path;
use nom::Err;
use nom::Parser as _;
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::{IResult, sequence::terminated};

use crate::lexer::token::Token;
use crate::lexer::tokens::Tokens;
use crate::loader::LexedSourceTree;
use crate::loader::ParsedSourceTree;

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
            Token::StringLiteral(val) => Ok((rest, Literal::StringLiteral(val.to_string()))),
            Token::BooleanLiteral(val) => Ok((rest, Literal::BooleanLiteral(val))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_ident_name(input: Tokens) -> IResult<Tokens, &str> {
    let (rest, found) = take(1usize)(input)?;
    // dbg!(rest, found, input);
    if found.tokens.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match found.tokens[0].clone() {
            Token::Ident(name) => Ok((rest, name)),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_identifier(input: Tokens) -> IResult<Tokens, Identifier> {
    map(parse_ident_name, |name| Identifier { name, id: None }).parse(input)
}

fn parse_identifier_reference(input: Tokens) -> IResult<Tokens, IdentifierReference> {
    map(parse_ident_name, |name| IdentifierReference {
        name,
        id: None,
    })
    .parse(input)
}

fn parse_program(input: Tokens) -> IResult<Tokens, Program> {
    // println!("parse_program");
    terminated(many0(parse_statement), eof_tag).parse(input)
}

pub struct Parser;

impl Parser {
    pub fn parse_tree<'src>(tree: LexedSourceTree<'src>) -> ParsedSourceTree<'src> {
        let mut output = HashMap::new();
        for (path, source) in tree {
            let tokens = Tokens::new(&source);
            let path = path
                .into_iter()
                .map(|p| {
                    let p = Tokens::new(&[p]);
                    let (_, parsed) = parse_identifier_reference(p).unwrap();
                    parsed
                })
                .collect::<Vec<_>>();
            let (_rest, parsed) = Parser::parse(tokens).unwrap();
            output.insert(Path::new(path), parsed);
        }
        output
    }

    pub fn parse(tokens: Tokens) -> IResult<Tokens, Program> {
        parse_program(tokens)
    }
}

#[cfg(test)]
mod tests {
    use ast::{
        CallExpr, ElseIfExpr, FunctionDecl, IdentifierReference, IfExpr, IndexExpr, InfixExpr,
        MemberAccessExpr, Path, StructDecl, StructField, TypeExpression,
    };

    use super::{
        ast::{Block, Declaration, Expression, InfixOperator, Statement},
        *,
    };
    use crate::lexer::*;

    fn assert_input_with_program(input: &str, expected_results: Program) {
        let input = Span::new(input);
        let (_, r) = Lexer::lex_tokens(input).unwrap();
        let tokens = Tokens::new(&r);
        let (_, result) = Parser::parse(tokens).unwrap();
        assert_eq!(expected_results, result);
    }

    #[test]
    fn empty() {
        assert_input_with_program("", vec![]);
    }

    #[test]
    fn declaration_statement() {
        let input = "
            let test = 5;\
        ";
        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::LetDeclaration(
                Identifier {
                    name: "test",
                    id: None,
                },
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
        ";

        let program: Program = vec![
            Statement::DeclarationStatement(Declaration::LetDeclaration(
                Identifier {
                    name: "test",
                    id: None,
                },
                Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                    base: NumberBase::Dec,
                    value: 5,
                })),
            )),
            Statement::DeclarationStatement(Declaration::ConstDeclaration(
                Identifier {
                    name: "stuff",
                    id: None,
                },
                Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                    base: NumberBase::Dec,
                    value: 12,
                })),
            )),
            Statement::DeclarationStatement(Declaration::LetDeclaration(
                Identifier {
                    name: "things",
                    id: None,
                },
                Expression::LiteralExpression(Literal::BooleanLiteral(true)),
            )),
            Statement::DeclarationStatement(Declaration::ConstDeclaration(
                Identifier {
                    name: "foo",
                    id: None,
                },
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
        ";

        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::FunctionDeclaration(FunctionDecl {
                name: Identifier {
                    name: "test",
                    id: None,
                },
                parameters: vec![],
                body: Some(Block {
                    statements: vec![Statement::DeclarationStatement(
                        Declaration::LetDeclaration(
                            Identifier {
                                name: "variable",
                                id: None,
                            },
                            Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                                base: NumberBase::Dec,
                                value: 5,
                            })),
                        ),
                    )],
                    return_value: None,
                }),
                has_body: true,
                return_type: None,
            }),
        )];

        assert_input_with_program(input, program);
    }

    #[test]
    fn infix_expression() {
        let input = "let foo = 5 - 10 * 2;";

        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::LetDeclaration(
                Identifier {
                    name: "foo",
                    id: None,
                },
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
        ";
        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::FunctionDeclaration(FunctionDecl {
                name: Identifier {
                    name: "test",
                    id: None,
                },
                parameters: vec![],
                body: Some(Block {
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
                }),
                return_type: Some(TypeExpression::Path(Path::from(IdentifierReference {
                    name: "Number",
                    id: None,
                }))),
                has_body: true,
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
                    lhs: Box::new(Expression::PathExpression(Path::from(
                        IdentifierReference {
                            name: "array",
                            id: None,
                        },
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
                    lhs: Box::new(Expression::PathExpression(Path::from(
                        IdentifierReference {
                            name: "array",
                            id: None,
                        },
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

        assert_input_with_program(input, program);
    }

    #[test]
    fn call_expression() {
        let input = "foo(20, 30 - 2);";

        let program: Program = vec![Statement::ExpressionStatement {
            expression: Expression::CallExpression(CallExpr {
                lhs: Box::new(Expression::PathExpression(Path::from(
                    IdentifierReference {
                        name: "foo",
                        id: None,
                    },
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

        assert_input_with_program(input, program);
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
        assert_input_with_program(input, program);
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
                name: Identifier {
                    name: "test",
                    id: None,
                },
                parameters: vec![],
                has_body: true,
                body: Some(Block {
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
                                    lhs: Box::new(Expression::PathExpression(Path::from(
                                        IdentifierReference {
                                            name: "foo",
                                            id: None,
                                        },
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
                            Identifier {
                                name: "variable",
                                id: None,
                            },
                            Expression::LiteralExpression(Literal::NumberLiteral(Number::I {
                                base: NumberBase::Dec,
                                value: 5,
                            })),
                        )),
                    ],
                    return_value: Some(Expression::PathExpression(Path::from(
                        IdentifierReference {
                            name: "variable",
                            id: None,
                        },
                    ))),
                }),
                return_type: None,
            })),
            Statement::ExpressionStatement {
                expression: Expression::CallExpression(CallExpr {
                    lhs: Box::new(Expression::PathExpression(Path::from(
                        IdentifierReference {
                            name: "test",
                            id: None,
                        },
                    ))),
                    arguments: vec![],
                }),
                has_semicolon: true,
            },
            Statement::ExpressionStatement {
                expression: Expression::CallExpression(CallExpr {
                    lhs: Box::new(Expression::MemberAccessExpression(MemberAccessExpr {
                        lhs: Box::new(Expression::PathExpression(Path::from(
                            IdentifierReference {
                                name: "console".into(),
                                id: None,
                            },
                        ))),
                        ident: Identifier {
                            name: "log".into(),
                            id: None,
                        },
                    })),
                    arguments: vec![Expression::LiteralExpression(Literal::StringLiteral(
                        "Hello world".into(),
                    ))],
                }),
                has_semicolon: true,
            },
            Statement::ExpressionStatement {
                expression: Expression::IndexExpression(IndexExpr {
                    lhs: Box::new(Expression::PathExpression(Path::from(
                        IdentifierReference {
                            name: "array",
                            id: None,
                        },
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

        assert_input_with_program(input, program);
    }

    #[test]
    fn struct_declaration() {
        let input = r#"struct Test {
            foo: String,
            bar: Number,
        }"#;
        let program: Program = vec![Statement::DeclarationStatement(
            Declaration::StructDeclaration(StructDecl {
                ident: Identifier {
                    name: "Test",
                    id: None,
                },
                fields: vec![
                    StructField {
                        ident: Identifier {
                            name: "foo",
                            id: None,
                        },
                        r#type: TypeExpression::Path(Path::from(IdentifierReference {
                            name: "String",
                            id: None,
                        })),
                    },
                    StructField {
                        ident: Identifier {
                            name: "bar",
                            id: None,
                        },
                        r#type: TypeExpression::Path(Path::from(IdentifierReference {
                            name: "Number",
                            id: None,
                        })),
                    },
                ],
            }),
        )];
        assert_input_with_program(input, program);
    }
}
