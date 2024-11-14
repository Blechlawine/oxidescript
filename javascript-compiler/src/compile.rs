use std::cell::Cell;

use oxc::{
    ast::{
        ast::{
            BindingIdentifier, BindingPattern, Expression, FormalParameter, FormalParameters,
            FunctionBody, IdentifierReference, Program, Statement, VariableDeclarator,
        },
        AstBuilder,
    },
    span::{SourceType, Span},
    syntax::{number::NumberBase, reference::ReferenceFlag},
};

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, Program<'c>> for oxidescript::parser::ast::Program {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Program {
        AstBuilder::new(ctx.allocator).program(
            Span::new(0, 0),
            SourceType::default(),
            oxc::allocator::Vec::new_in(ctx.allocator),
            None,
            oxc::allocator::Vec::from_iter_in(
                self.into_iter().map(|statement| statement.into_oxc(ctx)),
                ctx.allocator,
            ),
        )
    }
}

impl<'c> IntoOxc<'c, Statement<'c>> for oxidescript::parser::ast::Statement {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Statement {
        match self {
            oxidescript::parser::ast::Statement::ExpressionStatement { expression, .. } => {
                AstBuilder::new(ctx.allocator)
                    .expression_statement(Span::new(0, 0), expression.into_oxc(ctx))
            }
            oxidescript::parser::ast::Statement::DeclarationStatement(declaration) => {
                match declaration {
                    oxidescript::parser::ast::Declaration::ConstDeclaration(ident, expr) => {
                        oxc::ast::ast::Statement::VariableDeclaration(
                            AstBuilder::new(ctx.allocator).variable_declaration(
                                Span::new(0, 0),
                                oxc::ast::ast::VariableDeclarationKind::Const,
                                oxc::allocator::Vec::from_iter_in(
                                    vec![VariableDeclarator {
                                        span: Span::new(0, 0),
                                        kind: oxc::ast::ast::VariableDeclarationKind::Const,
                                        id: ident.into_oxc(ctx),
                                        init: Some(expr.into_oxc(ctx)),
                                        definite: false,
                                    }],
                                    ctx.allocator,
                                ),
                                false,
                            ),
                        )
                    }
                    oxidescript::parser::ast::Declaration::LetDeclaration(ident, expr) => {
                        oxc::ast::ast::Statement::VariableDeclaration(
                            AstBuilder::new(ctx.allocator).variable_declaration(
                                Span::new(0, 0),
                                oxc::ast::ast::VariableDeclarationKind::Let,
                                oxc::allocator::Vec::from_iter_in(
                                    vec![VariableDeclarator {
                                        span: Span::new(0, 0),
                                        kind: oxc::ast::ast::VariableDeclarationKind::Let,
                                        id: ident.into_oxc(ctx),
                                        init: Some(expr.into_oxc(ctx)),
                                        definite: false,
                                    }],
                                    ctx.allocator,
                                ),
                                false,
                            ),
                        )
                    }
                    oxidescript::parser::ast::Declaration::FunctionDeclaration {
                        name,
                        parameters,
                        body,
                    } => {
                        oxc::ast::ast::Statement::FunctionDeclaration(oxc::allocator::Box::new_in(
                            oxc::ast::ast::Function {
                                r#type: oxc::ast::ast::FunctionType::FunctionDeclaration,
                                span: Span::new(0, 0),
                                id: Some(name.into_oxc(ctx)),
                                generator: false,
                                r#async: false,
                                declare: false,
                                type_parameters: None,
                                this_param: None,
                                params: oxc::allocator::Box::new_in(
                                    parameters.into_oxc(ctx),
                                    ctx.allocator,
                                ),
                                body: Some(oxc::allocator::Box::new_in(
                                    body.into_oxc(ctx),
                                    ctx.allocator,
                                )),
                                return_type: None,
                                scope_id: None.into(),
                            },
                            ctx.allocator,
                        ))
                    }
                }
            }
        }
    }
}

impl<'c> IntoOxc<'c, FunctionBody<'c>> for oxidescript::parser::ast::Block {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> FunctionBody<'c> {
        FunctionBody {
            span: Span::new(0, 0),
            directives: oxc::allocator::Vec::new_in(ctx.allocator),
            statements: oxc::allocator::Vec::from_iter_in(
                self.statements
                    .into_iter()
                    .map(|statement| statement.into_oxc(ctx)),
                ctx.allocator,
            ),
        }
    }
}

impl<'c> IntoOxc<'c, BindingPattern<'c>> for oxidescript::parser::ast::Identifier {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> BindingPattern<'c> {
        BindingPattern {
            kind: oxc::ast::ast::BindingPatternKind::BindingIdentifier(
                oxc::allocator::Box::new_in(self.into_oxc(ctx), ctx.allocator),
            ),
            type_annotation: None,
            optional: false,
        }
    }
}

impl<'c> IntoOxc<'c, BindingIdentifier<'c>> for oxidescript::parser::ast::Identifier {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> BindingIdentifier<'c> {
        let name = AstBuilder::new(ctx.allocator).new_atom(&self.0.clone());
        BindingIdentifier {
            span: Span::new(0, 0),
            name,
            symbol_id: None.into(),
        }
    }
}

impl<'c> IntoOxc<'c, FormalParameters<'c>> for Vec<oxidescript::parser::ast::Parameter> {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> FormalParameters<'c> {
        FormalParameters {
            span: Span::new(0, 0),
            kind: oxc::ast::ast::FormalParameterKind::FormalParameter,
            items: oxc::allocator::Vec::from_iter_in(
                self.into_iter().map(|param| param.into_oxc(ctx)),
                ctx.allocator,
            ),
            rest: None,
        }
    }
}

impl<'c> IntoOxc<'c, FormalParameter<'c>> for oxidescript::parser::ast::Parameter {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> FormalParameter<'c> {
        FormalParameter {
            span: Span::new(0, 0),
            pattern: self.name.into_oxc(ctx),
            accessibility: None,
            readonly: false,
            r#override: false,
            decorators: oxc::allocator::Vec::new_in(ctx.allocator),
        }
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Expression {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        match self {
            oxidescript::parser::ast::Expression::IdentifierExpression(ident) => {
                let name = AstBuilder::new(ctx.allocator).new_atom(&ident.0.clone());
                AstBuilder::new(ctx.allocator).identifier_reference_expression(
                    IdentifierReference {
                        span: Span::new(0, 0),
                        name,
                        reference_id: Cell::new(None),
                        reference_flag: ReferenceFlag::all(),
                    },
                )
            }
            oxidescript::parser::ast::Expression::LiteralExpression(literal) => match literal {
                oxidescript::parser::ast::Literal::StringLiteral(s) => {
                    AstBuilder::new(ctx.allocator).literal_string_expression(
                        AstBuilder::new(ctx.allocator).string_literal(Span::new(0, 0), &s),
                    )
                }
                oxidescript::parser::ast::Literal::NumberLiteral(n) => {
                    let s = AstBuilder::new(ctx.allocator).new_str(n.to_string().as_str());
                    AstBuilder::new(ctx.allocator).literal_number_expression(
                        AstBuilder::new(ctx.allocator).number_literal(
                            Span::new(0, 0),
                            n.try_into().unwrap(),
                            s,
                            NumberBase::Decimal,
                        ),
                    )
                }
                oxidescript::parser::ast::Literal::BooleanLiteral(_) => todo!(),
            },
            oxidescript::parser::ast::Expression::UnaryExpression(_, _) => todo!(),
            oxidescript::parser::ast::Expression::InfixExpression(_, _, _) => todo!(),
            oxidescript::parser::ast::Expression::ArrayExpression(_) => todo!(),
            oxidescript::parser::ast::Expression::IfExpression {
                condition,
                then_block,
                else_if_blocks,
                else_block,
            } => todo!(),
            oxidescript::parser::ast::Expression::BlockExpression(_) => todo!(),
            oxidescript::parser::ast::Expression::CallExpression(_, _) => todo!(),
            oxidescript::parser::ast::Expression::IndexExpression(_, _) => todo!(),
            oxidescript::parser::ast::Expression::MemberAccessExpression(_, _) => todo!(),
        }
    }
}
