use oxc::{
    ast::{
        ast::{Argument, ArrayExpressionElement, Expression, Program, Statement},
        AstBuilder,
    },
    span::{SourceType, Span},
};

pub mod block;
pub mod conditional;
pub mod function;
pub mod ident;
pub mod index;
pub mod infix;
pub mod literal;
pub mod r#loop;
pub mod member_access;
pub mod unary;

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, Program<'c>> for oxidescript::parser::ast::Program {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Program<'c> {
        AstBuilder::new(ctx.allocator).program(
            Span::new(0, 0),
            SourceType::default(),
            "",
            oxc::allocator::Vec::new_in(ctx.allocator),
            None,
            oxc::allocator::Vec::new_in(ctx.allocator),
            oxc::allocator::Vec::from_iter_in(
                self.into_iter().map(|statement| statement.into_oxc(ctx)),
                ctx.allocator,
            ),
        )
    }
}

impl<'c> IntoOxc<'c, Statement<'c>> for oxidescript::parser::ast::Statement {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Statement<'c> {
        match self {
            oxidescript::parser::ast::Statement::ExpressionStatement { expression, .. } => {
                match expression {
                    oxidescript::parser::ast::Expression::IfExpression(if_expr) => {
                        if_expr.into_oxc(ctx)
                    }
                    oxidescript::parser::ast::Expression::ForExpression(for_expr) => {
                        for_expr.into_oxc(ctx)
                    }
                    oxidescript::parser::ast::Expression::BlockExpression(block) => {
                        block.into_oxc(ctx)
                    }
                    _ => ctx.statement_expr(expression.into_oxc(ctx)),
                }
            }
            oxidescript::parser::ast::Statement::DeclarationStatement(declaration) => {
                match declaration {
                    oxidescript::parser::ast::Declaration::ConstDeclaration(ident, expr) => {
                        oxc::ast::ast::Statement::VariableDeclaration(oxc::allocator::Box::new_in(
                            ctx.r#const(ident.into_oxc(ctx), Some(expr.into_oxc(ctx))),
                            ctx.allocator,
                        ))
                    }
                    oxidescript::parser::ast::Declaration::LetDeclaration(ident, expr) => {
                        oxc::ast::ast::Statement::VariableDeclaration(oxc::allocator::Box::new_in(
                            ctx.r#let(ident.into_oxc(ctx), Some(expr.into_oxc(ctx))),
                            ctx.allocator,
                        ))
                    }
                    oxidescript::parser::ast::Declaration::FunctionDeclaration {
                        name,
                        parameters,
                        body,
                    } => oxc::ast::ast::Statement::FunctionDeclaration(ctx.r#box(ctx.function(
                        name.into_oxc(ctx),
                        parameters.into_oxc(ctx),
                        body.into_oxc(ctx),
                    ))),
                    oxidescript::parser::ast::Declaration::StructDeclaration { .. } => {
                        // this is just a type, it doesn't compile to anything
                        oxc::ast::ast::Statement::EmptyStatement(ctx.r#box(
                            oxc::ast::ast::EmptyStatement {
                                span: Span::new(0, 0),
                            },
                        ))
                    }
                }
            }
        }
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Expression {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        match self {
            oxidescript::parser::ast::Expression::IdentifierExpression(ident) => {
                ident.into_oxc(ctx)
            }
            oxidescript::parser::ast::Expression::LiteralExpression(literal) => {
                literal.into_oxc(ctx)
            }
            oxidescript::parser::ast::Expression::UnaryExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::InfixExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::ArrayExpression(exprs) => AstBuilder::new(
                ctx.allocator,
            )
            .expression_array(Span::new(0, 0), exprs.into_oxc(ctx), None),
            oxidescript::parser::ast::Expression::IfExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::BlockExpression(block) => block.into_oxc(ctx),
            oxidescript::parser::ast::Expression::CallExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::IndexExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::MemberAccessExpression(expr) => {
                expr.into_oxc(ctx)
            }
            oxidescript::parser::ast::Expression::ForExpression(expr) => expr.into_oxc(ctx),
        }
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Vec<'c, ArrayExpressionElement<'c>>>
    for Vec<oxidescript::parser::ast::Expression>
{
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Vec<'c, ArrayExpressionElement<'c>> {
        AstBuilder::new(ctx.allocator)
            .vec_from_iter(self.into_iter().map(|expr| expr.into_oxc(ctx).into()))
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Vec<'c, Argument<'c>>>
    for Vec<oxidescript::parser::ast::Expression>
{
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Vec<'c, Argument<'c>> {
        AstBuilder::new(ctx.allocator)
            .vec_from_iter(self.into_iter().map(|expr| expr.into_oxc(ctx).into()))
    }
}
