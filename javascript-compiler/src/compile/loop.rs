use oxc::{
    ast::{
        ast::{Expression, Statement, TSTypeParameterInstantiation, VariableDeclarationKind},
        AstBuilder,
    },
    span::Span,
};
use oxidescript::parser::ast::ForExpr;
use rand::{distributions::Alphanumeric, Rng};

use crate::{compile::iife, IntoOxc};

impl<'c> IntoOxc<'c, Expression<'c>> for ForExpr {
    fn into_oxc(self, ctx: &'c crate::JavascriptCompilerContext<'c>) -> Expression<'c> {
        let output_id = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(8)
            .map(char::from)
            .collect::<String>();
        let output = AstBuilder::new(ctx.allocator).variable_declaration(
            Span::new(0, 0),
            VariableDeclarationKind::Let,
            oxc::allocator::Vec::from_iter_in(
                [AstBuilder::new(ctx.allocator).variable_declarator(
                    Span::new(0, 0),
                    VariableDeclarationKind::Let,
                    oxidescript::parser::ast::Identifier(output_id.clone()).into_oxc(ctx),
                    Some(AstBuilder::new(ctx.allocator).expression_array(
                        Span::new(0, 0),
                        oxc::allocator::Vec::new_in(ctx.allocator),
                        None,
                    )),
                    false,
                )],
                ctx.allocator,
            ),
            false,
        );
        let inner_statements = oxc::allocator::Vec::from_iter_in(
            [AstBuilder::new(ctx.allocator).statement_expression(
                Span::new(0, 0),
                AstBuilder::new(ctx.allocator).expression_call(
                    Span::new(0, 0),
                    AstBuilder::new(ctx.allocator)
                        .member_expression_static(
                            Span::new(0, 0),
                            oxidescript::parser::ast::Identifier(output_id.clone()).into_oxc(ctx),
                            oxc::ast::ast::IdentifierName {
                                span: Span::new(0, 0),
                                name: AstBuilder::new(ctx.allocator).atom("push"),
                            },
                            false,
                        )
                        .into(),
                    None::<TSTypeParameterInstantiation>,
                    oxc::allocator::Vec::from_iter_in(
                        [iife(self.body.into_oxc(ctx), ctx).into()],
                        ctx.allocator,
                    ),
                    false,
                ),
            )],
            ctx.allocator,
        );
        let r#loop = AstBuilder::new(ctx.allocator).statement_for_of(
            Span::new(0, 0),
            false,
            self.lhs.into_oxc(ctx),
            self.rhs.into_oxc(ctx),
            AstBuilder::new(ctx.allocator).statement_block(Span::new(0, 0), inner_statements),
        );
        iife(
            oxc::allocator::Vec::from_iter_in(
                [
                    oxc::ast::ast::Statement::VariableDeclaration(oxc::allocator::Box::new_in(
                        output,
                        ctx.allocator,
                    )),
                    r#loop,
                    AstBuilder::new(ctx.allocator).statement_return(
                        Span::new(0, 0),
                        Some(oxidescript::parser::ast::Identifier(output_id).into_oxc(ctx)),
                    ),
                ],
                ctx.allocator,
            ),
            ctx,
        )
    }
}

impl<'c> IntoOxc<'c, Statement<'c>> for ForExpr {
    fn into_oxc(self, ctx: &'c crate::JavascriptCompilerContext<'c>) -> Statement<'c> {
        let inner_statements: oxc::allocator::Vec<Statement> = self.body.into_oxc(ctx);
        AstBuilder::new(ctx.allocator).statement_for_of(
            Span::new(0, 0),
            false,
            self.lhs.into_oxc(ctx),
            self.rhs.into_oxc(ctx),
            AstBuilder::new(ctx.allocator).statement_block(Span::new(0, 0), inner_statements),
        )
    }
}
