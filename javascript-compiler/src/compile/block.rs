use oxc::{
    ast::{
        ast::{
            BindingRestElement, Expression, FunctionBody, Statement, TSTypeAnnotation,
            TSTypeParameterDeclaration, TSTypeParameterInstantiation,
        },
        AstBuilder,
    },
    span::Span,
};

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, FunctionBody<'c>> for oxidescript::parser::ast::Block {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> FunctionBody<'c> {
        AstBuilder::new(ctx.allocator).function_body(
            Span::new(0, 0),
            oxc::allocator::Vec::new_in(ctx.allocator),
            self.into_oxc(ctx),
        )
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Vec<'c, Statement<'c>>> for oxidescript::parser::ast::Block {
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Vec<'c, Statement<'c>> {
        let statements = self
            .statements
            .into_iter()
            .map(|statement| statement.into_oxc(ctx));
        let return_value = self.return_value.map(|return_value| {
            AstBuilder::new(ctx.allocator)
                .statement_return(Span::new(0, 0), Some(return_value.into_oxc(ctx)))
        });
        let mut vec = oxc::allocator::Vec::from_iter_in(statements, ctx.allocator);
        if let Some(return_value) = return_value {
            vec.push(return_value);
        }
        vec
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Block {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        let callee = AstBuilder::new(ctx.allocator).expression_arrow_function(
            Span::new(0, 0),
            false,
            false,
            None::<TSTypeParameterDeclaration>,
            AstBuilder::new(ctx.allocator).formal_parameters(
                Span::new(0, 0),
                oxc::ast::ast::FormalParameterKind::FormalParameter,
                oxc::allocator::Vec::new_in(ctx.allocator),
                None::<BindingRestElement>,
            ),
            None::<TSTypeAnnotation>,
            ctx.r#box(self.into_oxc(ctx)),
        );
        AstBuilder::new(ctx.allocator).expression_call(
            Span::new(0, 0),
            callee,
            None::<TSTypeParameterInstantiation>,
            oxc::allocator::Vec::new_in(ctx.allocator),
            false,
        )
    }
}

impl<'c> IntoOxc<'c, Statement<'c>> for oxidescript::parser::ast::Block {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Statement<'c> {
        AstBuilder::new(ctx.allocator).statement_block(Span::new(0, 0), self.into_oxc(ctx))
    }
}
