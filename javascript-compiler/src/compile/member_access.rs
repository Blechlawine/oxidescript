use oxc::{
    ast::{ast::Expression, AstBuilder},
    span::Span,
};

use crate::IntoOxc;

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::MemberAccessExpr {
    fn into_oxc(self, ctx: &'c crate::JavascriptCompilerContext<'c>) -> Expression<'c> {
        AstBuilder::new(ctx.allocator)
            .member_expression_static(
                Span::new(0, 0),
                self.lhs.into_oxc(ctx),
                self.ident.into_oxc(ctx),
                false,
            )
            .into()
    }
}
