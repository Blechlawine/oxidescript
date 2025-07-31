use oxc::ast::ast::Expression;

use crate::IntoOxc;

impl<'c, 'src> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::MemberAccessExpr<'src> {
    fn into_oxc(self, ctx: &'c crate::JavascriptCompilerContext<'c>) -> Expression<'c> {
        ctx.member_access(self.lhs.into_oxc(ctx), self.ident.into_oxc(ctx))
    }
}
