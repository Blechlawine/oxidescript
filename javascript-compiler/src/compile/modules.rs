use oxc::{
    ast::{AstBuilder, ast::Expression},
    span::Span,
};
use oxidescript::parser::ast::Path;

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c, 'src> IntoOxc<'c, Expression<'c>> for Path<'src> {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        if self.len() == 1 {
            let ident_name = &self.elements.first().unwrap().name;
            AstBuilder::new(ctx.allocator)
                .expression_identifier_reference(Span::new(0, 0), *ident_name)
        } else {
            // let resolved_path = ctx.resolved.borrow().get(&self).cloned();
            // if let Some(resolved_path) = resolved_path {}
            // AstBuilder::new(ctx.allocator)
            //     .member_expression_static(Span::new(0, 0), object, property, false)
            //     .into()
            todo!("compile path with more than one element")
        }
    }
}
