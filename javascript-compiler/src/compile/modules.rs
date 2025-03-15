use oxc::{
    ast::{ast::Expression, AstBuilder},
    span::Span,
};
use oxidescript::parser::ast::Path;
use std::fmt::Write;

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, Expression<'c>> for Path {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        if self.len() == 1 {
            let ident_name = &self.elements.first().unwrap().0;
            AstBuilder::new(ctx.allocator)
                .expression_identifier_reference(Span::new(0, 0), ident_name)
        } else {
            let import_path = self
                .elements
                .into_iter()
                .fold(String::new(), |mut acc, element| {
                    write!(acc, "/{}", element.0).unwrap();
                    acc
                });
            ctx.imports.borrow_mut().push(ctx.import(import_path));
            todo!()
        }
    }
}
