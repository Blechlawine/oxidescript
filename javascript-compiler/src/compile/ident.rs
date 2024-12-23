use oxc::{
    ast::{
        ast::{BindingIdentifier, BindingPattern, Expression, IdentifierReference},
        AstBuilder,
    },
    span::Span,
};

use crate::{IntoOxc, JavascriptCompilerContext};

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
        let name = AstBuilder::new(ctx.allocator).atom(&self.0.clone());
        BindingIdentifier {
            span: Span::new(0, 0),
            name,
            symbol_id: None.into(),
        }
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Identifier {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        let name = AstBuilder::new(ctx.allocator).atom(&self.0.clone());
        AstBuilder::new(ctx.allocator).expression_identifier_reference(Span::new(0, 0), name)
    }
}

impl<'c> IntoOxc<'c, IdentifierReference<'c>> for oxidescript::parser::ast::Identifier {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> IdentifierReference {
        AstBuilder::new(ctx.allocator).identifier_reference(Span::new(0, 0), self.0)
    }
}
