use oxc::{
    ast::{
        ast::{
            BindingIdentifier, BindingPattern, Expression, ForStatementLeft, IdentifierName,
            IdentifierReference, VariableDeclarator,
        },
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
        let name = AstBuilder::new(ctx.allocator).atom(&self.name.clone());
        BindingIdentifier {
            span: Span::new(0, 0),
            name,
            symbol_id: None.into(),
        }
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Identifier {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        let name = AstBuilder::new(ctx.allocator).atom(&self.name.clone());
        AstBuilder::new(ctx.allocator).expression_identifier_reference(Span::new(0, 0), name)
    }
}

impl<'c> IntoOxc<'c, IdentifierReference<'c>> for oxidescript::parser::ast::Identifier {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> IdentifierReference<'c> {
        AstBuilder::new(ctx.allocator).identifier_reference(Span::new(0, 0), self.name)
    }
}

impl<'c> IntoOxc<'c, IdentifierName<'c>> for oxidescript::parser::ast::Identifier {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> IdentifierName<'c> {
        AstBuilder::new(ctx.allocator).identifier_name(Span::new(0, 0), self.name)
    }
}

impl<'c> IntoOxc<'c, ForStatementLeft<'c>> for oxidescript::parser::ast::Identifier {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> ForStatementLeft<'c> {
        ForStatementLeft::VariableDeclaration(oxc::allocator::Box::new_in(
            oxc::ast::ast::VariableDeclaration {
                span: Span::new(0, 0),
                kind: oxc::ast::ast::VariableDeclarationKind::Const,
                declarations: oxc::allocator::Vec::from_iter_in(
                    [VariableDeclarator {
                        span: Span::new(0, 0),
                        kind: oxc::ast::ast::VariableDeclarationKind::Const,
                        id: self.into_oxc(ctx),
                        init: None,
                        definite: false,
                    }],
                    ctx.allocator,
                ),
                declare: false,
            },
            ctx.allocator,
        ))
    }
}
