use crate::JavascriptCompilerContext;
use oxc::{
    ast::{
        ast::{
            Argument, ArrayExpressionElement, BindingIdentifier, BindingPattern,
            BindingRestElement, Expression, ForStatementLeft, FormalParameters, Function,
            FunctionBody, IdentifierName, Statement, TSTypeAnnotation, TSTypeParameterDeclaration,
            TSTypeParameterInstantiation, VariableDeclaration, VariableDeclarationKind,
        },
        AstBuilder,
    },
    span::Span,
};

impl<'c> JavascriptCompilerContext<'c> {
    pub fn iife(&self, body: oxc::allocator::Vec<'c, Statement<'c>>) -> Expression<'c> {
        self.expr_call(
            self.arrow_function(
                AstBuilder::new(self.allocator).formal_parameters(
                    Span::new(0, 0),
                    oxc::ast::ast::FormalParameterKind::FormalParameter,
                    oxc::allocator::Vec::new_in(self.allocator),
                    None::<BindingRestElement>,
                ),
                AstBuilder::new(self.allocator).function_body(
                    Span::new(0, 0),
                    oxc::allocator::Vec::new_in(self.allocator),
                    body,
                ),
            ),
            oxc::allocator::Vec::new_in(self.allocator),
        )
    }

    pub fn for_of(
        &self,
        lhs: ForStatementLeft<'c>,
        rhs: Expression<'c>,
        inner: oxc::allocator::Vec<'c, Statement<'c>>,
    ) -> Statement<'c> {
        AstBuilder::new(self.allocator).statement_for_of(
            Span::new(0, 0),
            false,
            lhs,
            rhs,
            AstBuilder::new(self.allocator).statement_block(Span::new(0, 0), inner),
        )
    }

    pub fn r#return(&self, expr: Option<Expression<'c>>) -> Statement<'c> {
        AstBuilder::new(self.allocator).statement_return(Span::new(0, 0), expr)
    }

    pub fn expr_call(
        &self,
        callee: Expression<'c>,
        args: oxc::allocator::Vec<'c, Argument<'c>>,
    ) -> Expression<'c> {
        AstBuilder::new(self.allocator).expression_call(
            Span::new(0, 0),
            callee,
            None::<TSTypeParameterInstantiation>,
            args,
            false,
        )
    }

    pub fn statement_expr(&self, expr: Expression<'c>) -> Statement<'c> {
        AstBuilder::new(self.allocator).statement_expression(Span::new(0, 0), expr)
    }

    pub fn r#let(
        &self,
        id: BindingPattern<'c>,
        expr: Option<Expression<'c>>,
    ) -> VariableDeclaration<'c> {
        AstBuilder::new(self.allocator).variable_declaration(
            Span::new(0, 0),
            VariableDeclarationKind::Let,
            oxc::allocator::Vec::from_iter_in(
                [AstBuilder::new(self.allocator).variable_declarator(
                    Span::new(0, 0),
                    VariableDeclarationKind::Let,
                    id,
                    expr,
                    false,
                )],
                self.allocator,
            ),
            false,
        )
    }

    pub fn r#const(
        &self,
        id: BindingPattern<'c>,
        expr: Option<Expression<'c>>,
    ) -> VariableDeclaration<'c> {
        AstBuilder::new(self.allocator).variable_declaration(
            Span::new(0, 0),
            VariableDeclarationKind::Const,
            oxc::allocator::Vec::from_iter_in(
                [AstBuilder::new(self.allocator).variable_declarator(
                    Span::new(0, 0),
                    VariableDeclarationKind::Const,
                    id,
                    expr,
                    false,
                )],
                self.allocator,
            ),
            false,
        )
    }

    pub fn function(
        &self,
        id: BindingIdentifier<'c>,
        parameters: FormalParameters<'c>,
        body: FunctionBody<'c>,
    ) -> Function<'c> {
        oxc::ast::ast::Function {
            r#type: oxc::ast::ast::FunctionType::FunctionDeclaration,
            span: Span::new(0, 0),
            id: Some(id),
            generator: false,
            r#async: false,
            declare: false,
            type_parameters: None,
            this_param: None,
            params: self.r#box(parameters),
            body: Some(self.r#box(body)),
            return_type: None,
            scope_id: None.into(),
        }
    }

    pub fn arrow_function(
        &self,
        parameters: FormalParameters<'c>,
        body: FunctionBody<'c>,
    ) -> Expression<'c> {
        AstBuilder::new(self.allocator).expression_arrow_function(
            Span::new(0, 0),
            false,
            false,
            None::<TSTypeParameterDeclaration>,
            parameters,
            None::<TSTypeAnnotation>,
            body,
        )
    }

    pub fn r#box<T>(&self, inner: T) -> oxc::allocator::Box<'c, T> {
        oxc::allocator::Box::new_in(inner, self.allocator)
    }

    pub fn array(
        &self,
        inner: Option<oxc::allocator::Vec<'c, ArrayExpressionElement<'c>>>,
    ) -> Expression<'c> {
        AstBuilder::new(self.allocator).expression_array(
            Span::new(0, 0),
            match inner {
                Some(expr) => expr,
                None => oxc::allocator::Vec::new_in(self.allocator),
            },
            None,
        )
    }

    pub fn member_access(
        &self,
        obj: Expression<'c>,
        property: IdentifierName<'c>,
    ) -> Expression<'c> {
        AstBuilder::new(self.allocator)
            .member_expression_static(Span::new(0, 0), obj, property, false)
            .into()
    }

    pub fn ident_name(&self, name: impl AsRef<str>) -> IdentifierName<'c> {
        oxc::ast::ast::IdentifierName {
            span: Span::new(0, 0),
            name: AstBuilder::new(self.allocator).atom(name.as_ref()),
        }
    }
}
