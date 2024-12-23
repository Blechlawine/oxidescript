use oxc::{
    ast::{
        ast::{
            ArrayExpressionElement, BindingRestElement, Expression, FunctionBody, Program,
            Statement, TSTypeAnnotation, TSTypeParameterDeclaration, TSTypeParameterInstantiation,
            VariableDeclarator,
        },
        AstBuilder,
    },
    span::{SourceType, Span},
};

pub mod function;
pub mod ident;
pub mod infix;
pub mod literal;
pub mod unary;

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, Program<'c>> for oxidescript::parser::ast::Program {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Program {
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
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Statement {
        match self {
            oxidescript::parser::ast::Statement::ExpressionStatement { expression, .. } => {
                AstBuilder::new(ctx.allocator)
                    .statement_expression(Span::new(0, 0), expression.into_oxc(ctx))
            }
            oxidescript::parser::ast::Statement::DeclarationStatement(declaration) => {
                match declaration {
                    oxidescript::parser::ast::Declaration::ConstDeclaration(ident, expr) => {
                        oxc::ast::ast::Statement::VariableDeclaration(oxc::allocator::Box::new_in(
                            AstBuilder::new(ctx.allocator).variable_declaration(
                                Span::new(0, 0),
                                oxc::ast::ast::VariableDeclarationKind::Const,
                                oxc::allocator::Vec::from_iter_in(
                                    vec![VariableDeclarator {
                                        span: Span::new(0, 0),
                                        kind: oxc::ast::ast::VariableDeclarationKind::Const,
                                        id: ident.into_oxc(ctx),
                                        init: Some(expr.into_oxc(ctx)),
                                        definite: false,
                                    }],
                                    ctx.allocator,
                                ),
                                false,
                            ),
                            ctx.allocator,
                        ))
                    }
                    oxidescript::parser::ast::Declaration::LetDeclaration(ident, expr) => {
                        oxc::ast::ast::Statement::VariableDeclaration(oxc::allocator::Box::new_in(
                            AstBuilder::new(ctx.allocator).variable_declaration(
                                Span::new(0, 0),
                                oxc::ast::ast::VariableDeclarationKind::Let,
                                oxc::allocator::Vec::from_iter_in(
                                    vec![VariableDeclarator {
                                        span: Span::new(0, 0),
                                        kind: oxc::ast::ast::VariableDeclarationKind::Let,
                                        id: ident.into_oxc(ctx),
                                        init: Some(expr.into_oxc(ctx)),
                                        definite: false,
                                    }],
                                    ctx.allocator,
                                ),
                                false,
                            ),
                            ctx.allocator,
                        ))
                    }
                    oxidescript::parser::ast::Declaration::FunctionDeclaration {
                        name,
                        parameters,
                        body,
                    } => {
                        oxc::ast::ast::Statement::FunctionDeclaration(oxc::allocator::Box::new_in(
                            oxc::ast::ast::Function {
                                r#type: oxc::ast::ast::FunctionType::FunctionDeclaration,
                                span: Span::new(0, 0),
                                id: Some(name.into_oxc(ctx)),
                                generator: false,
                                r#async: false,
                                declare: false,
                                type_parameters: None,
                                this_param: None,
                                params: oxc::allocator::Box::new_in(
                                    parameters.into_oxc(ctx),
                                    ctx.allocator,
                                ),
                                body: Some(body.into_oxc(ctx)),
                                return_type: None,
                                scope_id: None.into(),
                            },
                            ctx.allocator,
                        ))
                    }
                }
            }
        }
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Box<'c, FunctionBody<'c>>>
    for oxidescript::parser::ast::Block
{
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Box<'c, FunctionBody<'c>> {
        oxc::allocator::Box::new_in(
            AstBuilder::new(ctx.allocator).function_body(
                Span::new(0, 0),
                oxc::allocator::Vec::new_in(ctx.allocator),
                self.into_oxc(ctx),
            ),
            ctx.allocator,
        )
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Vec<'c, Statement<'c>>> for oxidescript::parser::ast::Block {
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Vec<'c, Statement<'c>> {
        oxc::allocator::Vec::from_iter_in(
            self.statements
                .into_iter()
                .map(|statement| statement.into_oxc(ctx)),
            ctx.allocator,
        )
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
            <Self as IntoOxc<oxc::allocator::Box<FunctionBody>>>::into_oxc(self, ctx),
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
            oxidescript::parser::ast::Expression::BlockExpression(block) => todo!(),
            oxidescript::parser::ast::Expression::CallExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::IndexExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::MemberAccessExpression(expr) => todo!(),
        }
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::IfExpr {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        AstBuilder::new(ctx.allocator).expression_call(
            Span::new(0, 0),
            AstBuilder::new(ctx.allocator).expression_arrow_function(
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
                AstBuilder::new(ctx.allocator).function_body(
                    Span::new(0, 0),
                    oxc::allocator::Vec::new_in(ctx.allocator),
                    oxc::allocator::Vec::from_iter_in([self.into_oxc(ctx)], ctx.allocator),
                ),
            ),
            None::<TSTypeParameterInstantiation>,
            oxc::allocator::Vec::new_in(ctx.allocator),
            false,
        )
    }
}

impl<'c> IntoOxc<'c, Statement<'c>> for oxidescript::parser::ast::IfExpr {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Statement<'c> {
        AstBuilder::new(ctx.allocator).statement_if(
            Span::new(0, 0),
            self.condition.into_oxc(ctx),
            AstBuilder::new(ctx.allocator)
                .statement_block(Span::new(0, 0), self.then_block.into_oxc(ctx)),
            if let Some(else_block) = self.else_block {
                if !self.else_if_blocks.is_empty() {
                    let mut iter = self.else_if_blocks.into_iter().rev();
                    let mut inner = iter.next().map(|block| {
                        AstBuilder::new(ctx.allocator).statement_if(
                            Span::new(0, 0),
                            block.condition.into_oxc(ctx),
                            AstBuilder::new(ctx.allocator)
                                .statement_block(Span::new(0, 0), block.then_block.into_oxc(ctx)),
                            Some(
                                AstBuilder::new(ctx.allocator)
                                    .statement_block(Span::new(0, 0), else_block.into_oxc(ctx)),
                            ),
                        )
                    });
                    for else_if_block in iter {
                        inner = Some(AstBuilder::new(ctx.allocator).statement_if(
                            Span::new(0, 0),
                            else_if_block.condition.into_oxc(ctx),
                            AstBuilder::new(ctx.allocator).statement_block(
                                Span::new(0, 0),
                                else_if_block.then_block.into_oxc(ctx),
                            ),
                            inner,
                        ));
                    }
                    inner
                } else {
                    Some(
                        AstBuilder::new(ctx.allocator)
                            .statement_block(Span::new(0, 0), else_block.into_oxc(ctx)),
                    )
                }
            } else if !self.else_if_blocks.is_empty() {
                let mut iter = self.else_if_blocks.into_iter().rev();
                let mut inner = iter.next().map(|block| {
                    AstBuilder::new(ctx.allocator).statement_if(
                        Span::new(0, 0),
                        block.condition.into_oxc(ctx),
                        AstBuilder::new(ctx.allocator)
                            .statement_block(Span::new(0, 0), block.then_block.into_oxc(ctx)),
                        None,
                    )
                });
                for else_if_block in iter {
                    inner = Some(AstBuilder::new(ctx.allocator).statement_if(
                        Span::new(0, 0),
                        else_if_block.condition.into_oxc(ctx),
                        AstBuilder::new(ctx.allocator).statement_block(
                            Span::new(0, 0),
                            else_if_block.then_block.into_oxc(ctx),
                        ),
                        inner,
                    ));
                }
                inner
            } else {
                None
            },
        )
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
            .vec_from_iter(self.into_iter().map(|expr| expr.into_oxc(ctx)))
    }
}

impl<'c> IntoOxc<'c, ArrayExpressionElement<'c>> for oxidescript::parser::ast::Expression {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> ArrayExpressionElement<'c> {
        match self {
            oxidescript::parser::ast::Expression::IdentifierExpression(ident) => {
                ArrayExpressionElement::Identifier(oxc::allocator::Box::new_in(
                    ident.into_oxc(ctx),
                    ctx.allocator,
                ))
            }
            oxidescript::parser::ast::Expression::LiteralExpression(literal) => todo!(),
            oxidescript::parser::ast::Expression::UnaryExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::InfixExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::ArrayExpression(exprs) => todo!(),
            oxidescript::parser::ast::Expression::IfExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::BlockExpression(block) => todo!(),
            oxidescript::parser::ast::Expression::CallExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::IndexExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::MemberAccessExpression(expr) => todo!(),
        }
    }
}
