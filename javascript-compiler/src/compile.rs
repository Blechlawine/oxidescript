use oxc::{
    ast::{
        ast::{
            ArrayExpressionElement, BindingIdentifier, BindingPattern, BindingRestElement,
            Expression, FormalParameter, FormalParameters, FunctionBody, Program, Statement,
            TSTypeAnnotation, TSTypeParameterDeclaration, TSTypeParameterInstantiation,
            VariableDeclarator,
        },
        AstBuilder,
    },
    span::{SourceType, Span},
    syntax::{number::NumberBase, operator::UnaryOperator},
};

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

impl<'c> IntoOxc<'c, FormalParameters<'c>> for Vec<oxidescript::parser::ast::Parameter> {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> FormalParameters<'c> {
        FormalParameters {
            span: Span::new(0, 0),
            kind: oxc::ast::ast::FormalParameterKind::FormalParameter,
            items: oxc::allocator::Vec::from_iter_in(
                self.into_iter().map(|param| param.into_oxc(ctx)),
                ctx.allocator,
            ),
            rest: None,
        }
    }
}

impl<'c> IntoOxc<'c, FormalParameter<'c>> for oxidescript::parser::ast::Parameter {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> FormalParameter<'c> {
        FormalParameter {
            span: Span::new(0, 0),
            pattern: self.name.into_oxc(ctx),
            accessibility: None,
            readonly: false,
            r#override: false,
            decorators: oxc::allocator::Vec::new_in(ctx.allocator),
        }
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Expression {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        match self {
            oxidescript::parser::ast::Expression::IdentifierExpression(ident) => {
                let name = AstBuilder::new(ctx.allocator).atom(&ident.0.clone());
                AstBuilder::new(ctx.allocator)
                    .expression_identifier_reference(Span::new(0, 0), name)
            }
            oxidescript::parser::ast::Expression::LiteralExpression(literal) => match literal {
                oxidescript::parser::ast::Literal::StringLiteral(s) => AstBuilder::new(
                    ctx.allocator,
                )
                .expression_string_literal(Span::new(0, 0), &s, None),
                oxidescript::parser::ast::Literal::NumberLiteral(n) => {
                    let s = AstBuilder::new(ctx.allocator).str(n.to_string().as_str());
                    AstBuilder::new(ctx.allocator).expression_numeric_literal(
                        Span::new(0, 0),
                        n.try_into().unwrap(),
                        Some(AstBuilder::new(ctx.allocator).atom(s)),
                        NumberBase::Decimal,
                    )
                }
                oxidescript::parser::ast::Literal::BooleanLiteral(b) => {
                    AstBuilder::new(ctx.allocator).expression_boolean_literal(Span::new(0, 0), b)
                }
            },
            oxidescript::parser::ast::Expression::UnaryExpression(op, expr) => AstBuilder::new(
                ctx.allocator,
            )
            .expression_unary(Span::new(0, 0), op.into_oxc(ctx), expr.into_oxc(ctx)),
            oxidescript::parser::ast::Expression::InfixExpression(op, lhs, rhs) => match op {
                oxidescript::parser::ast::InfixOperator::Plus => AstBuilder::new(ctx.allocator)
                    .expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::Addition,
                        rhs.into_oxc(ctx),
                    ),
                oxidescript::parser::ast::InfixOperator::Minus => AstBuilder::new(ctx.allocator)
                    .expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::Subtraction,
                        rhs.into_oxc(ctx),
                    ),
                oxidescript::parser::ast::InfixOperator::Multiply => AstBuilder::new(ctx.allocator)
                    .expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::Multiplication,
                        rhs.into_oxc(ctx),
                    ),
                oxidescript::parser::ast::InfixOperator::Divide => AstBuilder::new(ctx.allocator)
                    .expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::Division,
                        rhs.into_oxc(ctx),
                    ),
                oxidescript::parser::ast::InfixOperator::Modulo => AstBuilder::new(ctx.allocator)
                    .expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::Remainder,
                        rhs.into_oxc(ctx),
                    ),

                oxidescript::parser::ast::InfixOperator::Equal => AstBuilder::new(ctx.allocator)
                    .expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::StrictEquality,
                        rhs.into_oxc(ctx),
                    ),
                oxidescript::parser::ast::InfixOperator::NotEqual => AstBuilder::new(ctx.allocator)
                    .expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::Inequality,
                        rhs.into_oxc(ctx),
                    ),
                oxidescript::parser::ast::InfixOperator::GreaterThan => {
                    AstBuilder::new(ctx.allocator).expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::GreaterThan,
                        rhs.into_oxc(ctx),
                    )
                }
                oxidescript::parser::ast::InfixOperator::LessThan => AstBuilder::new(ctx.allocator)
                    .expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::LessThan,
                        rhs.into_oxc(ctx),
                    ),

                oxidescript::parser::ast::InfixOperator::GreaterThanEqual => {
                    AstBuilder::new(ctx.allocator).expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::GreaterEqualThan,
                        rhs.into_oxc(ctx),
                    )
                }
                oxidescript::parser::ast::InfixOperator::LessThanEqual => {
                    AstBuilder::new(ctx.allocator).expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::LessEqualThan,
                        rhs.into_oxc(ctx),
                    )
                }

                oxidescript::parser::ast::InfixOperator::LogicalOr => {
                    AstBuilder::new(ctx.allocator).expression_logical(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::LogicalOperator::Or,
                        rhs.into_oxc(ctx),
                    )
                }
                oxidescript::parser::ast::InfixOperator::LogicalAnd => {
                    AstBuilder::new(ctx.allocator).expression_logical(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::LogicalOperator::And,
                        rhs.into_oxc(ctx),
                    )
                }

                oxidescript::parser::ast::InfixOperator::BitwiseOr => {
                    AstBuilder::new(ctx.allocator).expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::BitwiseOR,
                        rhs.into_oxc(ctx),
                    )
                }
                oxidescript::parser::ast::InfixOperator::BitwiseXor => {
                    AstBuilder::new(ctx.allocator).expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::BitwiseXOR,
                        rhs.into_oxc(ctx),
                    )
                }
                oxidescript::parser::ast::InfixOperator::BitwiseAnd => {
                    AstBuilder::new(ctx.allocator).expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::BitwiseAnd,
                        rhs.into_oxc(ctx),
                    )
                }
                oxidescript::parser::ast::InfixOperator::BitwiseLeftShift => {
                    AstBuilder::new(ctx.allocator).expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::ShiftLeft,
                        rhs.into_oxc(ctx),
                    )
                }
                oxidescript::parser::ast::InfixOperator::BitwiseRightShift => {
                    AstBuilder::new(ctx.allocator).expression_binary(
                        Span::new(0, 0),
                        lhs.into_oxc(ctx),
                        oxc::syntax::operator::BinaryOperator::ShiftRight,
                        rhs.into_oxc(ctx),
                    )
                }
            },
            oxidescript::parser::ast::Expression::ArrayExpression(exprs) => AstBuilder::new(
                ctx.allocator,
            )
            .expression_array(Span::new(0, 0), exprs.into_oxc(ctx), None),
            oxidescript::parser::ast::Expression::IfExpression {
                condition,
                then_block,
                else_if_blocks,
                else_block,
            } => {
                if then_block.return_value.is_some() {
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
                                oxc::allocator::Vec::from_iter_in(
                                    [AstBuilder::new(ctx.allocator).statement_if(
                                        Span::new(0, 0),
                                        condition.into_oxc(ctx),
                                        AstBuilder::new(ctx.allocator).statement_block(
                                            Span::new(0, 0),
                                            then_block.into_oxc(ctx),
                                        ),
                                        if let Some(else_block) = else_block {
                                            if !else_if_blocks.is_empty() {
                                                todo!()
                                            } else {
                                                Some(
                                                    AstBuilder::new(ctx.allocator).statement_block(
                                                        Span::new(0, 0),
                                                        else_block.into_oxc(ctx),
                                                    ),
                                                )
                                            }
                                        } else if !else_if_blocks.is_empty() {
                                            todo!()
                                        } else {
                                            None
                                        },
                                    )],
                                    ctx.allocator,
                                ),
                            ),
                        ),
                        None::<TSTypeParameterInstantiation>,
                        oxc::allocator::Vec::new_in(ctx.allocator),
                        false,
                    )
                } else {
                    todo!()
                }
            }
            oxidescript::parser::ast::Expression::BlockExpression(_) => todo!(),
            oxidescript::parser::ast::Expression::CallExpression(_, _) => todo!(),
            oxidescript::parser::ast::Expression::IndexExpression(_, _) => todo!(),
            oxidescript::parser::ast::Expression::MemberAccessExpression(_, _) => todo!(),
        }
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Vec<'c, ArrayExpressionElement<'c>>>
    for Vec<oxidescript::parser::ast::Expression>
{
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Vec<'c, ArrayExpressionElement<'c>> {
        AstBuilder::new(ctx.allocator).vec_from_iter(self.into_iter().map(|expr| todo!()))
    }
}

impl<'c> IntoOxc<'c, UnaryOperator> for oxidescript::parser::ast::UnaryOperator {
    fn into_oxc(self, _: &'c JavascriptCompilerContext<'c>) -> UnaryOperator {
        match self {
            oxidescript::parser::ast::UnaryOperator::LogicalNot => UnaryOperator::LogicalNot,
            oxidescript::parser::ast::UnaryOperator::BitwiseNot => UnaryOperator::BitwiseNot,
            oxidescript::parser::ast::UnaryOperator::Minus => UnaryOperator::UnaryNegation,
            oxidescript::parser::ast::UnaryOperator::Plus => UnaryOperator::UnaryPlus,
        }
    }
}
