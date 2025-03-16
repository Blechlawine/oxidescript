use crate::parser::ast::{
    CallExpr, ElseIfExpr, Expression, ForExpr, IfExpr, InfixExpr, InfixOperator, Literal,
    MemberAccessExpr, UnaryExpr, UnaryOperator,
};

use super::{AstNode, TypeChecker, VariableType};

impl AstNode for Expression {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        match self {
            Expression::PathExpression(path) => todo!(), //ctx.resolve_symbol(path).r#type,
            Expression::LiteralExpression(literal) => literal.check_type(ctx),
            Expression::UnaryExpression(unary_expr) => unary_expr.check_type(ctx),
            Expression::InfixExpression(infix_expr) => infix_expr.check_type(ctx),
            Expression::ArrayExpression(vec) => todo!("array infer_type"),
            Expression::IfExpression(if_expr) => if_expr.check_type(ctx),
            Expression::ForExpression(for_expr) => for_expr.check_type(ctx),
            Expression::BlockExpression(block) => todo!("block infer_type"),
            Expression::CallExpression(call_expr) => call_expr.check_type(ctx),
            Expression::IndexExpression(index_expr) => todo!("index infer_type"),
            Expression::MemberAccessExpression(member_access_expr) => {
                member_access_expr.check_type(ctx)
            }
        }
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}

impl AstNode for Literal {
    fn check_type(&self, _: &TypeChecker) -> VariableType {
        match self {
            Literal::StringLiteral(_) => VariableType::String,
            Literal::NumberLiteral(_) => VariableType::Number,
            Literal::BooleanLiteral(_) => VariableType::Bool,
        }
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}

impl AstNode for UnaryExpr {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        let rhs_type = self.rhs.check_type(ctx);
        match (&self.op, &rhs_type) {
            (UnaryOperator::LogicalNot, VariableType::Bool) => VariableType::Bool,
            (UnaryOperator::BitwiseNot, VariableType::Number) => VariableType::Number,
            (UnaryOperator::Minus, VariableType::Number) => VariableType::Number,
            (UnaryOperator::Plus, VariableType::Number) => VariableType::Number,
            _ => panic!("Invalid operator for type {rhs_type}"),
        }
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}

impl AstNode for InfixExpr {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        let lhs_type = self.lhs.check_type(ctx);
        let rhs_type = self.rhs.check_type(ctx);
        match (&lhs_type, &rhs_type) {
            (VariableType::String, VariableType::String) => match self.op {
                InfixOperator::Equal
                | InfixOperator::NotEqual
                | InfixOperator::GreaterThan
                | InfixOperator::LessThan
                | InfixOperator::GreaterThanEqual
                | InfixOperator::LessThanEqual => VariableType::Bool,
                InfixOperator::Plus => VariableType::String,
                _ => {
                    panic!("Invalid operator for type String")
                }
            },
            (VariableType::Number, VariableType::Number) => match self.op {
                InfixOperator::Equal
                | InfixOperator::NotEqual
                | InfixOperator::GreaterThan
                | InfixOperator::LessThan
                | InfixOperator::GreaterThanEqual
                | InfixOperator::LessThanEqual => VariableType::Bool,
                InfixOperator::Plus
                | InfixOperator::Minus
                | InfixOperator::Multiply
                | InfixOperator::Divide
                | InfixOperator::Modulo => VariableType::Number,
                InfixOperator::BitwiseOr
                | InfixOperator::BitwiseXor
                | InfixOperator::BitwiseAnd
                | InfixOperator::BitwiseLeftShift
                | InfixOperator::BitwiseRightShift => VariableType::Number,
                _ => {
                    panic!("Invalid operator for type number")
                }
            },
            (VariableType::Bool, VariableType::Bool) => match self.op {
                InfixOperator::Equal
                | InfixOperator::NotEqual
                | InfixOperator::GreaterThan
                | InfixOperator::LessThan
                | InfixOperator::GreaterThanEqual
                | InfixOperator::LessThanEqual
                | InfixOperator::LogicalOr
                | InfixOperator::LogicalAnd => VariableType::Bool,
                _ => panic!("Invalid operator for type bool"),
            },
            (VariableType::Struct { .. }, VariableType::Struct { .. }) => match self.op {
                InfixOperator::Equal | InfixOperator::NotEqual => VariableType::Bool,
                _ => panic!("Invalid operator for type Struct"),
            },
            (VariableType::Vec(_), VariableType::Vec(_)) => match self.op {
                InfixOperator::Equal | InfixOperator::NotEqual => VariableType::Bool,
                _ => panic!("Invalid operator for type Vec"),
            },
            (VariableType::Void, VariableType::Void) => VariableType::Void,
            _ => panic!("Invalid type comparison: {lhs_type} {} {rhs_type}", self.op),
        }
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}

impl AstNode for MemberAccessExpr {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        let lhs_type = self.lhs.check_type(ctx);
        if let VariableType::Struct { fields } = lhs_type {
            if let Some(field_type) = fields.get(&self.ident.name) {
                field_type.clone()
            } else {
                panic!("Missing field {}", self.ident.name)
            }
        } else {
            panic!("Cannot index type other than struct: {}", self.ident.name)
        }
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}

impl AstNode for ForExpr {
    fn check_type(&self, _: &TypeChecker) -> VariableType {
        if self.body.return_value.is_none() {
            VariableType::Void
        } else {
            todo!("for expr infer type")
        }
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}

impl AstNode for IfExpr {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        let condition_type = self.condition.check_type(ctx);
        if condition_type != VariableType::Bool {
            panic!("Expected bool, found: {condition_type}");
        }
        let then_type = self.then_block.check_type(ctx);
        if !self
            .else_if_blocks
            .iter()
            .map(|b| b.check_type(ctx))
            .all(|t| t == then_type)
        {
            panic!("Expected {then_type}");
        }
        let else_type = self.else_block.as_ref().map(|b| b.check_type(ctx));
        if let Some(else_type) = else_type {
            if else_type != then_type {
                panic!("Expected {then_type}, found: {else_type}");
            }
        }
        then_type
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}

impl AstNode for ElseIfExpr {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        let condition_type = self.condition.check_type(ctx);
        if condition_type != VariableType::Bool {
            panic!("Expected bool, found: {condition_type}");
        }
        self.then_block.check_type(ctx)
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}

impl AstNode for CallExpr {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        let lhs_type = self.lhs.check_type(ctx);
        if let VariableType::Function {
            parameters,
            return_type,
        } = lhs_type
        {
            if !self
                .arguments
                .iter()
                .map(|a| a.check_type(ctx))
                .zip(parameters)
                .all(|(given, expected)| given == expected)
            {
                panic!("Invalid argument type");
            }
            *return_type
        } else {
            panic!("Cannot call non-function type: {lhs_type}");
        }
    }

    fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
        todo!()
    }
}
