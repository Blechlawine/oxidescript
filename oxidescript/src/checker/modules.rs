use crate::parser::ast::{ModuleDeclaration, Use};

use super::{
    symbols::{ModuleSymbol, Symbol, SymbolId, SymbolInner},
    AstNode, AstVisitor, TypeChecker,
};

impl AstNode for ModuleDeclaration {
    fn check_type(&self, ctx: &TypeChecker) -> super::VariableType {
        match self {
            ModuleDeclaration::Extern { path, content } => todo!(),
            ModuleDeclaration::Intern { path, content } => todo!(),
        }
        // if let Some(content) = &self.content {
        //     // this ctx does not need to call .init, becaus it inherits the resolved tree from the
        //     // passed in ctx
        //     ctx.insert_declaration(
        //         self.path.clone(),
        //         Resolved::Module {
        //             is_extern: self.is_extern,
        //         },
        //     );
        //     let new_ctx = CheckContext {
        //         scope: RefCell::new(Scope::default()),
        //         errors: ctx.errors,
        //         resolved: ctx.resolved,
        //         current_resolved_path: self.path.clone(),
        //     };
        //     content.check(&new_ctx)
        // } else {
        //     todo!()
        // }
    }

    fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        if let ModuleDeclaration::Intern { content, .. } = self {
            if content.is_none() {
                // TODO: parse module and push into symbol_table
                // let (unlexed, tokens) = Lexer::lex_tokens(loaded_file.as_bytes()).unwrap();
                // let (unparsed, ast) = Parser::parse(Tokens::new(&tokens)).unwrap();
            }
        }
        // visitor.symbol_table.symbols.insert(
        //     SymbolId::new(),
        //     Symbol {
        //         scope: visitor.current_scope.unwrap().clone(),
        //         inner: SymbolInner::Module(match self {
        //             ModuleDeclaration::Extern { path, content } => ModuleSymbol::Extern {
        //                 ast: content.clone(),
        //             },
        //             ModuleDeclaration::Intern { path, content } => ModuleSymbol::Intern {
        //                 ast: content.clone(),
        //             },
        //         }),
        //     },
        // );
    }
}

impl AstNode for Use {
    fn check_type(&self, ctx: &TypeChecker) -> super::VariableType {
        todo!()
    }

    fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        todo!()
    }
}
