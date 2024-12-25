use crate::parser::ast::Program;

pub trait Compiler {
    fn new() -> Self;
    fn compile(&self, program: Program) -> String;
}
