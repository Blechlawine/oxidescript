use oxc::allocator::Allocator;
use oxc_codegen::Gen;
use oxidescript::compiler::Compiler;

mod compile;

pub struct JavascriptCompiler {
    allocator: Allocator,
}

impl Compiler for JavascriptCompiler {
    fn new() -> Self {
        JavascriptCompiler {
            allocator: Allocator::default(),
        }
    }

    fn compile(&self, program: oxidescript::parser::ast::Program) -> String {
        let ctx = JavascriptCompilerContext::new(&self.allocator);
        let compiled_ast = program.into_oxc(&ctx);
        //compiled.gen();
        todo!();
    }
}

struct JavascriptCompilerContext<'a> {
    allocator: &'a Allocator,
}

impl<'ctx> JavascriptCompilerContext<'ctx> {
    fn new(allocator: &'ctx Allocator) -> Self {
        JavascriptCompilerContext { allocator }
    }
}

trait JavascriptCompile<const MINIFY: bool> {
    fn compile<'ctx>(&'ctx self, ctx: &'ctx JavascriptCompilerContext<'ctx>) -> impl Gen<MINIFY>;
}

trait IntoOxc<'c, T> {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> T;
}
