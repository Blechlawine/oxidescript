use oxc::allocator::Allocator;
use oxc_codegen::Codegen;
use oxidescript::compiler::Compiler;

mod compile;
mod js;

pub struct JavascriptCompiler {
    allocator: Allocator,
}

impl Compiler for JavascriptCompiler {
    fn new() -> Self {
        JavascriptCompiler {
            allocator: Allocator::default(),
        }
    }

    fn compile(
        &self,
        program: oxidescript::parser::ast::Program,
        // resolved: &RefCell<BTreeMap<Path, Resolved>>,
    ) -> String {
        let ctx = JavascriptCompilerContext::new(&self.allocator);
        let compiled_ast = program.into_oxc(&ctx);
        let code_gen = Codegen::new();
        let code = code_gen.build(&compiled_ast);
        code.code
    }
}

struct JavascriptCompilerContext<'a> {
    allocator: &'a Allocator,
    // resolved: &'a RefCell<BTreeMap<Path, Resolved>>,
}

impl<'ctx> JavascriptCompilerContext<'ctx> {
    fn new(allocator: &'ctx Allocator) -> Self {
        JavascriptCompilerContext {
            allocator,
            // resolved,
        }
    }
}

trait IntoOxc<'c, T> {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> T;
}
