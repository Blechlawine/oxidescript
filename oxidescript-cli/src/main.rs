use std::{
    cell::RefCell,
    fs::read_to_string,
    path::{Path, PathBuf},
    process::{Command, exit},
};

use clap::Parser as ClapParser;
use environment::JavascriptEnvironment;
use oxidescript::{
    compiler::Compiler, lexer::Lexer, loader::SourceLoader, parser::Parser, resolver::Resolver,
};
use oxidescript_javascript_compiler::JavascriptCompiler;

mod environment;

#[derive(clap::Parser, Debug)]
#[command(version)]
struct Args {
    #[arg(short, long)]
    input: PathBuf,

    #[arg(short, long, action = clap::ArgAction::SetTrue)]
    verbose: bool,

    #[command(subcommand)]
    command: OxideCommand,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum OxideCommand {
    /// Compile files
    Compile {
        #[arg(short, long)]
        environment: Option<JavascriptEnvironment>,

        #[arg(short, long)]
        outdir: Option<PathBuf>,
    },
}

#[derive(Clone, Debug, Default)]
enum JavascriptRuntime {
    #[default]
    Bun,
    Node,
}

impl JavascriptRuntime {
    fn run(&self, path: &Path) -> Result<std::process::Child, std::io::Error> {
        let cmd = match self {
            Self::Bun => "bun",
            Self::Node => "node",
        };
        Command::new(cmd).arg(path).spawn()
    }
}

impl From<&str> for JavascriptRuntime {
    fn from(value: &str) -> Self {
        match value {
            "bun" => Self::Bun,
            "node" => Self::Node,
            _ => Self::default(),
        }
    }
}

struct Context {
    verbose: bool,
}

const DEFAULT_DEVDIR: &str = "./.oxidescript-tmp";

fn main() {
    let args = Args::parse();

    let ctx = Context {
        verbose: args.verbose,
    };

    match args.command {
        OxideCommand::Compile {
            environment,
            outdir,
        } => {
            let outdir = if let Some(outdir) = outdir {
                if outdir.is_dir() {
                    if let Ok(read_dir) = outdir.read_dir() {
                        if read_dir.count() > 0 {
                            println!(
                                "Outdir {} is not empty. Cancelling",
                                outdir.to_string_lossy()
                            );
                            exit(1);
                        }
                    }
                    if outdir.exists() {
                        std::fs::remove_dir_all(&outdir).unwrap();
                    }
                    std::fs::create_dir_all(&outdir).unwrap();
                    outdir
                } else {
                    println!(
                        "Outdir {} is not a directory. Cancelling",
                        outdir.to_string_lossy()
                    );
                    exit(1);
                }
            } else {
                PathBuf::from(".")
            };

            let compiled = compile_files(&args.input, &ctx, environment.unwrap_or_default());
            let new_file_name = format!("{}.js", args.input.file_stem().unwrap().to_str().unwrap());
            let compiled_path = outdir.join(new_file_name);
            std::fs::write(compiled_path, compiled).unwrap();
        }
    }
}

fn compile_files(path: &Path, ctx: &Context, environment: JavascriptEnvironment) -> String {
    let loader = SourceLoader::new(path.to_path_buf());
    let source_tree = loader.load();
    if ctx.verbose {
        println!("Loaded file: {:#?}", &source_tree);
    }

    let lexed = Lexer::tokenize(&source_tree);
    if ctx.verbose {
        println!("Lexed: {:#?}", lexed);
    }

    let parsed = Parser::parse_tree(lexed);
    if ctx.verbose {
        println!("Parsed: {:#?}", &parsed);
    }

    let errors = RefCell::new(vec![]);

    let mut resolver = Resolver::default();

    resolver.resolve_program(parsed);

    // let mut analyser = SemanticAnalyser::new(&errors);
    // analyser.init();
    // TODO: do i want to load the environment like this or should the user put a extern module in
    // their source code depending on what environment they want to build for?
    // environment.load(&mut analyser);
    // let analyser = analyser.analyse_program(ast);
    // ast.check_type(&analyser);

    let compiler = JavascriptCompiler::new();
    let compiled = compiler.compile(ast);
    if ctx.verbose {
        println!("{}", compiled);
    }

    compiled
}

fn load_file(path: &Path) -> String {
    read_to_string(path).unwrap_or_else(|_| panic!("Unable to read file {}", path.display()))
}
