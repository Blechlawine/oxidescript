use std::{
    fs::read_to_string,
    os::unix::process,
    path::{Path, PathBuf},
    process::{exit, Command},
};

use clap::Parser as ClapParser;
use oxidescript::{
    compiler::JavascriptCompiler,
    lexer::{tokens::Tokens, Lexer},
    parser::Parser,
};

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
        outdir: Option<PathBuf>,
    },
    Run {
        #[arg(short, long)]
        with: Option<JavascriptRuntime>,

        #[arg(short, long)]
        devdir: PathBuf,
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

fn main() {
    let args = Args::parse();

    let ctx = Context {
        verbose: args.verbose,
    };

    match args.command {
        OxideCommand::Compile { outdir } => {
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

            let compiled = compile_file(&args.input, &ctx);
            let new_file_name = format!("{}.js", args.input.file_stem().unwrap().to_str().unwrap());
            let compiled_path = outdir.join(new_file_name);
            std::fs::write(compiled_path, compiled).unwrap();
        }
        OxideCommand::Run { with, devdir } => {
            if devdir.exists() {
                std::fs::remove_dir_all(&devdir).unwrap();
            }
            std::fs::create_dir_all(&devdir).unwrap();

            let with = with.unwrap_or_default();

            let compiled = compile_file(&args.input, &ctx);
            let new_file_name = format!("{}.js", args.input.file_stem().unwrap().to_str().unwrap());
            let compiled_path = devdir.join(new_file_name);
            std::fs::write(&compiled_path, compiled).unwrap();

            with.run(&compiled_path).unwrap().wait().unwrap();
        }
    }
}

fn compile_file(path: &Path, ctx: &Context) -> String {
    let loaded_file = load_file(path);
    if ctx.verbose {
        println!("Loaded file: {:?}", &loaded_file);
    }

    let (unlexed, tokens) = Lexer::lex_tokens(loaded_file.as_bytes()).unwrap();
    if ctx.verbose {
        println!("Unlexed: {:?}", unlexed);
        println!("Tokens: {:#?}", tokens);
    }

    let (unparsed, ast) = Parser::parse(Tokens::new(&tokens)).unwrap();
    if ctx.verbose {
        println!("Unparsed: {:?}", unparsed);
        println!("AST: {:#?}", &ast);
    }

    let compiled = JavascriptCompiler::compile(ast);
    if ctx.verbose {
        println!("{}", compiled);
    }

    compiled
}

fn load_file(path: &Path) -> String {
    read_to_string(path).unwrap_or_else(|_| panic!("Unable to read file {}", path.display()))
}
