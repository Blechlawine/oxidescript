use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
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
    #[arg(short, long, action = clap::ArgAction::Append)]
    inputs: Vec<String>,

    #[arg(short, long, action = clap::ArgAction::SetTrue)]
    verbose: bool,

    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Command {
    /// Compile files
    Compile {
        #[arg(short, long)]
        outdir: PathBuf,
    },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Compile { outdir } => {
            if outdir.exists() {
                std::fs::remove_dir_all(&outdir).unwrap();
            }
            std::fs::create_dir_all(&outdir).unwrap();

            for file in args.inputs {
                let file_path = PathBuf::from(&file);
                let loaded_file = load_file(&file_path);
                if args.verbose {
                    println!("Loaded file: {:?}", &loaded_file);
                }

                let (unlexed, tokens) = Lexer::lex_tokens(loaded_file.as_bytes()).unwrap();
                if args.verbose {
                    println!("Unlexed: {:?}", unlexed);
                    println!("Tokens: {:#?}", tokens);
                }

                let (unparsed, ast) = Parser::parse(Tokens::new(&tokens)).unwrap();
                if args.verbose {
                    println!("Unparsed: {:?}", unparsed);
                    println!("AST: {:#?}", &ast);
                }

                let compiled = JavascriptCompiler::compile(ast);
                if args.verbose {
                    println!("{}", compiled);
                }

                let new_file_name =
                    format!("{}.js", file_path.file_stem().unwrap().to_str().unwrap());
                let compiled_path = outdir.join(new_file_name);
                std::fs::write(compiled_path, compiled).unwrap();
            }
        }
    }
}

fn load_file(path: &Path) -> String {
    read_to_string(path).unwrap_or_else(|_| panic!("Unable to read file {}", path.display()))
}
