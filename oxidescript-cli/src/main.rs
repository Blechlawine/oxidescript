use std::{fs::read_to_string, path::PathBuf};

use clap::Parser as ClapParser;
use oxidescript::{
    compiler::JavascriptCompiler,
    lexer::{tokens::Tokens, Lexer},
    parser::Parser,
};

#[derive(clap::Parser, Debug)]
#[command(version)]
struct Args {
    #[arg(last = true)]
    inputs: Vec<String>,

    #[arg(short, long, action = clap::ArgAction::SetTrue)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    for file in args.inputs {
        let loaded_file = load_file(PathBuf::from(file));
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
        println!("{}", compiled);
    }
}

fn load_file(path: PathBuf) -> String {
    read_to_string(&path).unwrap_or_else(|_| panic!("Unable to read file {}", path.display()))
}
