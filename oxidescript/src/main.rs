use clap::Parser as ClapParser;
use std::{fs::read_to_string, path::PathBuf};

use crate::lexer::Lexer;

pub mod lexer;

#[derive(ClapParser, Debug)]
#[command(version)]
struct Args {
    #[arg(last = true)]
    inputs: Vec<String>,
}

fn main() {
    let args = Args::parse();

    for file in args.inputs {
        let loaded_file = load_file(PathBuf::from(file));
        dbg!(&loaded_file);

        let (unlexed, tokens) = Lexer::lex_tokens(loaded_file.as_bytes()).unwrap();
        dbg!(unlexed);
        dbg!(tokens);
    }
}

fn load_file(path: PathBuf) -> String {
    read_to_string(&path).unwrap_or_else(|_| panic!("Unable to read file {}", path.display()))
}
