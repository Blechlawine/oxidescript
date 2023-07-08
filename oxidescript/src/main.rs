use clap::Parser as ClapParser;
use std::{fs::read_to_string, path::PathBuf};

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


    }
}

fn load_file(path: PathBuf) -> String {
    read_to_string(&path).unwrap_or_else(|_| panic!("Unable to read file {}", path.display()))
}
