use clap::Parser as ClapParser;
use std::{fs::read_to_string, path::PathBuf};
use tree_sitter::Parser;
use tree_sitter_oxidescript::language;

#[derive(ClapParser, Debug)]
#[command(version)]
struct Args {
    #[arg(last = true)]
    inputs: Vec<String>,
}

fn main() {
    let args = Args::parse();

    let mut parser = Parser::new();
    let language = language();
    parser.set_language(language).unwrap();

    for file in args.inputs {
        let loaded_file = load_file(PathBuf::from(file));
        dbg!(&loaded_file);
        let tree = parser.parse(loaded_file, None);
        dbg!(tree);
    }
}

fn load_file(path: PathBuf) -> String {
    read_to_string(&path).expect(format!("Unable to read file {}", path.display()).as_str())
}
