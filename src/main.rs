use clap::Parser as ClapParser;
use std::{fs::read_to_string, path::PathBuf};

mod parser;

#[derive(ClapParser, Debug)]
#[command(version)]
struct Args {
    #[arg(last = true)]
    inputs: Vec<String>,
}

fn main() {
    let args = Args::parse();
    dbg!(&args);

    for file in args.inputs {
        let loaded_file = load_file(PathBuf::from(file));
        dbg!(&loaded_file);
        parser::parse_string(loaded_file).unwrap();
    }
}

fn load_file(path: PathBuf) -> String {
    read_to_string(&path).expect(format!("Unable to read file {}", path.display()).as_str())
}
