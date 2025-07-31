use std::{collections::HashMap, fs::read_to_string, path::PathBuf};

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::tag,
    character::complete::{alpha1, alphanumeric1},
    combinator::{map, recognize},
    multi::many0,
    sequence::pair,
};

use crate::{
    lexer::token::Token,
    parser::ast::{IdentifierReference, Path, Program},
};

/// This struct is responsible for loading all source files in one folder into a sourcetree, to be ready
/// to be parsed
pub struct SourceLoader {
    root_path: PathBuf,
}

pub type SourceTree = HashMap<PathBuf, String>;
/// the key is the tokens of the module path, the value is the tokens of the source code
pub type LexedSourceTree<'src> = HashMap<Vec<Token<'src>>, Vec<Token<'src>>>;
/// the key is the path of the module, the value is the parsed source code of the module
pub type ParsedSourceTree<'src> = HashMap<Path<'src>, Program<'src>>;

pub type ModuleTree<'src> = HashMap<Path<'src>, &'src str>;

impl SourceLoader {
    pub fn new(root_path: PathBuf) -> Self {
        Self { root_path }
    }

    pub fn load(&self) -> SourceTree {
        let walkdir = walkdir::WalkDir::new(&self.root_path);
        let mut output = SourceTree::new();
        for entry in walkdir {
            match entry {
                Ok(entry) => {
                    if !entry.file_type().is_dir() {
                        let file_name = entry.file_name().to_string_lossy();
                        if file_name.ends_with(".os") {
                            // file has correct ending
                            let path = entry.path();
                            let source = match read_to_string(path) {
                                Ok(v) => v,
                                Err(e) => {
                                    eprintln!(
                                        "An error occured when trying to read source file at: {}\n{}",
                                        path.to_string_lossy(),
                                        e
                                    );
                                    continue;
                                }
                            };
                            output.insert(path.to_path_buf(), source);
                        }
                    }
                }
                Err(e) => eprintln!("An error occured when loading the source code: {e}"),
            }
        }
        output
    }

    /// Turn a source tree, which is a map of the file path to the source code, into a module tree,
    /// which is a map of the module path to the source code of the module.
    /// The module tree includes the modules not defined in separate files
    pub fn build_module_tree<'src>(&self, source_tree: &'src SourceTree) -> ModuleTree<'src> {
        source_tree
            .iter()
            .map(|(path, source)| {
                let path_parts = path.iter().flat_map(|p| p.to_str()).collect::<Vec<_>>();
                let module_path = Path::new(
                    path_parts
                        .into_iter()
                        .map(|p| {
                            let (rest, parsed) = parse_module_identifier(p).unwrap();
                            assert!(rest.is_empty());
                            parsed
                        })
                        .collect::<Vec<_>>(),
                );
                (module_path, source.as_str())
            })
            .collect()
    }
}

fn parse_module_identifier(input: &str) -> IResult<&str, IdentifierReference> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: &str| IdentifierReference { name: s, id: None },
    )
    .parse(input)
}
