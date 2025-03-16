use std::{collections::HashMap, fs::read_to_string, path::PathBuf};

/// This struct is responsible for loading all source files in one folder into a sourcetree, to be ready
/// to be parsed
pub struct SourceLoader {
    root_path: PathBuf,
}

pub type SourceTree = HashMap<PathBuf, String>;

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
}
