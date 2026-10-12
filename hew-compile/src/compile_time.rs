use std::path::{Component, Path, PathBuf};

use hew_types::compile_time::{InputError, InputFile, InputProvider};

#[derive(Debug)]
pub(crate) struct Inputs {
    root: PathBuf,
    documents: super::SourceSet,
}

impl Inputs {
    pub(crate) fn new(root: &str, documents: super::SourceSet) -> Self {
        Self {
            root: PathBuf::from(root),
            documents,
        }
    }
}

pub(super) fn normalize(path: &Path) -> PathBuf {
    let mut result = PathBuf::new();
    for part in path.components() {
        match part {
            Component::CurDir => {}
            Component::ParentDir => {
                if matches!(result.components().next_back(), Some(Component::Normal(_))) {
                    result.pop();
                } else {
                    result.push("..");
                }
            }
            _ => result.push(part.as_os_str()),
        }
    }
    result
}

impl InputProvider for Inputs {
    fn root_source(&self) -> &Path {
        &self.root
    }

    fn include(&self, source: &Path, path: &str) -> Result<InputFile, InputError> {
        let error = |code, message| InputError { code, message };
        let relative = Path::new(path);
        if relative.is_absolute()
            || relative
                .components()
                .any(|part| matches!(part, Component::Prefix(_)))
        {
            return Err(error(
                "E_INCLUDE_OUTSIDE_PACKAGE",
                "include paths must be relative to the source file".into(),
            ));
        }
        let source = if source.is_absolute() {
            source.to_path_buf()
        } else if !self.documents.filesystem {
            Path::new("/").join(source)
        } else {
            std::env::current_dir().unwrap_or_default().join(source)
        };
        let source_dir = source.parent().unwrap_or(Path::new("."));
        let root = source_dir
            .ancestors()
            .find(|dir| {
                let manifest = dir.join("hew.toml");
                self.documents.is_file(&manifest)
            })
            .unwrap_or(source_dir);
        let root = if self.documents.filesystem {
            root.canonicalize().unwrap_or_else(|_| normalize(root))
        } else {
            normalize(root)
        };
        let candidate = source_dir.join(relative);
        let resolved = if self.documents.filesystem {
            super::buffer_identity(&candidate).unwrap_or_else(|| normalize(&candidate))
        } else {
            normalize(&candidate)
        };
        if !resolved.starts_with(&root)
            || (!self.documents.filesystem
                && resolved
                    .components()
                    .any(|part| part == Component::ParentDir))
        {
            return Err(error(
                "E_INCLUDE_OUTSIDE_PACKAGE",
                format!("`{path}` resolves outside its package"),
            ));
        }
        let bytes = self.documents.read_bytes(&resolved).map_err(|cause| {
            error(
                "E_INCLUDE_NOT_FOUND",
                format!("cannot include `{path}`: {cause}"),
            )
        })?;
        Ok(InputFile {
            display_path: resolved
                .strip_prefix(&root)
                .unwrap_or(&resolved)
                .to_string_lossy()
                .replace('\\', "/"),
            path: resolved,
            bytes,
        })
    }

    fn inputs(&self) -> Vec<hew_types::compile_time::CompilationInput> {
        self.documents
            .inputs()
            .into_iter()
            .map(|mut input| {
                let directory = input.path.parent().unwrap_or(Path::new("."));
                let root = directory
                    .ancestors()
                    .find(|dir| self.documents.is_file(&dir.join("hew.toml")))
                    .unwrap_or(directory);
                input.display_path = input
                    .path
                    .strip_prefix(root)
                    .unwrap_or(&input.path)
                    .to_string_lossy()
                    .replace('\\', "/");
                input
            })
            .collect()
    }
    fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
        self.documents.read_bytes(path)
    }
    fn exists(&self, path: &Path) -> bool {
        self.documents.is_file(path)
    }
}
