//! In-memory source overlays and admitted file-content snapshots for frontend runs.

use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hew_parser::ast::Program;

use crate::compile_time;

/// Source text that overrides the filesystem for one frontend run.
///
/// The LSP, the browser analysis surface and the REPL all check buffers that
/// either have no file behind them or differ from the file on disk. They hand
/// the driver this set instead of running a frontend of their own.
#[derive(Debug, Clone)]
pub struct SourceSet {
    sources: BTreeMap<PathBuf, SourceContent>,
    /// One context world for compiler-prepared syntax retained across live inputs.
    pub(crate) prepared_expansions: Arc<hew_parser::macro_syntax::ExpansionMap>,
    pub(crate) filesystem: bool,
    reads: Arc<std::sync::Mutex<BTreeMap<PathBuf, Option<String>>>>,
    pub(crate) dependency_roots: Arc<std::sync::Mutex<HashSet<PathBuf>>>,
    /// Successful reads retained only within one explicit frontend admission.
    admitted: Option<Arc<std::sync::Mutex<BTreeMap<PathBuf, SourceContent>>>>,
}

impl Default for SourceSet {
    fn default() -> Self {
        Self {
            sources: BTreeMap::new(),
            prepared_expansions: Arc::default(),
            filesystem: true,
            reads: Arc::default(),
            dependency_roots: Arc::default(),
            admitted: None,
        }
    }
}

#[derive(Debug, Clone)]
enum SourceContent {
    Text(Arc<str>),
    Bytes(Arc<[u8]>),
    Prepared {
        source: Arc<str>,
        program: Arc<Program>,
    },
}

impl SourceContent {
    fn bytes(&self) -> &[u8] {
        match self {
            Self::Text(source) | Self::Prepared { source, .. } => source.as_bytes(),
            Self::Bytes(bytes) => bytes,
        }
    }
}

impl SourceSet {
    /// Whether missing documents are read from the host filesystem.
    #[must_use]
    pub fn uses_filesystem(&self) -> bool {
        self.filesystem
    }

    #[must_use]
    pub fn is_file(&self, path: &Path) -> bool {
        self.contains(path) || (self.filesystem && path.is_file())
    }
    #[must_use]
    pub fn in_memory() -> Self {
        Self {
            filesystem: false,
            ..Self::default()
        }
    }

    /// Start a fresh read epoch while preserving the caller's supplied overlays.
    /// Clones inside the run share admitted bytes; later runs never reuse them.
    pub(crate) fn for_frontend(&self) -> Self {
        Self {
            sources: self.sources.clone(),
            prepared_expansions: Arc::clone(&self.prepared_expansions),
            filesystem: self.filesystem,
            reads: Arc::default(),
            dependency_roots: Arc::default(),
            admitted: Some(Arc::default()),
        }
    }

    fn input_identity(&self, path: &Path) -> PathBuf {
        if self.filesystem {
            buffer_identity(path).unwrap_or_else(|| path.to_path_buf())
        } else {
            compile_time::normalize(path)
        }
    }

    fn read_content(&self, path: &Path, text: bool) -> std::io::Result<SourceContent> {
        use sha2::{Digest, Sha256};
        use std::fmt::Write as _;
        let identity = self.input_identity(path);
        let mut admitted = self
            .admitted
            .as_ref()
            .map(|table| table.lock().expect("source admission table poisoned"));
        if let Some(content) = admitted
            .as_ref()
            .and_then(|table| table.get(path).or_else(|| table.get(&identity)))
        {
            return Ok(content.clone());
        }
        self.reads
            .lock()
            .expect("source input table poisoned")
            .insert(identity.clone(), None);
        let content = if let Some(content) = self.content(path) {
            content.clone()
        } else if self.filesystem {
            let bytes = std::fs::read(path)?;
            if text {
                match String::from_utf8(bytes) {
                    Ok(source) => SourceContent::Text(source.into()),
                    Err(error) => SourceContent::Bytes(error.into_bytes().into()),
                }
            } else {
                SourceContent::Bytes(bytes.into())
            }
        } else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "compilation input was not supplied",
            ));
        };
        let fingerprint = Sha256::digest(content.bytes()).iter().fold(
            String::with_capacity(64),
            |mut fingerprint, byte| {
                write!(fingerprint, "{byte:02x}").expect("writing to a String cannot fail");
                fingerprint
            },
        );
        self.reads
            .lock()
            .expect("source input table poisoned")
            .insert(identity.clone(), Some(fingerprint));
        if let Some(admitted) = admitted.as_mut() {
            admitted.insert(identity, content.clone());
            admitted.insert(path.to_path_buf(), content.clone());
        }
        Ok(content)
    }

    /// Read an input and record its content fingerprint.
    ///
    /// # Errors
    /// Returns an I/O error if the input cannot be read or was not supplied.
    ///
    /// # Panics
    /// Panics if a shared input table's mutex is poisoned.
    pub fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
        self.read_content(path, false)
            .map(|content| content.bytes().to_vec())
    }

    /// Read shared UTF-8 text, retaining the exact admitted bytes for this run.
    ///
    /// # Errors
    /// Returns an I/O error for missing inputs or invalid UTF-8.
    ///
    /// # Panics
    /// Panics if a shared input table's mutex is poisoned.
    pub fn read_text(&self, path: &Path) -> std::io::Result<Arc<str>> {
        match self.read_content(path, true)? {
            SourceContent::Text(source) | SourceContent::Prepared { source, .. } => Ok(source),
            SourceContent::Bytes(bytes) => {
                let source: Arc<str> = std::str::from_utf8(&bytes)
                    .map_err(|error| std::io::Error::new(std::io::ErrorKind::InvalidData, error))?
                    .into();
                if let Some(admitted) = &self.admitted {
                    let mut admitted = admitted.lock().expect("source admission table poisoned");
                    admitted.insert(
                        self.input_identity(path),
                        SourceContent::Text(Arc::clone(&source)),
                    );
                    admitted.insert(path.to_path_buf(), SourceContent::Text(Arc::clone(&source)));
                }
                Ok(source)
            }
        }
    }

    fn admitted_content(&self, path: &Path) -> Option<SourceContent> {
        let admitted = self
            .admitted
            .as_ref()?
            .lock()
            .expect("source admission table poisoned");
        admitted
            .get(path)
            .or_else(|| admitted.get(&self.input_identity(path)))
            .cloned()
    }

    /// UTF-8 text from an already observed input; never reads new content.
    pub(crate) fn admitted_text(&self, path: &Path) -> Option<Arc<str>> {
        match self.admitted_content(path)? {
            SourceContent::Text(source) | SourceContent::Prepared { source, .. } => Some(source),
            SourceContent::Bytes(bytes) => std::str::from_utf8(&bytes).ok().map(Arc::from),
        }
    }

    /// Display an observed asset even when its bytes are not valid source text.
    pub(crate) fn admitted_diagnostic_text(&self, path: &Path) -> Option<Arc<str>> {
        match self.admitted_content(path)? {
            SourceContent::Text(source) | SourceContent::Prepared { source, .. } => Some(source),
            SourceContent::Bytes(bytes) => {
                Some(Arc::from(String::from_utf8_lossy(&bytes).as_ref()))
            }
        }
    }

    /// Mutating an admitted clone begins a new provider, preserving the old run.
    fn leave_admission(&mut self) {
        if self.admitted.take().is_some() {
            self.reads = Arc::default();
            self.dependency_roots = Arc::default();
        }
    }

    /// Return the inputs observed by this frontend run.
    ///
    /// # Panics
    ///
    /// Panics if the shared input table's mutex is poisoned.
    #[must_use]
    pub fn inputs(&self) -> Vec<hew_types::compile_time::CompilationInput> {
        let dependency_roots = self
            .dependency_roots
            .lock()
            .expect("source dependency table poisoned");
        self.reads
            .lock()
            .expect("source input table poisoned")
            .iter()
            .map(|(path, sha256)| hew_types::compile_time::CompilationInput {
                path: path.clone(),
                display_path: path.to_string_lossy().replace('\\', "/"),
                sha256: sha256.clone(),
                dependency_root: dependency_roots
                    .iter()
                    .filter(|root| path.starts_with(root))
                    .max_by_key(|root| root.components().count())
                    .cloned(),
            })
            .collect()
    }
    pub fn insert_bytes(&mut self, path: impl Into<PathBuf>, bytes: impl Into<Vec<u8>>) {
        self.leave_admission();
        let mut path = path.into();
        if !self.filesystem {
            path = compile_time::normalize(&path);
        }
        let bytes = SourceContent::Bytes(bytes.into().into());
        if let Some(canonical) = self.filesystem.then(|| buffer_identity(&path)).flatten() {
            self.sources.insert(canonical, bytes.clone());
        }
        self.sources.insert(path, bytes);
    }

    #[must_use]
    pub fn get_bytes(&self, path: &Path) -> Option<&[u8]> {
        match self.content(path)? {
            SourceContent::Text(text) => Some(text.as_bytes()),
            SourceContent::Bytes(bytes) => Some(bytes),
            SourceContent::Prepared { source, .. } => Some(source.as_bytes()),
        }
    }
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Record `source` as the current content of `path`.
    ///
    /// The canonical spelling is recorded alongside the given one because the
    /// import resolver canonicalizes every candidate before loading it.
    pub fn insert(&mut self, path: impl Into<PathBuf>, source: impl Into<String>) {
        self.leave_admission();
        let mut path = path.into();
        if !self.filesystem {
            path = compile_time::normalize(&path);
        }
        let source = SourceContent::Text(source.into().into());
        if let Some(canonical) = self.filesystem.then(|| buffer_identity(&path)).flatten() {
            if canonical != path {
                self.sources.insert(canonical, source.clone());
            }
        }
        self.sources.insert(path, source);
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.sources.is_empty()
    }

    fn contains(&self, path: &Path) -> bool {
        self.content(path).is_some()
            || self.admitted.as_ref().is_some_and(|table| {
                let admitted = table.lock().expect("source admission table poisoned");
                admitted.contains_key(path) || admitted.contains_key(&self.input_identity(path))
            })
    }

    /// The documents directly inside `dir`, so a host without a filesystem
    /// lists a directory module's peers from the sources it was given.
    pub(crate) fn files_in(&self, dir: &Path) -> Vec<PathBuf> {
        let cwd = std::env::current_dir().unwrap_or_default();
        let relative_dir = compile_time::normalize(dir.strip_prefix(&cwd).unwrap_or(dir));
        self.sources
            .keys()
            .filter(|path| {
                path.parent() == Some(dir) || path.parent() == Some(relative_dir.as_path())
            })
            .map(|path| dir.join(path.file_name().expect("a source file has a name")))
            .collect()
    }

    /// The recorded content of `path`, under its given or canonical spelling.
    #[must_use]
    pub fn get(&self, path: &Path) -> Option<&str> {
        match self.content(path)? {
            SourceContent::Text(text) => Some(text),
            SourceContent::Bytes(bytes) => std::str::from_utf8(bytes).ok(),
            SourceContent::Prepared { source, .. } => Some(source),
        }
    }

    /// Retain compiler-prepared syntax with its original diagnostic source.
    /// Session declaration snapshots keep expansion hygiene and source spans;
    /// rendering and reparsing their text would discard those identities.
    pub(crate) fn insert_prepared(&mut self, path: PathBuf, source: String, mut program: Program) {
        if !program.expansions.templates.is_empty() || !program.expansions.expansions.is_empty() {
            self.prepared_expansions = Arc::new(std::mem::take(&mut program.expansions));
        }
        self.insert(path.clone(), source.clone());
        let content = SourceContent::Prepared {
            source: source.into(),
            program: program.into(),
        };
        if let Some(canonical) = self.filesystem.then(|| buffer_identity(&path)).flatten() {
            self.sources.insert(canonical, content.clone());
        }
        self.sources.insert(
            if self.filesystem {
                path
            } else {
                compile_time::normalize(&path)
            },
            content,
        );
    }

    pub(crate) fn retain_prepared_expansions(
        &mut self,
        expansions: hew_parser::macro_syntax::ExpansionMap,
    ) {
        self.prepared_expansions = Arc::new(expansions);
    }

    pub(crate) fn parse_document(&self, path: &Path, source: &str) -> hew_parser::ParseResult {
        if let Some(SourceContent::Prepared {
            source: retained,
            program,
        }) = self.content(path)
        {
            if retained.as_ref() == source {
                return hew_parser::ParseResult {
                    program: (**program).clone(),
                    errors: Vec::new(),
                    contextual_keyword_starts: Vec::new(),
                };
            }
        }
        hew_parser::parse(source)
    }

    fn content(&self, path: &Path) -> Option<&SourceContent> {
        if self.sources.is_empty() {
            return None;
        }
        if let Some(source) = self.sources.get(path) {
            return Some(source);
        }
        if !self.filesystem {
            if let Some(source) = self.sources.get(&compile_time::normalize(path)) {
                return Some(source);
            }
        }
        if self.filesystem {
            if let Some(source) = buffer_identity(path).and_then(|path| self.sources.get(&path)) {
                return Some(source);
            }
        }
        std::env::current_dir()
            .ok()
            .and_then(|cwd| {
                path.strip_prefix(cwd)
                    .ok()
                    .and_then(|path| self.sources.get(path))
            })
            .or_else(|| {
                path.strip_prefix("/")
                    .ok()
                    .and_then(|path| self.sources.get(path))
            })
    }
}

/// Resolve the existing prefix of an input, including missing directories.
pub(crate) fn buffer_identity(path: &Path) -> Option<PathBuf> {
    for ancestor in path.ancestors() {
        if let Ok(canonical) = ancestor.canonicalize() {
            return Some(compile_time::normalize(
                &canonical.join(path.strip_prefix(ancestor).ok()?),
            ));
        }
    }
    None
}

/// The overlay used when a caller supplies no [`crate::FrontendOptions`].
pub(crate) static EMPTY_DOCUMENTS: std::sync::LazyLock<SourceSet> =
    std::sync::LazyLock::new(SourceSet::default);

/// The canonical path of an import candidate, or `None` when nothing supplies
/// it. An open buffer stands in for a file the filesystem does not have, so an
/// editor can check a document set that is not on disk.
pub(crate) fn resolve_candidate(documents: &SourceSet, candidate: &Path) -> Option<PathBuf> {
    if !documents.filesystem {
        return documents
            .contains(candidate)
            .then(|| compile_time::normalize(candidate));
    }
    match candidate.canonicalize() {
        Ok(canonical) => Some(canonical),
        Err(_) => documents
            .contains(candidate)
            .then(|| candidate.to_path_buf()),
    }
}

/// Read a source file, preferring an open buffer over the file on disk.
pub(crate) fn read_source(documents: &SourceSet, path: &Path) -> std::io::Result<Arc<str>> {
    documents.read_text(path)
}
