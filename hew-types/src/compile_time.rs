use std::path::{Path, PathBuf};

/// Host-provided compilation inputs; the checker never reads the filesystem.
pub trait InputProvider: std::fmt::Debug + Send + Sync {
    fn root_source(&self) -> &Path;
    /// Resolve package membership with the frontend's source-input authority.
    ///
    /// # Errors
    /// Returns an invalid package manifest diagnostic.
    fn package_anchor(
        &self,
        source: &Path,
    ) -> Result<crate::module_registry::ModuleAnchor, InputError>;
    /// Resolve and read an include relative to its source.
    ///
    /// # Errors
    /// Rejects paths outside the source package and inputs that cannot be read.
    fn include(&self, source: &Path, path: &str) -> Result<InputFile, InputError>;
    fn inputs(&self) -> Vec<CompilationInput>;
    /// Read an input through the provider.
    ///
    /// # Errors
    /// Returns the input's I/O error if it cannot be read.
    fn read(&self, path: &Path) -> std::io::Result<Vec<u8>>;
    fn exists(&self, path: &Path) -> bool;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationInput {
    pub path: PathBuf,
    pub display_path: String,
    pub sha256: Option<String>,
}

#[derive(Debug, Clone)]
pub struct InputFile {
    pub path: PathBuf,
    pub display_path: String,
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct InputReference {
    pub path: PathBuf,
    pub display_path: String,
    pub size: usize,
}

impl crate::TypeCheckOutput {
    #[must_use]
    pub fn source_input_at(
        &self,
        file: u32,
        offset: usize,
    ) -> Option<(crate::SpanKey, &InputReference)> {
        self.input_references
            .iter()
            .filter(|(span, _)| {
                span.module_idx == file && span.start <= offset && offset < span.end
            })
            .min_by_key(|(span, _)| span.end - span.start)
            .map(|(span, input)| (span.clone(), input))
    }
}

#[derive(Debug, Clone)]
pub struct InputError {
    pub code: &'static str,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Integer(i128),
    Float(f64),
    Duration(i64),
    Bool(bool),
    Char(char),
    Unit,
    String(String),
    Bytes(Vec<u8>),
    Record(Vec<(String, TypedConstant)>),
    Tuple(Vec<TypedConstant>),
    Array(Vec<TypedConstant>),
    Vector(Vec<TypedConstant>),
    Set(Vec<TypedConstant>),
    Map(Vec<(TypedConstant, TypedConstant)>),
    Variant {
        index: usize,
        fields: Vec<(String, TypedConstant)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedConstant {
    pub ty: crate::ResolvedTy,
    pub value: ConstantValue,
}
