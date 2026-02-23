/// A simple enum where all variants are unit variants (serialized as strings).
#[derive(Debug)]
pub struct SimpleEnum {
    pub name: String,
    pub variants: Vec<String>,
}

/// A tagged enum with potentially complex variants (externally-tagged serde).
#[derive(Debug)]
pub struct TaggedEnum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub enum EnumVariant {
    Unit { name: String },
    Newtype { name: String, ty: RustType },
    Struct { name: String, fields: Vec<FieldDef> },
    Tuple { name: String, fields: Vec<RustType> },
}

impl EnumVariant {
    pub fn name(&self) -> &str {
        match self {
            Self::Unit { name }
            | Self::Newtype { name, .. }
            | Self::Struct { name, .. }
            | Self::Tuple { name, .. } => name,
        }
    }
}

/// A Rust struct definition.
#[derive(Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
}

/// A field in a struct or struct variant.
#[derive(Debug)]
pub struct FieldDef {
    pub name: String,
    pub ty: RustType,
    pub serde_skip: bool,
    pub serde_default: bool,
    pub serde_rename: Option<String>,
}

/// Rust type representation for code generation.
#[derive(Debug, Clone)]
pub enum RustType {
    String,
    Bool,
    I64,
    U64,
    U32,
    F64,
    Char,
    Usize,
    Vec(Box<RustType>),
    Option(Box<RustType>),
    Box(Box<RustType>),
    Spanned(Box<RustType>),
    Named(String),
    Tuple(Vec<RustType>),
    HashMap(Box<RustType>, Box<RustType>),
    /// Range<usize> → Span
    Range(Box<RustType>),
    PathBuf,
}

/// Top-level type definition extracted from Rust source.
#[derive(Debug)]
pub enum TypeDef {
    SimpleEnum(SimpleEnum),
    TaggedEnum(TaggedEnum),
    Struct(StructDef),
}

impl TypeDef {
    pub fn name(&self) -> &str {
        match self {
            Self::SimpleEnum(e) => &e.name,
            Self::TaggedEnum(e) => &e.name,
            Self::Struct(s) => &s.name,
        }
    }
}
