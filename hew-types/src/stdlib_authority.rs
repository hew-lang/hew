//! Build-time authority extracted from the closed stdlib substrate surface.
//!
//! The compiler binary embeds the substrate sources with [`include_str!`].
//! [`STDLIB_AUTHORITY`] parses them lazily with `hew_parser`, validates the
//! compiler-recognised attribute vocabularies, and exposes typed facts without
//! changing any existing consumer in this foundation lane.

use std::collections::BTreeMap;
use std::fmt;
use std::sync::LazyLock;

use hew_parser::ast::{
    Attribute, AttributeArg, FnDecl, ImportSpec, Item, TraitItem, TypeBodyItem, TypeDeclKind,
    TypeExpr,
};
use strum::{EnumIter, IntoEnumIterator};

use crate::LangItem;

pub(crate) mod codegen;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StdlibRoot {
    Builtins,
    Option,
    Result,
    Failure,
    LinkMonitor,
    String,
    Io,
    Prelude,
}

impl StdlibRoot {
    #[must_use]
    pub const fn module_name(self) -> &'static str {
        match self {
            Self::Builtins => "builtins",
            Self::Option => "option",
            Self::Result => "result",
            Self::Failure => "failure",
            Self::LinkMonitor => "link_monitor",
            Self::String => "string",
            Self::Io => "io",
            Self::Prelude => "prelude",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AuthoritySource<'a> {
    pub root: Option<StdlibRoot>,
    pub path: &'a str,
    pub source: &'a str,
}

impl<'a> AuthoritySource<'a> {
    #[must_use]
    pub const fn embedded(root: StdlibRoot, path: &'a str, source: &'a str) -> Self {
        Self {
            root: Some(root),
            path,
            source,
        }
    }

    #[must_use]
    pub const fn external(path: &'a str, source: &'a str) -> Self {
        Self {
            root: None,
            path,
            source,
        }
    }
}

pub const SUBSTRATE_SOURCES: &[AuthoritySource<'static>] = &[
    AuthoritySource::embedded(
        StdlibRoot::Builtins,
        "std/builtins.hew",
        include_str!("../../std/builtins.hew"),
    ),
    AuthoritySource::embedded(
        StdlibRoot::Option,
        "std/option.hew",
        include_str!("../../std/option.hew"),
    ),
    AuthoritySource::embedded(
        StdlibRoot::Result,
        "std/result.hew",
        include_str!("../../std/result.hew"),
    ),
    AuthoritySource::embedded(
        StdlibRoot::Failure,
        "std/failure.hew",
        include_str!("../../std/failure.hew"),
    ),
    AuthoritySource::embedded(
        StdlibRoot::LinkMonitor,
        "std/link_monitor.hew",
        include_str!("../../std/link_monitor.hew"),
    ),
    AuthoritySource::embedded(
        StdlibRoot::String,
        "std/string.hew",
        include_str!("../../std/string.hew"),
    ),
    AuthoritySource::embedded(
        StdlibRoot::Io,
        "std/io.hew",
        include_str!("../../std/io.hew"),
    ),
    AuthoritySource::embedded(
        StdlibRoot::Prelude,
        "std/prelude.hew",
        include_str!("../../std/prelude.hew"),
    ),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AuthorityDeclarationKind {
    Type,
    Trait,
    TraitMethod,
    Function,
    Method,
    ExternFunction,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuthorityBinding {
    pub root: StdlibRoot,
    pub declaration: String,
    pub kind: AuthorityDeclarationKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter)]
pub enum Intrinsic {
    MathExp,
    MathLog,
    MathSqrt,
    MathSin,
    MathCos,
    MathFloor,
    MathCeil,
    MathAbs,
    MathTanh,
    MathLog2,
    MathLog10,
    MathExp2,
    MathPow,
    MathMax,
    MathMin,
    MathPi,
    MathE,
    MemAlloc,
    MemRealloc,
    MemDealloc,
    MemPtrOffset,
    MemPtrCopy,
}

impl Intrinsic {
    #[must_use]
    pub const fn key(self) -> &'static str {
        match self {
            Self::MathExp => "math.exp",
            Self::MathLog => "math.log",
            Self::MathSqrt => "math.sqrt",
            Self::MathSin => "math.sin",
            Self::MathCos => "math.cos",
            Self::MathFloor => "math.floor",
            Self::MathCeil => "math.ceil",
            Self::MathAbs => "math.abs",
            Self::MathTanh => "math.tanh",
            Self::MathLog2 => "math.log2",
            Self::MathLog10 => "math.log10",
            Self::MathExp2 => "math.exp2",
            Self::MathPow => "math.pow",
            Self::MathMax => "math.max",
            Self::MathMin => "math.min",
            Self::MathPi => "math.pi",
            Self::MathE => "math.e",
            Self::MemAlloc => "mem.alloc",
            Self::MemRealloc => "mem.realloc",
            Self::MemDealloc => "mem.dealloc",
            Self::MemPtrOffset => "mem.ptr_offset",
            Self::MemPtrCopy => "mem.ptr_copy",
        }
    }

    #[must_use]
    pub fn from_key(key: &str) -> Option<Self> {
        Self::iter().find(|intrinsic| intrinsic.key() == key)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternAbiFact {
    ReturnBytesTriple,
    BytesParamPointer,
    CowDropNullTolerant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternAbiEntry {
    pub binding: AuthorityBinding,
    pub facts: Vec<ExternAbiFact>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OverloadGroup {
    Println,
    Print,
    ToString,
    AssertEq,
    AssertNe,
    Len,
}

impl OverloadGroup {
    pub const ALL: [Self; 6] = [
        Self::Println,
        Self::Print,
        Self::ToString,
        Self::AssertEq,
        Self::AssertNe,
        Self::Len,
    ];

    #[must_use]
    pub const fn key(self) -> &'static str {
        match self {
            Self::Println => "println",
            Self::Print => "print",
            Self::ToString => "to_string",
            Self::AssertEq => "assert_eq",
            Self::AssertNe => "assert_ne",
            Self::Len => "len",
        }
    }

    #[must_use]
    pub fn from_key(key: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|group| group.key() == key)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticItem {
    Fs,
    Json,
}

impl DiagnosticItem {
    pub const ALL: [Self; 2] = [Self::Fs, Self::Json];

    #[must_use]
    pub const fn key(self) -> &'static str {
        match self {
            Self::Fs => "fs",
            Self::Json => "json",
        }
    }

    #[must_use]
    pub fn from_key(key: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|item| item.key() == key)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariantOrder {
    pub root: StdlibRoot,
    pub declaration: String,
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreludeExportKind {
    Module,
    Glob,
    Item,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreludeExport {
    pub module: String,
    pub name: Option<String>,
    pub alias: Option<String>,
    pub kind: PreludeExportKind,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct StdlibAuthority {
    lang_items: BTreeMap<LangItem, AuthorityBinding>,
    intrinsics: BTreeMap<Intrinsic, AuthorityBinding>,
    extern_abi: BTreeMap<String, ExternAbiEntry>,
    overload_groups: BTreeMap<OverloadGroup, Vec<AuthorityBinding>>,
    diagnostic_items: BTreeMap<DiagnosticItem, AuthorityBinding>,
    enum_variant_orders: BTreeMap<String, EnumVariantOrder>,
    prelude_exports: Vec<PreludeExport>,
}

impl StdlibAuthority {
    #[must_use]
    pub fn lang_items(&self) -> &BTreeMap<LangItem, AuthorityBinding> {
        &self.lang_items
    }

    #[must_use]
    pub fn intrinsics(&self) -> &BTreeMap<Intrinsic, AuthorityBinding> {
        &self.intrinsics
    }

    #[must_use]
    pub fn extern_abi(&self) -> &BTreeMap<String, ExternAbiEntry> {
        &self.extern_abi
    }

    #[must_use]
    pub fn overload_groups(&self) -> &BTreeMap<OverloadGroup, Vec<AuthorityBinding>> {
        &self.overload_groups
    }

    #[must_use]
    pub fn diagnostic_items(&self) -> &BTreeMap<DiagnosticItem, AuthorityBinding> {
        &self.diagnostic_items
    }

    #[must_use]
    pub fn enum_variant_orders(&self) -> &BTreeMap<String, EnumVariantOrder> {
        &self.enum_variant_orders
    }

    #[must_use]
    pub fn prelude_exports(&self) -> &[PreludeExport] {
        &self.prelude_exports
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthorityErrorKind {
    Parse { diagnostics: Vec<String> },
    AttributeOutsideStdRoot { attribute: String },
    InvalidAttributePlacement { attribute: String },
    MalformedAttribute { attribute: String },
    UnknownLangItem { key: String },
    UnknownIntrinsic { key: String },
    UnknownAbiKey { key: String },
    UnknownAbiValue { key: String, value: String },
    UnknownOverloadGroup { key: String },
    UnknownDiagnosticItem { key: String },
    DuplicateBinding { family: String, key: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuthorityError {
    pub source_path: String,
    pub declaration: Option<String>,
    pub kind: AuthorityErrorKind,
}

impl fmt::Display for AuthorityError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "stdlib authority error in {}", self.source_path)?;
        if let Some(declaration) = &self.declaration {
            write!(f, " at `{declaration}`")?;
        }
        write!(f, ": ")?;
        match &self.kind {
            AuthorityErrorKind::Parse { diagnostics } => {
                write!(f, "source failed to parse: {}", diagnostics.join("; "))
            }
            AuthorityErrorKind::AttributeOutsideStdRoot { attribute } => write!(
                f,
                "`#[{attribute}]` is substrate-only and cannot be declared outside a known stdlib root"
            ),
            AuthorityErrorKind::InvalidAttributePlacement { attribute } => {
                write!(f, "`#[{attribute}]` is not valid on this declaration form")
            }
            AuthorityErrorKind::MalformedAttribute { attribute } => {
                write!(f, "`#[{attribute}]` has an invalid argument shape")
            }
            AuthorityErrorKind::UnknownLangItem { key } => {
                write!(f, "unknown `#[lang_item]` key `{key}`")
            }
            AuthorityErrorKind::UnknownIntrinsic { key } => {
                write!(f, "unknown `#[intrinsic]` key `{key}`")
            }
            AuthorityErrorKind::UnknownAbiKey { key } => {
                write!(f, "unknown `#[abi]` key `{key}`")
            }
            AuthorityErrorKind::UnknownAbiValue { key, value } => {
                write!(f, "unknown `#[abi]` value `{value}` for key `{key}`")
            }
            AuthorityErrorKind::UnknownOverloadGroup { key } => {
                write!(f, "unknown `#[overload]` group `{key}`")
            }
            AuthorityErrorKind::UnknownDiagnosticItem { key } => {
                write!(f, "unknown `#[diagnostic_item]` key `{key}`")
            }
            AuthorityErrorKind::DuplicateBinding { family, key } => {
                write!(f, "duplicate {family} binding for key `{key}`")
            }
        }
    }
}

impl std::error::Error for AuthorityError {}

pub static STDLIB_AUTHORITY: LazyLock<StdlibAuthority> = LazyLock::new(|| {
    load_stdlib_authority(SUBSTRATE_SOURCES)
        .unwrap_or_else(|error| panic!("failed to build stdlib authority: {error}"))
});

#[must_use]
pub fn authority() -> &'static StdlibAuthority {
    &STDLIB_AUTHORITY
}

/// Parse and validate a set of authority sources.
///
/// Recognised authority attributes on an [`AuthoritySource::external`] input
/// are rejected before their payload is considered.
///
/// # Errors
///
/// Returns an [`AuthorityError`] when a source does not parse, an authority
/// attribute is outside the known stdlib roots, or a recognised key/value is
/// malformed, unknown, misplaced, or duplicated.
pub fn load_stdlib_authority(
    sources: &[AuthoritySource<'_>],
) -> Result<StdlibAuthority, AuthorityError> {
    let mut authority = StdlibAuthority::default();
    for source in sources {
        load_source(&mut authority, *source)?;
    }
    Ok(authority)
}

#[allow(
    clippy::too_many_lines,
    reason = "the exhaustive AST declaration walk keeps attribute placement rules together"
)]
fn load_source(
    authority: &mut StdlibAuthority,
    source: AuthoritySource<'_>,
) -> Result<(), AuthorityError> {
    let parsed = hew_parser::parse(source.source);
    if !parsed.errors.is_empty() {
        return Err(error(
            source,
            None,
            AuthorityErrorKind::Parse {
                diagnostics: parsed
                    .errors
                    .into_iter()
                    .map(|diagnostic| diagnostic.message)
                    .collect(),
            },
        ));
    }

    for (item, _) in parsed.program.items {
        match item {
            Item::TypeDecl(decl) => {
                if let Some(key) = decl.lang_item.as_deref() {
                    ensure_known_root(source, &decl.name, "lang_item")?;
                    let type_binding =
                        make_binding(source, decl.name.clone(), AuthorityDeclarationKind::Type)?;
                    register_lang_item(authority, source, &type_binding, key)?;
                }
                if let (TypeDeclKind::Enum, Some(root)) = (decl.kind, source.root) {
                    let variants = decl
                        .body
                        .iter()
                        .filter_map(|item| match item {
                            TypeBodyItem::Variant(variant) => Some(variant.name.clone()),
                            _ => None,
                        })
                        .collect();
                    let qualified = qualified(source, &decl.name);
                    authority.enum_variant_orders.insert(
                        qualified,
                        EnumVariantOrder {
                            root,
                            declaration: decl.name.clone(),
                            variants,
                        },
                    );
                }
                for item in decl.body {
                    if let TypeBodyItem::Method(method) = item {
                        let declaration = format!("{}::{}", decl.name, method.name);
                        load_function_attributes(
                            authority,
                            source,
                            &method,
                            declaration,
                            AuthorityDeclarationKind::Method,
                            false,
                        )?;
                    }
                }
            }
            Item::Trait(decl) => {
                if let Some(key) = decl.lang_item.as_deref() {
                    ensure_known_root(source, &decl.name, "lang_item")?;
                    let trait_binding =
                        make_binding(source, decl.name.clone(), AuthorityDeclarationKind::Trait)?;
                    register_lang_item(authority, source, &trait_binding, key)?;
                }
                for item in decl.items {
                    if let TraitItem::Method(method) = item {
                        if let Some(key) = method.lang_item.as_deref() {
                            let declaration = format!("{}::{}", decl.name, method.name);
                            ensure_known_root(source, &declaration, "lang_item")?;
                            let method_binding = make_binding(
                                source,
                                declaration,
                                AuthorityDeclarationKind::TraitMethod,
                            )?;
                            register_lang_item(authority, source, &method_binding, key)?;
                        }
                    }
                }
            }
            Item::Impl(decl) => {
                let owner = type_name(&decl.target_type.0);
                for method in decl.methods {
                    let declaration = format!("{owner}::{}", method.name);
                    load_function_attributes(
                        authority,
                        source,
                        &method,
                        declaration,
                        AuthorityDeclarationKind::Method,
                        false,
                    )?;
                }
            }
            Item::Function(function) => {
                let declaration = function.name.clone();
                load_function_attributes(
                    authority,
                    source,
                    &function,
                    declaration,
                    AuthorityDeclarationKind::Function,
                    true,
                )?;
            }
            Item::ExternBlock(block) => {
                for function in block.functions {
                    let declaration = function.name.clone();
                    for attr in &function.attributes {
                        ensure_substrate_attribute(source, &declaration, attr)?;
                        match attr.name.as_str() {
                            "abi" => {
                                let facts = parse_abi_facts(source, &declaration, attr)?;
                                let binding = make_binding(
                                    source,
                                    declaration.clone(),
                                    AuthorityDeclarationKind::ExternFunction,
                                )?;
                                let key = qualified(source, &declaration);
                                if authority
                                    .extern_abi
                                    .insert(key.clone(), ExternAbiEntry { binding, facts })
                                    .is_some()
                                {
                                    return Err(error(
                                        source,
                                        Some(declaration),
                                        AuthorityErrorKind::DuplicateBinding {
                                            family: "extern ABI".to_string(),
                                            key,
                                        },
                                    ));
                                }
                            }
                            "lang_item" | "intrinsic" | "diagnostic_item" | "overload" => {
                                return Err(error(
                                    source,
                                    Some(declaration),
                                    AuthorityErrorKind::InvalidAttributePlacement {
                                        attribute: attr.name.clone(),
                                    },
                                ));
                            }
                            _ => {}
                        }
                    }
                }
            }
            Item::Import(import) if source.root == Some(StdlibRoot::Prelude) => {
                collect_prelude_export(&mut authority.prelude_exports, import);
            }
            Item::Import(_)
            | Item::Const(_)
            | Item::TypeAlias(_)
            | Item::Actor(_)
            | Item::Supervisor(_)
            | Item::Machine(_)
            | Item::Record(_) => {}
        }
    }
    Ok(())
}

fn load_function_attributes(
    authority: &mut StdlibAuthority,
    source: AuthoritySource<'_>,
    function: &FnDecl,
    declaration: String,
    kind: AuthorityDeclarationKind,
    top_level: bool,
) -> Result<(), AuthorityError> {
    for attr in &function.attributes {
        ensure_substrate_attribute(source, &declaration, attr)?;
    }
    if source.root.is_none() {
        return Ok(());
    }
    let binding = make_binding(source, declaration.clone(), kind)?;
    for attr in &function.attributes {
        match attr.name.as_str() {
            "lang_item" => {
                let key = positional_key(source, &declaration, attr)?;
                register_lang_item(authority, source, &binding, key)?;
            }
            "intrinsic" => {
                if !top_level {
                    return Err(error(
                        source,
                        Some(declaration),
                        AuthorityErrorKind::InvalidAttributePlacement {
                            attribute: attr.name.clone(),
                        },
                    ));
                }
                let key = positional_key(source, &declaration, attr)?;
                let intrinsic = Intrinsic::from_key(key).ok_or_else(|| {
                    error(
                        source,
                        Some(declaration.clone()),
                        AuthorityErrorKind::UnknownIntrinsic {
                            key: key.to_string(),
                        },
                    )
                })?;
                insert_unique(
                    &mut authority.intrinsics,
                    intrinsic,
                    binding.clone(),
                    source,
                    &declaration,
                    "intrinsic",
                    key,
                )?;
            }
            "diagnostic_item" => {
                let key = positional_key(source, &declaration, attr)?;
                let diagnostic = DiagnosticItem::from_key(key).ok_or_else(|| {
                    error(
                        source,
                        Some(declaration.clone()),
                        AuthorityErrorKind::UnknownDiagnosticItem {
                            key: key.to_string(),
                        },
                    )
                })?;
                insert_unique(
                    &mut authority.diagnostic_items,
                    diagnostic,
                    binding.clone(),
                    source,
                    &declaration,
                    "diagnostic item",
                    key,
                )?;
            }
            "overload" => {
                let key = positional_key(source, &declaration, attr)?;
                let group = OverloadGroup::from_key(key).ok_or_else(|| {
                    error(
                        source,
                        Some(declaration.clone()),
                        AuthorityErrorKind::UnknownOverloadGroup {
                            key: key.to_string(),
                        },
                    )
                })?;
                authority
                    .overload_groups
                    .entry(group)
                    .or_default()
                    .push(binding.clone());
            }
            "abi" => {
                return Err(error(
                    source,
                    Some(declaration),
                    AuthorityErrorKind::InvalidAttributePlacement {
                        attribute: attr.name.clone(),
                    },
                ));
            }
            _ => {}
        }
    }
    Ok(())
}

fn register_lang_item(
    authority: &mut StdlibAuthority,
    source: AuthoritySource<'_>,
    binding: &AuthorityBinding,
    key: &str,
) -> Result<(), AuthorityError> {
    let item = LangItem::from_key(key).ok_or_else(|| {
        error(
            source,
            Some(binding.declaration.clone()),
            AuthorityErrorKind::UnknownLangItem {
                key: key.to_string(),
            },
        )
    })?;
    insert_unique(
        &mut authority.lang_items,
        item,
        binding.clone(),
        source,
        &binding.declaration,
        "lang item",
        key,
    )
}

fn parse_abi_facts(
    source: AuthoritySource<'_>,
    declaration: &str,
    attr: &Attribute,
) -> Result<Vec<ExternAbiFact>, AuthorityError> {
    if attr.args.is_empty() {
        return Err(error(
            source,
            Some(declaration.to_string()),
            AuthorityErrorKind::MalformedAttribute {
                attribute: attr.name.clone(),
            },
        ));
    }

    let mut facts = Vec::with_capacity(attr.args.len());
    for arg in &attr.args {
        let AttributeArg::KeyValue { key, value } = arg else {
            return Err(error(
                source,
                Some(declaration.to_string()),
                AuthorityErrorKind::MalformedAttribute {
                    attribute: attr.name.clone(),
                },
            ));
        };
        let fact = match (key.as_str(), value.as_str()) {
            ("ret", "bytes_triple") => ExternAbiFact::ReturnBytesTriple,
            ("bytes_param", "ptr") => ExternAbiFact::BytesParamPointer,
            ("drop", "cow_null_tolerant") => ExternAbiFact::CowDropNullTolerant,
            ("ret" | "bytes_param" | "drop", _) => {
                return Err(error(
                    source,
                    Some(declaration.to_string()),
                    AuthorityErrorKind::UnknownAbiValue {
                        key: key.clone(),
                        value: value.clone(),
                    },
                ));
            }
            _ => {
                return Err(error(
                    source,
                    Some(declaration.to_string()),
                    AuthorityErrorKind::UnknownAbiKey { key: key.clone() },
                ));
            }
        };
        facts.push(fact);
    }
    Ok(facts)
}

fn positional_key<'a>(
    source: AuthoritySource<'_>,
    declaration: &str,
    attr: &'a Attribute,
) -> Result<&'a str, AuthorityError> {
    match attr.args.as_slice() {
        [AttributeArg::Positional(key)] if !key.is_empty() => Ok(key),
        _ => Err(error(
            source,
            Some(declaration.to_string()),
            AuthorityErrorKind::MalformedAttribute {
                attribute: attr.name.clone(),
            },
        )),
    }
}

fn ensure_substrate_attribute(
    source: AuthoritySource<'_>,
    declaration: &str,
    attr: &Attribute,
) -> Result<(), AuthorityError> {
    if source.root.is_none()
        && matches!(
            attr.name.as_str(),
            "lang_item" | "abi" | "intrinsic" | "diagnostic_item" | "overload"
        )
    {
        return Err(error(
            source,
            Some(declaration.to_string()),
            AuthorityErrorKind::AttributeOutsideStdRoot {
                attribute: attr.name.clone(),
            },
        ));
    }
    Ok(())
}

fn ensure_known_root(
    source: AuthoritySource<'_>,
    declaration: &str,
    attribute: &str,
) -> Result<(), AuthorityError> {
    if source.root.is_none() {
        return Err(error(
            source,
            Some(declaration.to_string()),
            AuthorityErrorKind::AttributeOutsideStdRoot {
                attribute: attribute.to_string(),
            },
        ));
    }
    Ok(())
}

fn insert_unique<K: Ord>(
    map: &mut BTreeMap<K, AuthorityBinding>,
    key: K,
    binding: AuthorityBinding,
    source: AuthoritySource<'_>,
    declaration: &str,
    family: &str,
    display_key: &str,
) -> Result<(), AuthorityError> {
    if map.insert(key, binding).is_some() {
        return Err(error(
            source,
            Some(declaration.to_string()),
            AuthorityErrorKind::DuplicateBinding {
                family: family.to_string(),
                key: display_key.to_string(),
            },
        ));
    }
    Ok(())
}

fn make_binding(
    source: AuthoritySource<'_>,
    declaration: String,
    kind: AuthorityDeclarationKind,
) -> Result<AuthorityBinding, AuthorityError> {
    let root = source.root.ok_or_else(|| {
        error(
            source,
            Some(declaration.clone()),
            AuthorityErrorKind::AttributeOutsideStdRoot {
                attribute: "authority".to_string(),
            },
        )
    })?;
    Ok(AuthorityBinding {
        root,
        declaration,
        kind,
    })
}

fn qualified(source: AuthoritySource<'_>, declaration: &str) -> String {
    source.root.map_or_else(
        || declaration.to_string(),
        |root| format!("{}::{declaration}", root.module_name()),
    )
}

fn type_name(ty: &TypeExpr) -> String {
    match ty {
        TypeExpr::Named { name, .. } => name.clone(),
        _ => "<unnamed>".to_string(),
    }
}

fn collect_prelude_export(exports: &mut Vec<PreludeExport>, import: hew_parser::ast::ImportDecl) {
    let module = import.path.join("::");
    match import.spec {
        None => exports.push(PreludeExport {
            module,
            name: None,
            alias: import.module_alias,
            kind: PreludeExportKind::Module,
        }),
        Some(ImportSpec::Glob) => exports.push(PreludeExport {
            module,
            name: None,
            alias: None,
            kind: PreludeExportKind::Glob,
        }),
        Some(ImportSpec::Names(names)) => {
            for name in names {
                exports.push(PreludeExport {
                    module: module.clone(),
                    name: Some(name.name),
                    alias: name.alias,
                    kind: PreludeExportKind::Item,
                });
            }
        }
    }
}

fn error(
    source: AuthoritySource<'_>,
    declaration: Option<String>,
    kind: AuthorityErrorKind,
) -> AuthorityError {
    AuthorityError {
        source_path: source.path.to_string(),
        declaration,
        kind,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_intrinsic_key_round_trips() {
        for intrinsic in Intrinsic::iter() {
            assert_eq!(Intrinsic::from_key(intrinsic.key()), Some(intrinsic));
        }
    }

    #[test]
    fn embedded_substrate_populates_existing_authorities() {
        let authority = load_stdlib_authority(SUBSTRATE_SOURCES)
            .expect("embedded stdlib authority sources must load");

        for key in [
            LangItem::Index,
            LangItem::IndexGet,
            LangItem::IndexAt,
            LangItem::Display,
            LangItem::DisplayFmt,
        ] {
            assert!(
                authority.lang_items().contains_key(&key),
                "missing existing lang item {}",
                key.key()
            );
        }
        assert!(
            authority
                .enum_variant_orders()
                .contains_key("builtins::SendError"),
            "builtins enum orders must be collected"
        );
        assert!(
            authority
                .enum_variant_orders()
                .contains_key("failure::CrashAction"),
            "failure enum orders must be collected"
        );
    }

    #[test]
    fn synthetic_sources_populate_every_registry() {
        let sources = [
            AuthoritySource::embedded(
                StdlibRoot::Builtins,
                "std/builtins.hew",
                r#"
#[lang_item("option")]
pub enum Maybe<T> { Some(T); None; }

#[intrinsic("math.sqrt")]
pub fn sqrt(x: f64) -> f64;

#[diagnostic_item("fs")]
pub fn read_file() {}

#[overload("println")]
pub fn println_i64(value: i64) {}

extern "C" {
    #[abi(ret = bytes_triple, bytes_param = ptr, drop = cow_null_tolerant)]
    fn hew_bytes();
}
"#,
            ),
            AuthoritySource::embedded(
                StdlibRoot::Prelude,
                "std/prelude.hew",
                "import std::builtins::{ Maybe as Option };\n",
            ),
        ];

        let authority =
            load_stdlib_authority(&sources).expect("synthetic authority sources must load");
        assert_eq!(
            authority.lang_items()[&LangItem::Option].declaration,
            "Maybe"
        );
        assert_eq!(
            authority.intrinsics()[&Intrinsic::MathSqrt].declaration,
            "sqrt"
        );
        assert_eq!(
            authority.diagnostic_items()[&DiagnosticItem::Fs].declaration,
            "read_file"
        );
        assert_eq!(
            authority.overload_groups()[&OverloadGroup::Println][0].declaration,
            "println_i64"
        );
        assert_eq!(
            authority.extern_abi()["builtins::hew_bytes"].facts,
            vec![
                ExternAbiFact::ReturnBytesTriple,
                ExternAbiFact::BytesParamPointer,
                ExternAbiFact::CowDropNullTolerant,
            ]
        );
        assert_eq!(
            authority.enum_variant_orders()["builtins::Maybe"].variants,
            vec!["Some".to_string(), "None".to_string()]
        );
        assert_eq!(
            authority.prelude_exports(),
            &[PreludeExport {
                module: "std::builtins".to_string(),
                name: Some("Maybe".to_string()),
                alias: Some("Option".to_string()),
                kind: PreludeExportKind::Item,
            }]
        );
    }

    #[test]
    fn unknown_lang_item_key_is_a_pointed_build_error() {
        let source = AuthoritySource::embedded(
            StdlibRoot::Builtins,
            "std/builtins.hew",
            r#"#[lang_item("display_typo")] pub trait Display {}"#,
        );
        let error =
            load_stdlib_authority(&[source]).expect_err("unknown lang-item keys must fail closed");

        assert_eq!(error.source_path, "std/builtins.hew");
        assert_eq!(error.declaration.as_deref(), Some("Display"));
        assert_eq!(
            error.kind,
            AuthorityErrorKind::UnknownLangItem {
                key: "display_typo".to_string(),
            }
        );
        assert!(
            error.to_string().contains("display_typo"),
            "build error must name the rejected key: {error}"
        );
    }

    #[test]
    fn unknown_abi_key_is_a_pointed_build_error() {
        let source = AuthoritySource::embedded(
            StdlibRoot::Io,
            "std/io.hew",
            r#"
extern "C" {
    #[abi(byte_param = ptr)]
    fn hew_write(data: bytes);
}
"#,
        );
        let error =
            load_stdlib_authority(&[source]).expect_err("unknown ABI keys must fail closed");

        assert_eq!(error.declaration.as_deref(), Some("hew_write"));
        assert_eq!(
            error.kind,
            AuthorityErrorKind::UnknownAbiKey {
                key: "byte_param".to_string(),
            }
        );
        assert!(
            error.to_string().contains("byte_param"),
            "build error must name the rejected ABI key: {error}"
        );
    }

    #[test]
    fn authority_attribute_outside_std_roots_is_rejected() {
        let source = AuthoritySource::external(
            "src/main.hew",
            r#"#[diagnostic_item("fs")] pub fn read_file() {}"#,
        );
        let error = load_stdlib_authority(&[source])
            .expect_err("authority attributes outside std roots must fail closed");

        assert_eq!(error.source_path, "src/main.hew");
        assert_eq!(error.declaration.as_deref(), Some("read_file"));
        assert_eq!(
            error.kind,
            AuthorityErrorKind::AttributeOutsideStdRoot {
                attribute: "diagnostic_item".to_string(),
            }
        );
        assert!(
            error.to_string().contains("substrate-only"),
            "outside-root error must explain the privilege boundary: {error}"
        );
    }
}
