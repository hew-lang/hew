use std::fmt;

use crate::model::{EnumVariant, FieldDef, RustType, SimpleEnum, StructDef, TaggedEnum, TypeDef};
use syn::spanned::Spanned;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    InvalidRustSource {
        source_name: String,
        message: String,
    },
    UnsupportedStructShape {
        source_name: String,
        line: usize,
        struct_name: String,
        shape: UnsupportedStructShape,
    },
    UnnamedField {
        source_name: String,
        line: usize,
        container: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnsupportedStructShape {
    Tuple,
    Unit,
}

impl fmt::Display for UnsupportedStructShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Tuple => f.write_str("tuple struct"),
            Self::Unit => f.write_str("unit struct"),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidRustSource {
                source_name,
                message,
            } => write!(f, "{source_name}: failed to parse Rust source: {message}"),
            Self::UnsupportedStructShape {
                source_name,
                line,
                struct_name,
                shape,
            } => write!(
                f,
                "{source_name}:{line}: unsupported {shape} `{struct_name}`; hew-astgen only supports named-field structs"
            ),
            Self::UnnamedField {
                source_name,
                line,
                container,
            } => write!(
                f,
                "{source_name}:{line}: unnamed field in {container}; hew-astgen requires named fields"
            ),
        }
    }
}

impl std::error::Error for ParseError {}

/// Parse a Rust source file and extract serializable type definitions.
pub fn extract_types(source_name: &str, source: &str) -> Result<Vec<TypeDef>, ParseError> {
    let file = syn::parse_file(source).map_err(|error| ParseError::InvalidRustSource {
        source_name: source_name.to_string(),
        message: error.to_string(),
    })?;
    let mut types = Vec::new();

    for item in &file.items {
        match item {
            syn::Item::Enum(e) => {
                if !has_serialize_derive(e.attrs.as_slice()) {
                    continue;
                }
                if is_simple_enum(e) {
                    types.push(TypeDef::SimpleEnum(extract_simple_enum(e)));
                } else {
                    types.push(TypeDef::TaggedEnum(extract_tagged_enum(source_name, e)?));
                }
            }
            syn::Item::Struct(s) => {
                if !has_serialize_derive(s.attrs.as_slice()) {
                    continue;
                }
                types.push(TypeDef::Struct(extract_struct(source_name, s)?));
            }
            _ => {}
        }
    }

    Ok(types)
}

/// Check if an item has `derive(Serialize)`.
fn has_serialize_derive(attrs: &[syn::Attribute]) -> bool {
    for attr in attrs {
        if attr.path().is_ident("derive") {
            let mut found = false;
            let _ = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("Serialize") {
                    found = true;
                }
                Ok(())
            });
            if found {
                return true;
            }
        }
    }
    false
}

/// A simple enum has only unit variants.
fn is_simple_enum(e: &syn::ItemEnum) -> bool {
    e.variants
        .iter()
        .all(|v| matches!(v.fields, syn::Fields::Unit))
}

fn extract_simple_enum(e: &syn::ItemEnum) -> SimpleEnum {
    SimpleEnum {
        name: e.ident.to_string(),
        variants: e.variants.iter().map(|v| v.ident.to_string()).collect(),
    }
}

fn extract_tagged_enum(source_name: &str, e: &syn::ItemEnum) -> Result<TaggedEnum, ParseError> {
    Ok(TaggedEnum {
        name: e.ident.to_string(),
        variants: e
            .variants
            .iter()
            .map(|variant| extract_enum_variant(source_name, &e.ident.to_string(), variant))
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn extract_enum_variant(
    source_name: &str,
    enum_name: &str,
    v: &syn::Variant,
) -> Result<EnumVariant, ParseError> {
    let name = v.ident.to_string();
    Ok(match &v.fields {
        syn::Fields::Unit => EnumVariant::Unit { name },
        syn::Fields::Unnamed(fields) => {
            let types: Vec<RustType> = fields.unnamed.iter().map(|f| parse_type(&f.ty)).collect();
            if types.len() == 1 {
                EnumVariant::Newtype {
                    name,
                    ty: types
                        .into_iter()
                        .next()
                        .expect("newtype variant has one field"),
                }
            } else {
                EnumVariant::Tuple {
                    name,
                    fields: types,
                }
            }
        }
        syn::Fields::Named(fields) => EnumVariant::Struct {
            name: name.clone(),
            fields: fields
                .named
                .iter()
                .map(|field| {
                    extract_field(
                        source_name,
                        &format!("enum variant `{enum_name}::{name}`"),
                        field,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?,
        },
    })
}

fn extract_struct(source_name: &str, s: &syn::ItemStruct) -> Result<StructDef, ParseError> {
    let fields = match &s.fields {
        syn::Fields::Named(named) => named
            .named
            .iter()
            .map(|field| extract_field(source_name, &format!("struct `{}`", s.ident), field))
            .collect::<Result<Vec<_>, _>>()?,
        syn::Fields::Unnamed(_) => {
            return Err(ParseError::UnsupportedStructShape {
                source_name: source_name.to_string(),
                line: s.ident.span().start().line,
                struct_name: s.ident.to_string(),
                shape: UnsupportedStructShape::Tuple,
            });
        }
        syn::Fields::Unit => {
            return Err(ParseError::UnsupportedStructShape {
                source_name: source_name.to_string(),
                line: s.ident.span().start().line,
                struct_name: s.ident.to_string(),
                shape: UnsupportedStructShape::Unit,
            });
        }
    };
    Ok(StructDef {
        name: s.ident.to_string(),
        fields,
    })
}

fn extract_field(
    source_name: &str,
    container: &str,
    f: &syn::Field,
) -> Result<FieldDef, ParseError> {
    let Some(ident) = f.ident.as_ref() else {
        return Err(ParseError::UnnamedField {
            source_name: source_name.to_string(),
            line: f.span().start().line,
            container: container.to_string(),
        });
    };
    let name = ident.to_string();

    let mut serde_skip = false;
    let mut serde_default = false;
    let mut serde_rename = None;

    for attr in &f.attrs {
        if attr.path().is_ident("serde") {
            let _ = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("skip") {
                    serde_skip = true;
                } else if meta.path.is_ident("default") {
                    serde_default = true;
                } else if meta.path.is_ident("rename") {
                    if let Ok(lit) = meta.value() {
                        if let Ok(s) = lit.parse::<syn::LitStr>() {
                            serde_rename = Some(s.value());
                        }
                    }
                } else if meta.path.is_ident("skip_serializing_if") {
                    // If skip_serializing_if is present, the field might be absent
                    // during deserialization, so treat as default
                    serde_default = true;
                    // consume the value
                    let _ = meta
                        .value()
                        .and_then(syn::parse::ParseBuffer::parse::<syn::LitStr>);
                }
                Ok(())
            });
        }
    }

    Ok(FieldDef {
        name,
        ty: parse_type(&f.ty),
        serde_skip,
        serde_default,
        serde_rename,
    })
}

/// Parse a syn Type into our `RustType` representation.
fn parse_type(ty: &syn::Type) -> RustType {
    match ty {
        syn::Type::Path(tp) => parse_type_path(tp),
        syn::Type::Tuple(tt) => {
            let elems: Vec<RustType> = tt.elems.iter().map(parse_type).collect();
            RustType::Tuple(elems)
        }
        _ => RustType::Named(quote_type(ty)),
    }
}

fn parse_type_path(tp: &syn::TypePath) -> RustType {
    // Handle paths like std::ops::Range<usize>, crate::module::ModuleGraph, etc.
    let segments: Vec<_> = tp.path.segments.iter().collect();

    // Get the last segment for type identification
    let last = segments.last().unwrap();
    let ident = last.ident.to_string();

    match ident.as_str() {
        "String" => RustType::String,
        "bool" => RustType::Bool,
        "i64" => RustType::I64,
        "u64" => RustType::U64,
        "u32" => RustType::U32,
        "f64" => RustType::F64,
        "char" => RustType::Char,
        "usize" => RustType::Usize,
        "PathBuf" => RustType::PathBuf,
        "Vec" => {
            let inner = extract_single_generic(last);
            RustType::Vec(Box::new(inner))
        }
        "Option" => {
            let inner = extract_single_generic(last);
            RustType::Option(Box::new(inner))
        }
        "Box" => {
            let inner = extract_single_generic(last);
            RustType::Box(Box::new(inner))
        }
        "Spanned" => {
            let inner = extract_single_generic(last);
            RustType::Spanned(Box::new(inner))
        }
        "HashMap" => {
            let (k, v) = extract_double_generic(last);
            RustType::HashMap(Box::new(k), Box::new(v))
        }
        "Range" => {
            let inner = extract_single_generic(last);
            RustType::Range(Box::new(inner))
        }
        _ => {
            // For qualified paths like crate::module::ModuleGraph, use the last segment
            RustType::Named(ident)
        }
    }
}

fn extract_single_generic(seg: &syn::PathSegment) -> RustType {
    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
        if let Some(syn::GenericArgument::Type(ty)) = args.args.first() {
            return parse_type(ty);
        }
    }
    RustType::Named("unknown".to_string())
}

fn extract_double_generic(seg: &syn::PathSegment) -> (RustType, RustType) {
    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
        let mut iter = args.args.iter();
        let first = iter.next().and_then(|a| {
            if let syn::GenericArgument::Type(ty) = a {
                Some(parse_type(ty))
            } else {
                None
            }
        });
        let second = iter.next().and_then(|a| {
            if let syn::GenericArgument::Type(ty) = a {
                Some(parse_type(ty))
            } else {
                None
            }
        });
        if let (Some(k), Some(v)) = (first, second) {
            return (k, v);
        }
    }
    (
        RustType::Named("unknown".to_string()),
        RustType::Named("unknown".to_string()),
    )
}

fn quote_type(ty: &syn::Type) -> String {
    use quote::ToTokens;
    ty.to_token_stream().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn extract(source: &str) -> Vec<TypeDef> {
        extract_types("<test>", source).expect("test source should parse")
    }

    // ── extract_types: filtering by derive(Serialize) ───────────────────────

    #[test]
    fn skips_structs_without_serialize_derive() {
        let source = r"
            pub struct NotSerialized { pub x: i64 }

            #[derive(Debug)]
            pub struct DebugOnly { pub y: String }
        ";
        let types = extract(source);
        assert!(
            types.is_empty(),
            "Should skip types lacking derive(Serialize)"
        );
    }

    #[test]
    fn extracts_struct_with_serialize_derive() {
        let source = r"
            #[derive(Serialize)]
            pub struct Span {
                pub start: usize,
                pub end: usize,
            }
        ";
        let types = extract(source);
        assert_eq!(types.len(), 1);
        match &types[0] {
            TypeDef::Struct(s) => {
                assert_eq!(s.name, "Span");
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name, "start");
                assert!(matches!(s.fields[0].ty, RustType::Usize));
                assert_eq!(s.fields[1].name, "end");
            }
            other => panic!("Expected Struct, got {other:?}"),
        }
    }

    #[test]
    fn extracts_serialize_among_multiple_derives() {
        let source = r"
            #[derive(Debug, Clone, Serialize, PartialEq)]
            pub struct Token {
                pub value: String,
            }
        ";
        let types = extract(source);
        assert_eq!(types.len(), 1);
        assert_eq!(types[0].name(), "Token");
    }

    // ── Simple enum vs tagged enum classification ───────────────────────────

    #[test]
    fn classifies_all_unit_variants_as_simple_enum() {
        let source = r"
            #[derive(Serialize)]
            pub enum Visibility {
                Public,
                Private,
                Crate,
            }
        ";
        let types = extract(source);
        assert_eq!(types.len(), 1);
        match &types[0] {
            TypeDef::SimpleEnum(e) => {
                assert_eq!(e.name, "Visibility");
                assert_eq!(e.variants, vec!["Public", "Private", "Crate"]);
            }
            other => panic!("Expected SimpleEnum, got {other:?}"),
        }
    }

    #[test]
    fn classifies_enum_with_data_variants_as_tagged() {
        let source = r"
            #[derive(Serialize)]
            pub enum Expr {
                Literal(LitValue),
                Binary { left: Box<Expr>, op: BinOp, right: Box<Expr> },
                Unit,
            }
        ";
        let types = extract(source);
        assert_eq!(types.len(), 1);
        match &types[0] {
            TypeDef::TaggedEnum(e) => {
                assert_eq!(e.name, "Expr");
                assert_eq!(e.variants.len(), 3);

                // Newtype variant
                assert!(
                    matches!(&e.variants[0], EnumVariant::Newtype { name, .. } if name == "Literal")
                );
                // Struct variant
                assert!(
                    matches!(&e.variants[1], EnumVariant::Struct { name, fields, .. }
                    if name == "Binary" && fields.len() == 3)
                );
                // Unit variant inside tagged enum
                assert!(matches!(&e.variants[2], EnumVariant::Unit { name } if name == "Unit"));
            }
            other => panic!("Expected TaggedEnum, got {other:?}"),
        }
    }

    // ── Tuple variant parsing ───────────────────────────────────────────────

    #[test]
    fn parses_tuple_variant_with_multiple_fields() {
        let source = r"
            #[derive(Serialize)]
            pub enum Pattern {
                Or(Box<Pattern>, Box<Pattern>),
            }
        ";
        let types = extract(source);
        match &types[0] {
            TypeDef::TaggedEnum(e) => match &e.variants[0] {
                EnumVariant::Tuple { name, fields } => {
                    assert_eq!(name, "Or");
                    assert_eq!(fields.len(), 2);
                    assert!(
                        matches!(&fields[0], RustType::Box(inner) if matches!(inner.as_ref(), RustType::Named(n) if n == "Pattern"))
                    );
                }
                other => panic!("Expected Tuple variant, got {other:?}"),
            },
            other => panic!("Expected TaggedEnum, got {other:?}"),
        }
    }

    // ── Serde attribute extraction ──────────────────────────────────────────

    #[test]
    fn extracts_serde_skip_attribute() {
        let source = r"
            #[derive(Serialize)]
            pub struct Node {
                pub name: String,
                #[serde(skip)]
                pub cached: bool,
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        assert!(!s.fields[0].serde_skip, "name should not be skipped");
        assert!(s.fields[1].serde_skip, "cached should be skipped");
    }

    #[test]
    fn extracts_serde_default_attribute() {
        let source = r"
            #[derive(Serialize)]
            pub struct Config {
                pub name: String,
                #[serde(default)]
                pub flags: Vec<String>,
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        assert!(!s.fields[0].serde_default);
        assert!(s.fields[1].serde_default);
    }

    #[test]
    fn extracts_serde_rename_attribute() {
        let source = r#"
            #[derive(Serialize)]
            pub struct Field {
                #[serde(rename = "type")]
                pub ty: String,
            }
        "#;
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        assert_eq!(s.fields[0].serde_rename.as_deref(), Some("type"));
    }

    #[test]
    fn skip_serializing_if_implies_default() {
        let source = r#"
            #[derive(Serialize)]
            pub struct Item {
                #[serde(skip_serializing_if = "Option::is_none")]
                pub doc: Option<String>,
            }
        "#;
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        assert!(
            s.fields[0].serde_default,
            "skip_serializing_if should imply serde_default"
        );
    }

    // ── Type parsing ────────────────────────────────────────────────────────

    #[test]
    fn parses_primitive_types() {
        let source = r"
            #[derive(Serialize)]
            pub struct Primitives {
                pub a: String,
                pub b: bool,
                pub c: i64,
                pub d: u64,
                pub e: u32,
                pub f: f64,
                pub g: char,
                pub h: usize,
                pub i: PathBuf,
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        assert!(matches!(s.fields[0].ty, RustType::String));
        assert!(matches!(s.fields[1].ty, RustType::Bool));
        assert!(matches!(s.fields[2].ty, RustType::I64));
        assert!(matches!(s.fields[3].ty, RustType::U64));
        assert!(matches!(s.fields[4].ty, RustType::U32));
        assert!(matches!(s.fields[5].ty, RustType::F64));
        assert!(matches!(s.fields[6].ty, RustType::Char));
        assert!(matches!(s.fields[7].ty, RustType::Usize));
        assert!(matches!(s.fields[8].ty, RustType::PathBuf));
    }

    #[test]
    fn parses_generic_wrapper_types() {
        let source = r"
            #[derive(Serialize)]
            pub struct Wrappers {
                pub items: Vec<String>,
                pub maybe: Option<i64>,
                pub boxed: Box<Expr>,
                pub spanned: Spanned<TypeExpr>,
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };

        assert!(
            matches!(&s.fields[0].ty, RustType::Vec(inner) if matches!(inner.as_ref(), RustType::String))
        );
        assert!(
            matches!(&s.fields[1].ty, RustType::Option(inner) if matches!(inner.as_ref(), RustType::I64))
        );
        assert!(
            matches!(&s.fields[2].ty, RustType::Box(inner) if matches!(inner.as_ref(), RustType::Named(n) if n == "Expr"))
        );
        assert!(
            matches!(&s.fields[3].ty, RustType::Spanned(inner) if matches!(inner.as_ref(), RustType::Named(n) if n == "TypeExpr"))
        );
    }

    #[test]
    fn parses_hashmap_type() {
        let source = r"
            #[derive(Serialize)]
            pub struct Registry {
                pub entries: HashMap<String, ModuleId>,
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        match &s.fields[0].ty {
            RustType::HashMap(k, v) => {
                assert!(matches!(k.as_ref(), RustType::String));
                assert!(matches!(v.as_ref(), RustType::Named(n) if n == "ModuleId"));
            }
            other => panic!("Expected HashMap, got {other:?}"),
        }
    }

    #[test]
    fn parses_range_type() {
        let source = r"
            #[derive(Serialize)]
            pub struct Located {
                pub span: Range<usize>,
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        assert!(
            matches!(&s.fields[0].ty, RustType::Range(inner) if matches!(inner.as_ref(), RustType::Usize))
        );
    }

    #[test]
    fn parses_tuple_type() {
        let source = r"
            #[derive(Serialize)]
            pub struct Pair {
                pub coords: (u64, String),
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        match &s.fields[0].ty {
            RustType::Tuple(elems) => {
                assert_eq!(elems.len(), 2);
                assert!(matches!(&elems[0], RustType::U64));
                assert!(matches!(&elems[1], RustType::String));
            }
            other => panic!("Expected Tuple, got {other:?}"),
        }
    }

    #[test]
    fn parses_nested_generics() {
        let source = r"
            #[derive(Serialize)]
            pub struct Nested {
                pub items: Vec<Option<Box<Expr>>>,
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        // Vec<Option<Box<Expr>>>
        match &s.fields[0].ty {
            RustType::Vec(inner) => match inner.as_ref() {
                RustType::Option(inner2) => match inner2.as_ref() {
                    RustType::Box(inner3) => {
                        assert!(matches!(inner3.as_ref(), RustType::Named(n) if n == "Expr"));
                    }
                    other => panic!("Expected Box, got {other:?}"),
                },
                other => panic!("Expected Option, got {other:?}"),
            },
            other => panic!("Expected Vec, got {other:?}"),
        }
    }

    #[test]
    fn resolves_qualified_path_to_last_segment() {
        let source = r"
            #[derive(Serialize)]
            pub struct Module {
                pub graph: crate::module::ModuleGraph,
            }
        ";
        let types = extract(source);
        let TypeDef::Struct(s) = &types[0] else {
            panic!()
        };
        assert!(
            matches!(&s.fields[0].ty, RustType::Named(n) if n == "ModuleGraph"),
            "Qualified path should resolve to last segment"
        );
    }

    #[test]
    fn rejects_tuple_structs_with_a_diagnostic() {
        let source = r"
            #[derive(Serialize)]
            pub struct Pair(String, String);
        ";
        let error = extract_types("ast.rs", source).expect_err("tuple structs should be rejected");
        assert_eq!(
            error.to_string(),
            "ast.rs:3: unsupported tuple struct `Pair`; hew-astgen only supports named-field structs"
        );
    }

    #[test]
    fn rejects_unit_structs_with_a_diagnostic() {
        let source = r"
            #[derive(Serialize)]
            pub struct Marker;
        ";
        let error =
            extract_types("module.rs", source).expect_err("unit structs should be rejected");
        assert_eq!(
            error.to_string(),
            "module.rs:3: unsupported unit struct `Marker`; hew-astgen only supports named-field structs"
        );
    }

    // ── Multiple types in one file ──────────────────────────────────────────

    #[test]
    fn extracts_multiple_types_from_single_file() {
        let source = r"
            #[derive(Serialize)]
            pub enum Colour { Red, Green, Blue }

            #[derive(Serialize)]
            pub struct Point { pub x: f64, pub y: f64 }

            #[derive(Serialize)]
            pub enum Shape {
                Circle(f64),
                Rect { width: f64, height: f64 },
            }
        ";
        let types = extract(source);
        assert_eq!(types.len(), 3);
        assert!(matches!(&types[0], TypeDef::SimpleEnum(e) if e.name == "Colour"));
        assert!(matches!(&types[1], TypeDef::Struct(s) if s.name == "Point"));
        assert!(matches!(&types[2], TypeDef::TaggedEnum(e) if e.name == "Shape"));
    }
}
