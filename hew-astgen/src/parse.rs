use crate::model::*;

/// Parse a Rust source file and extract serializable type definitions.
pub fn extract_types(source: &str) -> Vec<TypeDef> {
    let file = syn::parse_file(source).expect("Failed to parse Rust source");
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
                    types.push(TypeDef::TaggedEnum(extract_tagged_enum(e)));
                }
            }
            syn::Item::Struct(s) => {
                if !has_serialize_derive(s.attrs.as_slice()) {
                    continue;
                }
                types.push(TypeDef::Struct(extract_struct(s)));
            }
            _ => {}
        }
    }

    types
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

fn extract_tagged_enum(e: &syn::ItemEnum) -> TaggedEnum {
    TaggedEnum {
        name: e.ident.to_string(),
        variants: e.variants.iter().map(extract_enum_variant).collect(),
    }
}

fn extract_enum_variant(v: &syn::Variant) -> EnumVariant {
    let name = v.ident.to_string();
    match &v.fields {
        syn::Fields::Unit => EnumVariant::Unit { name },
        syn::Fields::Unnamed(fields) => {
            let types: Vec<RustType> = fields.unnamed.iter().map(|f| parse_type(&f.ty)).collect();
            if types.len() == 1 {
                EnumVariant::Newtype {
                    name,
                    ty: types.into_iter().next().unwrap(),
                }
            } else {
                EnumVariant::Tuple {
                    name,
                    fields: types,
                }
            }
        }
        syn::Fields::Named(fields) => EnumVariant::Struct {
            name,
            fields: fields.named.iter().map(extract_field).collect(),
        },
    }
}

fn extract_struct(s: &syn::ItemStruct) -> StructDef {
    let fields = match &s.fields {
        syn::Fields::Named(named) => named.named.iter().map(extract_field).collect(),
        _ => Vec::new(),
    };
    StructDef {
        name: s.ident.to_string(),
        fields,
    }
}

fn extract_field(f: &syn::Field) -> FieldDef {
    let name = f.ident.as_ref().map(|i| i.to_string()).unwrap_or_default();

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
                    let _ = meta.value().and_then(|v| v.parse::<syn::LitStr>());
                }
                Ok(())
            });
        }
    }

    FieldDef {
        name,
        ty: parse_type(&f.ty),
        serde_skip,
        serde_default,
        serde_rename,
    }
}

/// Parse a syn Type into our RustType representation.
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
