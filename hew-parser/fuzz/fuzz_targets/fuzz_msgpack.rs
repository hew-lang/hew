#![no_main]

use arbitrary::{Arbitrary, Unstructured};
use hew_parser::ast::{
    Item, VariantDecl, VariantKind, Visibility, WireDecl, WireDeclKind, WireFieldDecl,
};
use hew_serialize::serialize_wire_decl_via_plan;
use hew_wirecodec::MsgpackCodecDesc;
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct WireSchema {
    visibility: bool,
    kind: SchemaKind,
    name: u8,
    fields: Vec<WireField>,
    variants: Vec<u8>,
}

#[derive(Debug, Arbitrary, Clone, Copy)]
enum SchemaKind {
    Struct,
    Enum,
}

#[derive(Debug, Arbitrary, Clone)]
struct WireField {
    name: u8,
    ty: WireType,
    explicit_number: Option<u8>,
    optional: bool,
    repeated: bool,
    deprecated: bool,
}

#[derive(Debug, Arbitrary, Clone)]
enum WireType {
    Bool,
    I64,
    U64,
    F64,
    String,
    Bytes,
    Duration,
    Unit,
    Nested,
}

impl WireSchema {
    fn to_source(&self) -> String {
        let vis = if self.visibility { "pub " } else { "" };
        let name = ident("Wire", self.name);
        match self.kind {
            SchemaKind::Struct => self.struct_source(vis, &name),
            SchemaKind::Enum => self.enum_source(vis, &name),
        }
    }

    fn to_decl(&self) -> WireDecl {
        let visibility = if self.visibility {
            Visibility::Pub
        } else {
            Visibility::Private
        };
        let name = ident("Wire", self.name);
        match self.kind {
            SchemaKind::Struct => {
                let fields = if self.fields.is_empty() {
                    vec![WireField::default_field()]
                } else {
                    self.fields.iter().take(8).cloned().collect()
                };
                WireDecl {
                    visibility,
                    kind: WireDeclKind::Struct,
                    name,
                    fields: fields
                        .iter()
                        .enumerate()
                        .map(|(idx, field)| WireFieldDecl {
                            name: ident("field", field.name),
                            ty: field.ty.as_source().to_string(),
                            field_number: field.explicit_number.map_or_else(
                                || u32::try_from(idx).unwrap_or(0) + 1,
                                |n| u32::from(n % 63) + 1,
                            ),
                            is_optional: field.optional,
                            is_repeated: field.repeated,
                            is_reserved: false,
                            is_deprecated: field.deprecated,
                            json_name: None,
                            yaml_name: None,
                            since: None,
                        })
                        .collect(),
                    variants: Vec::new(),
                    json_case: None,
                    yaml_case: None,
                }
            }
            SchemaKind::Enum => WireDecl {
                visibility,
                kind: WireDeclKind::Enum,
                name,
                fields: Vec::new(),
                variants: self
                    .variants
                    .iter()
                    .copied()
                    .take(8)
                    .map(|v| VariantDecl {
                        name: ident("Variant", v),
                        kind: VariantKind::Unit,
                        doc_comment: None,
                        span: 0..0,
                    })
                    .collect(),
                json_case: None,
                yaml_case: None,
            },
        }
    }

    fn struct_source(&self, vis: &str, name: &str) -> String {
        let mut out = format!("{vis}wire type {name} {{\n");
        let fields = if self.fields.is_empty() {
            vec![WireField::default_field()]
        } else {
            self.fields.iter().take(8).cloned().collect()
        };
        for (idx, field) in fields.iter().enumerate() {
            let field_name = ident("field", field.name);
            out.push_str("    ");
            if field.optional {
                out.push_str("optional ");
            }
            if field.repeated {
                out.push_str("repeated ");
            }
            if field.deprecated {
                out.push_str("deprecated ");
            }
            out.push_str(&field_name);
            out.push_str(": ");
            out.push_str(field.ty.as_source());
            if let Some(number) = field.explicit_number {
                out.push_str(" = ");
                out.push_str(&(u32::from(number % 63) + 1).to_string());
            } else {
                out.push_str(" = ");
                out.push_str(&(u32::try_from(idx).unwrap_or(0) + 1).to_string());
            }
            out.push_str(";\n");
        }
        out.push_str("}\n");
        out
    }

    fn enum_source(&self, vis: &str, name: &str) -> String {
        let mut out = format!("{vis}wire enum {name} {{\n");
        let variants: Vec<u8> = if self.variants.is_empty() {
            vec![0, 1]
        } else {
            self.variants.iter().copied().take(8).collect()
        };
        for variant in variants {
            out.push_str("    ");
            out.push_str(&ident("Variant", variant));
            out.push_str(";\n");
        }
        out.push_str("}\n");
        out
    }
}

impl WireField {
    fn default_field() -> Self {
        Self {
            name: 0,
            ty: WireType::I64,
            explicit_number: Some(1),
            optional: false,
            repeated: false,
            deprecated: false,
        }
    }
}

impl WireType {
    fn as_source(&self) -> &'static str {
        match self {
            Self::Bool => "bool",
            Self::I64 => "i64",
            Self::U64 => "u64",
            Self::F64 => "f64",
            Self::String => "string",
            Self::Bytes => "bytes",
            Self::Duration => "duration",
            Self::Unit => "()",
            Self::Nested => "NestedWire",
        }
    }
}

fn ident(prefix: &str, byte: u8) -> String {
    format!("{prefix}{}", byte % 16)
}

fn exercise_wire_decl(decl: &WireDecl) {
    if let Ok(bytes) = serialize_wire_decl_via_plan(decl) {
        let decoded: MsgpackCodecDesc =
            rmp_serde::from_slice(&bytes).expect("descriptor bytes round-trip");
        let encoded = decoded.to_msgpack_bytes();
        let _: MsgpackCodecDesc =
            rmp_serde::from_slice(&encoded).expect("re-encoded descriptor round-trip");
    }
}

fn exercise_source(source: &str) {
    let parsed = hew_parser::parse(source);
    if parsed
        .errors
        .iter()
        .any(|e| e.severity == hew_parser::Severity::Error)
    {
        return;
    }
    for (item, _) in &parsed.program.items {
        match item {
            Item::Wire(decl) => exercise_wire_decl(decl),
            Item::TypeDecl(decl) if decl.wire.is_some() => {
                // `wire type` structs desugar to TypeDecl today; the
                // descriptor path still accepts enum WireDecls directly.
            }
            _ => {}
        }
    }
}

fuzz_target!(|data: &[u8]| {
    let _ = rmp_serde::from_slice::<MsgpackCodecDesc>(data);

    if let Ok(source) = std::str::from_utf8(data) {
        exercise_source(source);
    }

    let mut unstructured = Unstructured::new(data);
    if let Ok(schema) = WireSchema::arbitrary(&mut unstructured) {
        let source = schema.to_source();
        exercise_source(&source);
        exercise_wire_decl(&schema.to_decl());
    }
});
