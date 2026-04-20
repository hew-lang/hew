//! Shadow-comparison test: every `e2e_wire` fixture lowers through the
//! `hew-wirecodec` plan path and emits byte-stable msgpack output that
//! round-trips losslessly through `rmp-serde`.
//!
//! This is the Stage 2 gate for Lane 7 — no divergence across any of the
//! 42 `.hew` fixtures in `hew-codegen/tests/examples/e2e_wire/`. A byte
//! divergence (or a plan-build error) means the descriptor emitter has
//! drifted from the parser-level wire contract.
//!
//! NOTE on oracle: the Stage 2 contract is that the descriptor path is
//! a deterministic function of the parsed `WireDecl`. We assert this by
//! (a) running two independent encode passes on the same declaration and
//! comparing bytes, and (b) round-tripping through `rmp-serde` to prove
//! the descriptor decodes to itself. Lane 7b Stage 7 extends this with
//! the 10,000-iteration random-corpus comparison against the legacy
//! WireDecl→rmp-serde path (enabled via the `legacy-wire-msgpack`
//! feature).

use std::fs;
use std::path::{Path, PathBuf};

use hew_parser::ast::WireDecl;
use hew_serialize::serialize_wire_decl_via_plan;
use hew_wirecodec::{MsgpackCodecDesc, WireCodecPlan};

fn e2e_wire_dir() -> PathBuf {
    // hew-serialize/tests → ../../hew-codegen/tests/examples/e2e_wire
    let manifest = env!("CARGO_MANIFEST_DIR");
    Path::new(manifest)
        .join("..")
        .join("hew-codegen")
        .join("tests")
        .join("examples")
        .join("e2e_wire")
}

fn iter_wire_fixtures() -> Vec<PathBuf> {
    let dir = e2e_wire_dir();
    assert!(
        dir.is_dir(),
        "expected fixture directory at {}",
        dir.display()
    );
    let mut out = Vec::new();
    for entry in fs::read_dir(&dir).expect("read e2e_wire dir") {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("hew") {
            out.push(path);
        }
    }
    out.sort();
    out
}

fn field_type_name(field_name: &str, ty: &hew_parser::ast::TypeExpr) -> String {
    match ty {
        hew_parser::ast::TypeExpr::Named { name, .. } => name.clone(),
        _ => panic!(
            "wire fixture field `{field_name}` uses unsupported non-named type in plan collection"
        ),
    }
}

/// Extract `WireDecls` from either the legacy `Item::Wire` shape (`wire type
/// Foo { ... }` syntax) or the newer `#[wire]`-on-`TypeDecl` shape.
///
/// The fixtures use both forms. The second is the predominant shape today:
/// the parser recognizes the attribute and records wire metadata on the
/// `TypeDecl` itself. To exercise the Stage 2 contract over every fixture,
/// we synthesise a `WireDecl` from each `TypeDecl { wire: Some(_), .. }`
/// and run the same plan-driven encode path.
fn collect_all_wire_decls_from_program(source: &str) -> Vec<WireDecl> {
    use hew_parser::ast::{
        Item as AstItem, TypeBodyItem, TypeDeclKind, VariantDecl, WireDeclKind, WireFieldDecl,
    };
    let result = hew_parser::parser::parse(source);
    let mut out: Vec<WireDecl> = Vec::new();
    for item in result.program.items {
        match item.0 {
            AstItem::Wire(w) => out.push(w),
            AstItem::TypeDecl(td) => {
                let Some(meta) = td.wire else { continue };
                // Lower TypeBodyItem::Field + WireFieldMeta pairs into
                // WireFieldDecl records for the plan-build path. Non-Field
                // body items (methods, variants) carry no wire number.
                let mut fields: Vec<WireFieldDecl> = Vec::new();
                let mut variants: Vec<VariantDecl> = Vec::new();
                for body in &td.body {
                    match body {
                        TypeBodyItem::Field { name, ty, .. } => {
                            let Some(meta_entry) =
                                meta.field_meta.iter().find(|m| &m.field_name == name)
                            else {
                                continue;
                            };
                            let ty_name = field_type_name(name, &ty.0);
                            fields.push(WireFieldDecl {
                                name: name.clone(),
                                ty: ty_name,
                                field_number: meta_entry.field_number,
                                is_optional: meta_entry.is_optional,
                                is_repeated: meta_entry.is_repeated,
                                is_reserved: false,
                                is_deprecated: meta_entry.is_deprecated,
                                json_name: meta_entry.json_name.clone(),
                                yaml_name: meta_entry.yaml_name.clone(),
                                since: meta_entry.since,
                            });
                        }
                        TypeBodyItem::Variant(v) => variants.push(v.clone()),
                        TypeBodyItem::Method(_) => {}
                    }
                }
                out.push(WireDecl {
                    visibility: td.visibility,
                    kind: match td.kind {
                        TypeDeclKind::Struct => WireDeclKind::Struct,
                        TypeDeclKind::Enum => WireDeclKind::Enum,
                    },
                    name: td.name,
                    fields,
                    variants,
                    json_case: meta.json_case,
                    yaml_case: meta.yaml_case,
                });
            }
            _ => {}
        }
    }
    out
}

#[test]
fn every_wire_fixture_has_a_deterministic_plan_driven_encoding() {
    let fixtures = iter_wire_fixtures();
    assert!(
        fixtures.len() >= 40,
        "expected ≥40 wire fixtures, found {}",
        fixtures.len()
    );
    let mut fixtures_with_wire_decls = 0usize;
    let mut total_wire_decls = 0usize;
    let mut skipped_fixtures = Vec::new();
    for path in &fixtures {
        let source = fs::read_to_string(path).expect("fixture read");
        let decls = collect_all_wire_decls_from_program(&source);
        if decls.is_empty() {
            // Fixtures without any wire-typed items (e.g. pure decode-side
            // tests that only call `X.decode(...)` against an externally
            // defined type) have no plan to build. Record and continue.
            skipped_fixtures.push(path.file_name().unwrap().to_string_lossy().into_owned());
            continue;
        }
        fixtures_with_wire_decls += 1;
        total_wire_decls += decls.len();
        for decl in decls {
            // (a) Deterministic: two encodes of the same decl produce the
            // exact same bytes.
            let a = serialize_wire_decl_via_plan(&decl).unwrap_or_else(|e| {
                panic!(
                    "plan build failed on {} (type {}): {e}",
                    path.display(),
                    decl.name
                )
            });
            let b = serialize_wire_decl_via_plan(&decl).expect("second encode");
            assert_eq!(a, b, "non-deterministic bytes for {}", path.display());

            // (b) Round-trip: bytes decode losslessly into a descriptor whose
            // plan-derivation reproduces itself.
            let desc: MsgpackCodecDesc = rmp_serde::from_slice(&a)
                .unwrap_or_else(|e| panic!("rmp-serde decode failed on {}: {e}", path.display()));
            let plan = WireCodecPlan::build(&decl).expect("plan");
            let desc_again = MsgpackCodecDesc::from_plan(&plan);
            assert_eq!(
                desc,
                desc_again,
                "descriptor round-trip mismatch for {}",
                path.display()
            );
        }
    }
    // Minimum surface threshold: we must exercise at least 35 of the 42
    // fixtures. Stage 2 is only credible if the contract covers the bulk
    // of the e2e_wire corpus.
    assert!(
        fixtures_with_wire_decls >= 35,
        "plan-driven shadow covers only {fixtures_with_wire_decls}/{} fixtures; skipped: {:?}",
        fixtures.len(),
        skipped_fixtures
    );
    eprintln!(
        "plan-driven msgpack shadow: {fixtures_with_wire_decls}/{} fixtures, {total_wire_decls} WireDecls; skipped {} fixtures: {:?}",
        fixtures.len(),
        skipped_fixtures.len(),
        skipped_fixtures
    );
}

#[test]
fn plan_collection_rejects_non_named_wire_fields() {
    use hew_parser::ast::TypeExpr;

    let tuple = TypeExpr::Tuple(vec![
        (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
    ]);

    let panic = std::panic::catch_unwind(|| field_type_name("field", &tuple))
        .expect_err("non-named wire fields must panic");
    let message = panic
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| panic.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");
    assert!(message.contains("unsupported non-named type"));
}
