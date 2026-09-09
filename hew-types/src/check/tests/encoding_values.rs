#[allow(
    clippy::wildcard_imports,
    reason = "checker tests share private helpers"
)]
use super::*;
use crate::{CloneKind, SendFact, ValueCapability, ValueClass, ValueMethodPlan};

const VALUE_SOURCE: &str = "#[opaque] pub type Value {}";

fn encoding_ty(kind: BuiltinType) -> Ty {
    Ty::Named {
        name: kind.canonical_name().to_string(),
        args: vec![],
        builtin: Some(kind),
    }
}

fn parsed_items(source: &str) -> Vec<Spanned<Item>> {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    parsed.program.items
}

#[test]
fn encoding_values_require_shipped_source_and_have_semantic_copy_facts() {
    for (format, kind) in [
        ("json", BuiltinType::JsonValue),
        ("yaml", BuiltinType::YamlValue),
    ] {
        let source = format!("{VALUE_SOURCE}\npub fn identity(value: Value) -> Value {{ value }}");
        let module: Vec<String> = ["std", "encoding", format].map(str::to_string).into();
        let output = check_source_in_canonical_std_module(&source, &module);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let ty = &output.fn_sigs[&format!("std.encoding.{format}.identity")].return_type;
        assert_eq!(ty, &encoding_ty(kind));
        let declaration = &output.type_fact_context.declarations()[kind.canonical_name()];
        assert_eq!(declaration.builtin, Some(kind));
        assert!(declaration.is_opaque);
        assert_ne!(
            ty_is_eq_eligible(ty, &output.type_defs),
            EqEligibility::Eligible
        );
        assert_ne!(
            crate::hash_eligibility::ty_is_hash_eligible_with_resources(
                ty,
                &output.type_defs,
                &HashSet::new()
            ),
            crate::hash_eligibility::HashEligibility::Eligible
        );
        let resolved = ResolvedTy::from_ty(ty).unwrap();
        let facts = output.type_facts[&resolved.clone().into()];
        assert_eq!(
            (facts.class, facts.clone),
            (ValueClass::CowValue, CloneKind::DeepCopy)
        );
        assert_eq!(facts.send, SendFact::Known(true));
        assert!(!facts.eq && !facts.hash);
        let mut service = TypeFactService::new(output.type_fact_context, output.type_facts);
        for capability in [ValueCapability::Eq, ValueCapability::Hash] {
            assert!(service
                .capability_plan(&resolved, capability)
                .unwrap()
                .is_none());
        }

        let lookalike = check_source_in_module(&source, module.clone());
        assert!(lookalike.errors.is_empty(), "{:?}", lookalike.errors);
        let ty = &lookalike.fn_sigs[&format!("std.encoding.{format}.identity")].return_type;
        assert!(matches!(ty, Ty::Named { builtin: None, .. }));
        assert_eq!(
            lookalike.type_fact_context.declarations()[kind.canonical_name()].builtin,
            None
        );

        let comparison = check_source_in_canonical_std_module(
            &format!("{source}\nfn compare(left: Value, right: Value) -> bool {{ left == right }}"),
            &module,
        );
        assert!(
            comparison.errors.iter().any(|error| {
                error.kind == TypeErrorKind::InvalidOperation
                    && error.message.contains("no selected Eq implementation")
            }),
            "{:?}",
            comparison.errors
        );
    }
}

#[test]
fn encoding_values_select_only_explicit_eq_methods() {
    for (format, kind) in [
        ("json", BuiltinType::JsonValue),
        ("yaml", BuiltinType::YamlValue),
    ] {
        let source = format!(
            "{VALUE_SOURCE}\nimpl Eq for Value {{ fn eq(self, other: Value) -> bool {{ true }} }}\n\
             pub fn compare(left: Vec<Value>, right: Vec<Value>) -> bool {{ left == right }}"
        );
        let module: Vec<String> = ["std", "encoding", format].map(str::to_string).into();
        let output = check_source_in_canonical_std_module(&source, &module);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let expected = output
            .identity
            .declaration_by_path(&format!(
                "std.encoding.{format}.Value::<impl Eq for std.encoding.{format}.Value>::eq"
            ))
            .unwrap()
            .clone();
        let mut service = TypeFactService::new(output.type_fact_context, output.type_facts);
        for opaque in [false, true] {
            let mut resolved = ResolvedTy::from_ty(&encoding_ty(kind)).unwrap();
            let ResolvedTy::Named { is_opaque, .. } = &mut resolved else {
                unreachable!()
            };
            *is_opaque = opaque;
            let selected = service
                .capability_plan(&resolved, ValueCapability::Eq)
                .unwrap()
                .unwrap();
            assert_eq!(
                selected.plan(),
                &ValueMethodPlan::User {
                    method: expected.clone(),
                    type_args: vec![]
                }
            );
            assert!(service
                .capability_plan(&resolved, ValueCapability::Hash)
                .unwrap()
                .is_none());
        }
    }
}

#[test]
fn generic_equality_preserves_opaque_encoding_type_arguments() {
    for (format, kind) in [
        ("json", BuiltinType::JsonValue),
        ("yaml", BuiltinType::YamlValue),
    ] {
        let source = format!(
            r"
            {VALUE_SOURCE}
            type Holder<T> {{ value: T }}
            impl<T> Eq for Holder<T> {{ fn eq(self, other: Holder<T>) -> bool {{ true }} }}
        "
        );
        let module: Vec<String> = ["std", "encoding", format].map(str::to_string).into();
        let output = check_source_in_canonical_std_module(&source, &module);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let mut service = TypeFactService::new(output.type_fact_context, output.type_facts);
        let value = ResolvedTy::Named {
            name: kind.canonical_name().to_string(),
            args: vec![],
            builtin: Some(kind),
            is_opaque: true,
        };
        for argument in [
            value.clone(),
            ResolvedTy::named_builtin("Option", BuiltinType::Option, vec![value]),
        ] {
            let receiver = ResolvedTy::named_user(
                format!("std.encoding.{format}.Holder"),
                vec![argument.clone()],
            );
            let selection = service
                .capability_plan(&receiver, ValueCapability::Eq)
                .unwrap()
                .unwrap();
            let ValueMethodPlan::User { type_args, .. } = selection.plan() else {
                panic!("expected explicit generic equality")
            };
            assert_eq!(type_args, &[argument]);
        }
    }
}

#[test]
fn encoding_value_markers_come_from_owned_tree_contract() {
    let registry = TraitRegistry::new();
    for kind in [BuiltinType::JsonValue, BuiltinType::YamlValue] {
        let ty = encoding_ty(kind);
        for marker in [
            MarkerTrait::Clone,
            MarkerTrait::Drop,
            MarkerTrait::Send,
            MarkerTrait::Sync,
        ] {
            assert!(
                registry.implements_marker(&ty, marker),
                "{kind:?}: {marker:?}"
            );
        }
        for marker in [
            MarkerTrait::Copy,
            MarkerTrait::Resource,
            MarkerTrait::Eq,
            MarkerTrait::Hash,
        ] {
            assert!(
                !registry.implements_marker(&ty, marker),
                "{kind:?}: {marker:?}"
            );
        }
    }
}

#[test]
fn encoding_value_names_do_not_grant_catalogue_authority() {
    for name in [
        "Value",
        "JsonValue",
        "YamlValue",
        "json.Value",
        "yaml.Value",
        "alias.Value",
        "std.encoding.json.Value",
        "std.encoding.yaml.Value",
    ] {
        assert_eq!(crate::lookup_builtin_type(name), None, "{name}");
        assert_eq!(
            Checker::default().resolved_builtin_type(name),
            None,
            "{name}"
        );
    }
    let output = check_source(
        "#[resource] #[opaque] type Value {}\nfn identity(value: Value) -> Value { value }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let ty = &output.fn_sigs["identity"].return_type;
    assert!(matches!(ty, Ty::Named { builtin: None, .. }));
    let facts = output.type_facts[&ResolvedTy::from_ty(ty).unwrap().into()];
    assert_eq!(
        (facts.class, facts.clone),
        (ValueClass::AffineResource, CloneKind::None)
    );
}

#[test]
fn encoding_value_import_aliases_preserve_identity_inside_generics() {
    for (format, kind) in [
        ("json", BuiltinType::JsonValue),
        ("yaml", BuiltinType::YamlValue),
    ] {
        for import_source in [
            format!("import std.encoding.{format} as encoded;"),
            format!("import std.encoding.{format}.{{Value as Selected}};"),
        ] {
            let mut items = parsed_items(&import_source);
            let Item::Import(import) = &mut items[0].0 else {
                panic!("import fixture")
            };
            import.resolved_items = Some(parsed_items(VALUE_SOURCE).into());
            let source_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .join(format!("std/encoding/{format}/{format}.hew"));
            import.resolved_source_paths = vec![source_path];
            let spelling = if import.module_alias.is_some() {
                "encoded.Value"
            } else {
                "Selected"
            };
            items.extend(parsed_items(&format!(
                "type Value {{ number: i64 }}\n\
                 type Envelope<T> {{ payload: T }}\n\
                 fn identity(value: Envelope<Option<{spelling}>>) -> Envelope<Option<{spelling}>> {{ value }}"
            )));
            let output = check_items(items);
            assert!(
                output.errors.is_empty(),
                "{import_source}: {:?}",
                output.errors
            );
            let expected = Ty::named("Envelope", vec![Ty::option(encoding_ty(kind))]);
            assert_eq!(output.fn_sigs["identity"].return_type, expected);
            let resolved = ResolvedTy::from_ty(&expected).unwrap();
            let facts = output.type_facts[&resolved.into()];
            assert_eq!(
                (facts.class, facts.clone),
                (ValueClass::CowValue, CloneKind::FieldWise)
            );
        }
    }
}

#[test]
fn encoding_value_reexported_signature_preserves_original_owner() {
    let relay_path = std::path::PathBuf::from("encoding-value-relay.hew");
    let mut relay = parsed_items("import std.encoding.json as data; pub fn round_trip(value: Vec<data.Value>) -> Vec<data.Value> { value }");
    let Item::Import(import) = &mut relay[0].0 else {
        panic!("import fixture")
    };
    import.resolved_items = Some(parsed_items(VALUE_SOURCE).into());
    import.resolved_source_paths = vec![std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/encoding/json/json.hew")];
    let mut items = parsed_items("import relay.{round_trip as forward};");
    let Item::Import(import) = &mut items[0].0 else {
        panic!("import fixture")
    };
    import.resolved_items = Some(relay.clone().into());
    import.resolved_item_source_paths = vec![relay_path.clone(); relay.len()];
    import.resolved_source_paths = vec![relay_path.clone()];
    let root = ModuleId::root();
    let json = ModuleId::new(["std", "encoding", "json"].map(str::to_string).into());
    let relay_id = ModuleId::new(vec!["relay".to_string()]);
    let mut graph = ModuleGraph::new(root.clone());
    graph
        .add_module(Module {
            id: json.clone(),
            items: parsed_items(VALUE_SOURCE),
            imports: vec![],
            source_paths: vec![std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .join("std/encoding/json/json.hew")],
            doc: None,
        })
        .unwrap();
    graph
        .add_module(Module {
            id: relay_id.clone(),
            items: relay,
            imports: vec![],
            source_paths: vec![relay_path],
            doc: None,
        })
        .unwrap();
    graph
        .add_module(Module {
            id: root.clone(),
            items: items.clone(),
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .unwrap();
    graph.topo_order = vec![json, relay_id, root];
    let output = Checker::default().check_program(&Program {
        items,
        module_graph: Some(graph),
        module_doc: None,
    });
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        output.fn_sigs["forward"].return_type,
        Ty::Named {
            name: "Vec".to_string(),
            args: vec![encoding_ty(BuiltinType::JsonValue)],
            builtin: Some(BuiltinType::Vec)
        }
    );
}

#[test]
fn encoding_value_import_alias_cannot_promote_a_same_named_user_resource() {
    let mut items = parsed_items("import user.format as json;");
    let Item::Import(import) = &mut items[0].0 else {
        panic!("import fixture")
    };
    import.resolved_items = Some(parsed_items("#[resource] #[opaque] pub type Value {}").into());
    items.extend(parsed_items(
        "fn identity(value: json.Value) -> json.Value { value }",
    ));
    let output = check_items(items);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let ty = &output.fn_sigs["identity"].return_type;
    assert_eq!(ty, &Ty::named("user.format.Value", vec![]));
    let facts = output.type_facts[&ResolvedTy::from_ty(ty).unwrap().into()];
    assert_eq!(
        (facts.class, facts.clone),
        (ValueClass::AffineResource, CloneKind::None)
    );
}

#[test]
fn encoding_extern_calls_require_the_exact_shipped_declaration_and_signature() {
    use crate::{EncodingFormat, EncodingOp, RuntimeCallFamily};
    for (format, encoding) in [
        ("json", EncodingFormat::Json),
        ("yaml", EncodingFormat::Yaml),
    ] {
        let source = format!(
            r#"
            {VALUE_SOURCE}
            extern "C" {{
                fn hew_{format}_parse(text: string) -> Value;
                fn hew_{format}_object_set(parent: Value, key: string, consume child: Value);
                fn hew_{format}_get_int(value: Value) -> i64;
                fn hew_{format}_eq(left: Value, right: Value) -> i32;
            }}
            pub fn probe(text: string, parent: Value) -> i64 {{
                let parsed = unsafe {{ hew_{format}_parse(text) }};
                unsafe {{ hew_{format}_object_set(parent, text, parsed); }}
                let _ = unsafe {{ hew_{format}_eq(parent, parent) }};
                unsafe {{ hew_{format}_get_int(parent) }}
            }}
        "#
        );
        let module = ["std", "encoding", format].map(str::to_string).to_vec();
        let canonical = check_source_in_canonical_std_module(&source, &module);
        assert!(canonical.errors.is_empty(), "{:?}", canonical.errors);
        for op in [
            EncodingOp::Parse,
            EncodingOp::ObjectSet,
            EncodingOp::GetInt,
            EncodingOp::Eq,
        ] {
            assert!(
                canonical.direct_call_targets.values().any(|target| {
                    *target
                        == CallTarget::Runtime(RuntimeCallFamily::Encoding {
                            format: encoding,
                            op,
                        })
                }),
                "missing {op:?}: {:?}",
                canonical.direct_call_targets
            );
        }
        let lookalike = check_source_in_module(&source, module);
        assert!(lookalike.errors.is_empty(), "{:?}", lookalike.errors);
        assert!(lookalike
            .direct_call_targets
            .values()
            .all(|target| !matches!(
                target,
                CallTarget::Runtime(RuntimeCallFamily::Encoding { .. })
            )));

        // A real source path cannot repair a malformed extern ABI. The value
        // identity still resolves, but its endpoint receives no runtime family.
        let malformed = source
            .replace("-> i64;", "-> i32;")
            .replace("-> i64 {", "-> i32 {");
        let wrong = check_source_in_canonical_std_module(
            &malformed,
            &["std".into(), "encoding".into(), format.into()],
        );
        assert!(wrong.errors.is_empty(), "{:?}", wrong.errors);
        assert!(!wrong.direct_call_targets.values().any(|target| *target
            == CallTarget::Runtime(RuntimeCallFamily::Encoding {
                format: encoding,
                op: EncodingOp::GetInt
            })));
    }
}

#[test]
fn user_extern_with_canonical_value_arguments_never_gains_encoding_authority() {
    let mut items = parsed_items("import std.encoding.json as data;");
    let Item::Import(import) = &mut items[0].0 else {
        panic!("import fixture")
    };
    import.resolved_items = Some(parsed_items(VALUE_SOURCE).into());
    import.resolved_source_paths = vec![std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/encoding/json/json.hew")];
    items.extend(parsed_items(
        r#"
        extern "C" { fn hew_json_get_int(value: data.Value) -> i64; }
        fn probe(value: data.Value) -> i64 { unsafe { hew_json_get_int(value) } }
    "#,
    ));
    let output = check_items(items);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output.direct_call_targets.values().any(|target| matches!(target,
        CallTarget::Extern { endpoint, trusted_compiled_stdlib: false, .. } if endpoint == "hew_json_get_int")));
    assert!(output.direct_call_targets.values().all(|target| !matches!(
        target,
        CallTarget::Runtime(crate::RuntimeCallFamily::Encoding { .. })
    )));
}

#[test]
fn selected_encoding_import_preserves_result_and_option_try_payload_identity() {
    for (format, kind) in [
        ("json", BuiltinType::JsonValue),
        ("yaml", BuiltinType::YamlValue),
    ] {
        let source = format!(
            r#"
            import std.encoding.{format}.{{self, Value}};
            fn required_field(obj: Value, key: string) -> Result<Value, string> {{
                match obj.get_field(key) {{
                    .Ok(.Some(value)) => Ok(value),
                    .Ok(.None) => Err("missing field"),
                    .Err(error) => Err(error),
                }}
            }}
            fn result_probe(obj: Value) -> Result<Value, string> {{
                let child = required_field(obj, "field")?;
                Ok(child)
            }}
            fn optional_field(obj: Value) -> Option<Value> {{ Some(obj) }}
            fn option_probe(obj: Value) -> Option<Value> {{
                let child = optional_field(obj)?;
                Some(child)
            }}
        "#
        );
        let mut items = parsed_items(&source);
        let Item::Import(import) = &mut items[0].0 else {
            panic!("import fixture")
        };
        let imported = parsed_items(&format!(
            r"
            {VALUE_SOURCE}
            impl Value {{
                pub fn get_field(self, key: string) -> Result<Option<Value>, string> {{
                    Ok(Some(self))
                }}
            }}
        "
        ));
        import.resolved_items = Some(imported.clone().into());
        import.resolved_source_paths = vec![std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join(format!("std/encoding/{format}/{format}.hew"))];
        let source_paths = import.resolved_source_paths.clone();
        let root = ModuleId::root();
        let module = ModuleId::new(vec![
            "std".to_string(),
            "encoding".to_string(),
            format.to_string(),
        ]);
        let mut graph = ModuleGraph::new(root.clone());
        for node in [
            Module {
                id: module.clone(),
                items: imported,
                imports: vec![],
                source_paths,
                doc: None,
            },
            Module {
                id: root.clone(),
                items: items.clone(),
                imports: vec![],
                source_paths: vec![],
                doc: None,
            },
        ] {
            graph.add_module(node).unwrap();
        }
        graph.topo_order = vec![module, root];
        let output = Checker::default().check_program(&Program {
            items,
            module_graph: Some(graph),
            module_doc: None,
        });
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        let expected = encoding_ty(kind);
        for (call, function, container) in [
            (
                "required_field(obj, \"field\")",
                "required_field",
                Ty::result(expected.clone(), Ty::String),
            ),
            (
                "optional_field(obj)",
                "optional_field",
                Ty::option(expected.clone()),
            ),
        ] {
            assert_eq!(output.fn_sigs[function].return_type, container);
            let start = source.rfind(call).unwrap();
            let end = start + call.len();
            assert_eq!(output.expr_types[&SpanKey::from(&(start..end))], container);
            assert_eq!(
                output.expr_types[&SpanKey::from(&(start..end + 1))],
                expected
            );
        }
    }
}
