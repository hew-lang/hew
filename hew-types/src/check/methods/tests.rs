//! Unit tests for `methods`, split out of the source file.
#![allow(
    unused_imports,
    clippy::wildcard_imports,
    clippy::module_inception,
    reason = "re-exported for nested test modules' glob imports"
)]
use super::*;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::module_registry::ModuleRegistry;

    /// The dyn layout deduplicates slots by trait method declaration, so
    /// every method of every builtin trait needs its identity.
    #[test]
    fn every_builtin_trait_method_has_a_declaration_identity() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&hew_parser::parse("fn main() {}").program);
        let mut builtin_methods = 0;
        for (key, info) in &checker.trait_defs {
            if !key.starts_with("std.builtins.") {
                continue;
            }
            for method in &info.methods {
                builtin_methods += 1;
                assert!(
                    output
                        .trait_method_ids
                        .contains_key(&format!("{key}::{}", method.name)),
                    "`{key}.{}` has no declaration identity",
                    method.name
                );
            }
        }
        assert!(
            builtin_methods > 0,
            "the builtin prelude registered no traits"
        );
    }

    /// A root trait written over a prelude trait by its bare spelling keeps
    /// an edge to the prelude trait's declaration, so the dyn layout reaches
    /// the supertrait's method identities.
    #[test]
    fn root_supertrait_edges_name_the_prelude_declaration() {
        let source = r"
            trait Pretty: Display { fn pretty(self) -> string; }
            trait Named: Iterator { fn name(self) -> string; }
            trait Failure: Error { fn code(self) -> i64; }
            fn main() {}
        ";
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&hew_parser::parse(source).program);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        for (subtrait, expected) in [
            ("Pretty", "std.builtins.Display"),
            ("Named", "std.builtins.Iterator"),
            ("Failure", "std.builtins.Error"),
        ] {
            let edges = &checker.trait_super[subtrait];
            assert_eq!(edges, &vec![expected.to_string()], "`{subtrait}` edges");
            for method in &checker.trait_defs[expected].methods {
                assert!(
                    output
                        .trait_method_ids
                        .contains_key(&format!("{expected}::{}", method.name)),
                    "`{expected}.{}` has no declaration identity",
                    method.name
                );
            }
        }
    }

    #[test]
    fn trait_method_target_ids_fail_closed_after_canonical_miss() {
        let mut checker = Checker {
            current_module: Some("app".to_string()),
            ..Checker::default()
        };
        checker.trait_defs.insert(
            "left.Render".to_string(),
            TraitInfo {
                source_module: None,
                file_index: 0,
                methods: Vec::new(),
                associated_types: Vec::new(),
                type_params: Vec::new(),
            },
        );
        checker
            .published_bare_trait_owners
            .entry((
                checker.current_module.clone(),
                checker.current_module_idx,
                "Render".to_string(),
            ))
            .or_default()
            .insert("left.Render".to_string());

        let wrong_trait = checker.defs.mint_for_test("right.Render");
        let wrong_method = checker.defs.mint_for_test("right.Render::render");
        checker
            .trait_method_ids
            .insert("Render::render".to_string(), (wrong_trait, wrong_method));

        assert_eq!(
            checker.trait_method_call_target_ids("Render", "render"),
            None,
            "a canonical lookup miss must not retry the first-write-wins bare key",
        );

        let canonical_trait = checker.defs.mint_for_test("left.Render");
        let canonical_method = checker.defs.mint_for_test("left.Render::render");
        checker.trait_method_ids.insert(
            "left.Render::render".to_string(),
            (canonical_trait, canonical_method),
        );
        assert_eq!(
            checker.trait_method_call_target_ids("Render", "render"),
            Some((canonical_trait, canonical_method)),
        );
    }

    #[test]
    fn canonical_std_io_runtime_methods_require_provenance_and_checked_signature() {
        let push_signature = FnSig {
            params: vec![Ty::U8],
            return_type: Ty::Unit,
            ..FnSig::default()
        };
        let mut canonical = Checker::default();
        canonical.extern_method_origins.insert(
            "bytes::push".to_string(),
            (Some("std.io".to_string()), true),
        );
        assert_eq!(
            canonical.canonical_std_io_runtime_method_family(
                "bytes::push",
                "hew_bytes_push",
                &push_signature,
            ),
            Some(crate::runtime_call::RuntimeCallFamily::BytesPush),
        );

        let lookalike = Checker::default();
        assert_eq!(
            lookalike.canonical_std_io_runtime_method_family(
                "bytes::push",
                "hew_bytes_push",
                &push_signature,
            ),
            None,
            "a user extern sharing the runtime spelling must remain an Extern call",
        );

        let wrong_signature = FnSig {
            params: vec![Ty::I64],
            return_type: Ty::Unit,
            ..FnSig::default()
        };
        assert_eq!(
            canonical.canonical_std_io_runtime_method_family(
                "bytes::push",
                "hew_bytes_push",
                &wrong_signature,
            ),
            None,
            "the runtime ABI family is not admitted by symbol spelling and arity alone",
        );

        let len_signature = FnSig {
            params: vec![],
            return_type: Ty::I64,
            ..FnSig::default()
        };
        canonical.extern_method_origins.insert(
            "string::len".to_string(),
            (Some("std.string".to_string()), true),
        );
        canonical.extern_method_origins.insert(
            "string::byte_len".to_string(),
            (Some("std.string".to_string()), true),
        );
        assert_eq!(
            canonical.canonical_std_io_runtime_method_family(
                "string::byte_len",
                "hew_string_byte_length",
                &len_signature,
            ),
            Some(crate::runtime_call::RuntimeCallFamily::StringByteLen),
        );
        assert_eq!(
            lookalike.canonical_std_io_runtime_method_family(
                "string::byte_len",
                "hew_string_byte_length",
                &len_signature,
            ),
            None,
            "a user extern cannot select the byte-length runtime operation",
        );
        assert_eq!(
            canonical.canonical_std_io_runtime_method_family(
                "string::len",
                "hew_string_length",
                &len_signature,
            ),
            Some(crate::runtime_call::RuntimeCallFamily::StringLen),
        );
        assert_eq!(
            lookalike.canonical_std_io_runtime_method_family(
                "string::len",
                "hew_string_length",
                &len_signature,
            ),
            None,
            "a user string len extern sharing the runtime spelling must remain untrusted",
        );
    }

    #[test]
    fn wasm_file_stream_function_policy_uses_canonical_std_owner_not_spellings() {
        let span = 0..0;
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker
            .canonical_std_module_sources
            .insert("std.fs".to_string());
        checker
            .module_import_bindings
            .insert((None, 0, "files".to_string()), "std.fs".to_string());
        checker.reject_wasm_native_only_module_function("files", "read", &span);
        assert_eq!(checker.errors.len(), 1, "module alias must reject");

        // Use a fresh checker: the production de-duplication key intentionally
        // suppresses repeated diagnostics at one source span.
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker
            .canonical_std_module_sources
            .insert("std.fs".to_string());
        checker.reject_wasm_native_only_function_identity("std.fs.read", &span);
        assert_eq!(checker.errors.len(), 1, "named-import identity must reject");

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker
            .module_import_bindings
            .insert((None, 0, "lookalike".to_string()), "app.fs".to_string());
        checker.reject_wasm_native_only_module_function("lookalike", "read", &span);
        checker.reject_wasm_native_only_function_identity("app.fs.read", &span);
        assert!(
            checker.errors.is_empty(),
            "a user package with the same leaf must not inherit std.fs policy"
        );
    }

    #[test]
    fn ask_reply_send_gate_uses_exact_import_owner_and_fails_closed_without_it() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker
            .registry
            .register_type("hew.replysend.Reply".to_string(), vec![Ty::I64]);
        checker.registry.register_type(
            "hew.replynonsend.Reply".to_string(),
            vec![Ty::Named {
                name: "Rc".to_string(),
                args: vec![Ty::I64],
                builtin: Some(BuiltinType::Rc),
            }],
        );
        checker.module_import_bindings.insert(
            (None, 0, "replysend".to_string()),
            "hew.replysend".to_string(),
        );
        checker.module_import_bindings.insert(
            (None, 0, "replynonsend".to_string()),
            "hew.replynonsend".to_string(),
        );
        let bare_reply = Ty::Named {
            name: "Reply".to_string(),
            args: Vec::new(),
            builtin: None,
        };

        let send = checker
            .send_gate_reply_ty("replysend.Producer::make", &bare_reply)
            .expect("an exact replysend binding and marker row must resolve");
        assert!(matches!(send, Ty::Named { ref name, .. } if name == "hew.replysend.Reply"));
        assert!(checker.registry.implements_marker(&send, MarkerTrait::Send));

        let non_send = checker
            .send_gate_reply_ty("replynonsend.Producer::make", &bare_reply)
            .expect("an exact replynonsend binding and marker row must resolve");
        assert!(matches!(non_send, Ty::Named { ref name, .. } if name == "hew.replynonsend.Reply"));
        assert!(!checker
            .registry
            .implements_marker(&non_send, MarkerTrait::Send));

        assert!(
            checker
                .send_gate_reply_ty("missing.Producer::make", &bare_reply)
                .is_none(),
            "a missing lexical module binding must not fall back to bare Reply"
        );
    }

    #[test]
    fn qualified_method_receiver_restores_only_its_own_return_identity() {
        fn empty_type_def(name: &str) -> TypeDef {
            TypeDef {
                kind: TypeDefKind::Struct,
                name: name.to_string(),
                type_params: Vec::new(),
                bounds: HashMap::new(),
                fields: HashMap::new(),
                field_order: Vec::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            }
        }

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        for identity in [
            "net.Listener",
            "net.Connection",
            "foo.Listener",
            "foo.Connection",
        ] {
            checker
                .type_defs
                .insert(identity.to_string(), empty_type_def(identity));
        }
        let bare_connection = Ty::Named {
            name: "Connection".to_string(),
            args: Vec::new(),
            builtin: None,
        };

        assert_eq!(
            checker.qualify_method_return_to_receiver_owner("net.Listener", &bare_connection,),
            Ty::Named {
                name: "net.Connection".to_string(),
                args: Vec::new(),
                builtin: None,
            },
            "a `net.Listener` method's module-local `Connection` return must regain `net` ownership",
        );
        assert_eq!(
            checker.qualify_method_return_to_receiver_owner("foo.Listener", &bare_connection,),
            Ty::Named {
                name: "foo.Connection".to_string(),
                args: Vec::new(),
                builtin: None,
            },
            "a foreign same-short-name method result must retain its own source owner",
        );

        let foreign_connection = Ty::Named {
            name: "foo.Connection".to_string(),
            args: Vec::new(),
            builtin: None,
        };
        assert_eq!(
            checker.qualify_method_return_to_receiver_owner("net.Listener", &foreign_connection,),
            foreign_connection,
            "an already-qualified foreign result is authoritative and must not be rewritten",
        );
        assert_eq!(
            checker.qualify_method_return_to_receiver_owner("Listener", &bare_connection,),
            bare_connection,
            "a root-local receiver has no module owner to project onto its result",
        );
    }

    #[test]
    fn finalize_lowering_facts_silently_drops_error_element_type() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 10..20;
        checker.record_hashset_lowering_fact(&span, &Ty::Error);

        let facts = checker.finalize_lowering_facts();

        assert!(
            facts.is_empty(),
            "a pending fact with Ty::Error element must not appear in the finalized output"
        );
        assert!(
            checker.errors.is_empty(),
            "finalize_lowering_facts must not emit a spurious error for Ty::Error elements; \
             the real error was reported upstream"
        );
    }

    /// A pending lowering fact whose element type is genuinely unresolved
    /// (`Ty::Var`) after inference must be pruned AND must emit an
    /// `InferenceFailed` diagnostic pointing at the lowering site.
    #[test]
    fn finalize_lowering_facts_emits_error_for_unresolved_inference_var() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 30..40;
        let unresolved_var = Ty::Var(crate::ty::TypeVar::fresh());
        checker.record_hashset_lowering_fact(&span, &unresolved_var);

        let facts = checker.finalize_lowering_facts();

        assert!(
            facts.is_empty(),
            "a pending fact with an unresolved Ty::Var element must not appear in the output"
        );
        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_lowering_facts must emit InferenceFailed for a genuinely unresolved \
             element type; got: {:?}",
            checker.errors
        );
    }

    // ── HashMap admission finalization ───────────────────────────────────────

    /// A deferred `HashMap` admission whose key type is `Ty::Error` must be
    /// dropped silently — no new diagnostic, no cascade.
    #[test]
    fn finalize_hashmap_admission_silently_drops_error_key() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 10..20;
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::Error,
                val_ty: Ty::I64,
                source_module: None,
                type_param_bounds: HashMap::new(),
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_hashmap_admission must not emit an error when key_ty is Ty::Error; \
             the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `HashMap` admission whose value type is still an unresolved
    /// inference variable after inference must emit `InferenceFailed`.
    #[test]
    fn finalize_hashmap_admission_emits_inference_failed_for_var_value() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 30..40;
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::String,
                val_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
                type_param_bounds: HashMap::new(),
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_hashmap_admission must emit InferenceFailed when val_ty is an unresolved \
             Ty::Var; got: {:?}",
            checker.errors
        );
    }

    /// A bare key type parameter is checked against the bounds recorded at
    /// the deferred site, not skipped: `K: Hash + Eq` must admit cleanly.
    #[test]
    fn finalize_hashmap_admission_admits_abstract_key_param_with_hash_eq_bounds() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 60..70;
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("K".to_string(), vec![]),
                val_ty: Ty::normalize_named("V".to_string(), vec![]),
                source_module: None,
                type_param_bounds: HashMap::from([
                    ("K".into(), vec!["Hash".into(), "Eq".into()]),
                    ("V".into(), vec![]),
                ]),
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker.errors.is_empty(),
            "K: Hash + Eq must satisfy Map key admission via its declared bounds; got: {:?}",
            checker.errors
        );
    }

    /// Negative control: a bare key type parameter without a `Hash` bound
    /// must still be refused, so admission decides from the recorded bounds
    /// rather than skipping bare type parameters altogether.
    #[test]
    fn finalize_hashmap_admission_rejects_abstract_key_param_missing_hash_bound() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 60..70;
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("K".to_string(), vec![]),
                val_ty: Ty::normalize_named("V".to_string(), vec![]),
                source_module: None,
                type_param_bounds: HashMap::from([
                    ("K".into(), vec!["Eq".into()]),
                    ("V".into(), vec![]),
                ]),
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::BoundsNotSatisfied && e.message.contains("Hash")),
            "K without a Hash bound must still be rejected as a Map key; got: {:?}",
            checker.errors
        );
    }

    #[test]
    fn record_resolved_hashmap_call_abstract_key_emits_resolved_call() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker
            .current_type_param_bounds
            .push(crate::check::types::TypeParamScope::new(
                std::collections::HashMap::from([
                    ("K".to_string(), vec!["Hash".to_string(), "Eq".to_string()]),
                    ("V".to_string(), vec![]),
                ]),
                std::collections::HashMap::new(),
            ));
        let span = 80..90;

        checker.record_resolved_hashmap_call(
            "insert",
            &Ty::normalize_named("K".to_string(), vec![]),
            &Ty::normalize_named("V".to_string(), vec![]),
            &span,
        );

        assert!(
            checker.errors.is_empty(),
            "declared K: Hash + Eq bounds must satisfy HashMap method dispatch; got: {:?}",
            checker.errors
        );
        let call = checker
            .resolved_calls
            .get(&SpanKey::in_module(&span, 0))
            .expect("generic HashMap method dispatch must record a resolved call");
        assert_eq!(call.method_target.symbol_name, "hew_hashmap_insert_layout");
    }

    /// Two deferred `HashMap` admissions sharing the same unresolved
    /// `(key_var, val_var)` pair must emit exactly one `InferenceFailed`.
    #[test]
    fn finalize_hashmap_admission_dedup_pair_emits_single_error() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let key_var = TypeVar::fresh();
        let val_var = TypeVar::fresh();
        let span_a = 100..110;
        let span_b = 200..210;

        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span_a, 0),
            DeferredHashMapAdmission {
                span: span_a.clone(),
                key_ty: Ty::Var(key_var),
                val_ty: Ty::Var(val_var),
                source_module: None,
                type_param_bounds: HashMap::new(),
            },
        );
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span_b, 0),
            DeferredHashMapAdmission {
                span: span_b.clone(),
                key_ty: Ty::Var(key_var),
                val_ty: Ty::Var(val_var),
                source_module: None,
                type_param_bounds: HashMap::new(),
            },
        );

        checker.finalize_hashmap_admission();

        let inference_failed: Vec<_> = checker
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
            .collect();
        assert_eq!(
            inference_failed.len(),
            1,
            "two admissions sharing the same (key_var, val_var) pair must emit exactly one \
             InferenceFailed; got {}: {:?}",
            inference_failed.len(),
            checker.errors,
        );
    }

    // ── HashSet admission finalization ───────────────────────────────────────

    /// A deferred `HashSet` admission whose element type is `Ty::Error` must be
    /// dropped silently — mirrors the lowering-facts sentinel for the admission path.
    #[test]
    fn finalize_hashset_admission_silently_drops_error_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 50..60;
        checker.deferred_hashset_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashSetAdmission {
                span: span.clone(),
                elem_ty: Ty::Error,
                source_module: None,
            },
        );

        checker.finalize_hashset_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_hashset_admission must not emit an error when elem_ty is Ty::Error; \
             the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `HashSet` admission whose element type is still an unresolved
    /// inference variable after inference must emit `InferenceFailed` with an
    /// "add annotation" hint.
    #[test]
    fn finalize_hashset_admission_emits_inference_failed_for_var_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 70..80;
        checker.deferred_hashset_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashSetAdmission {
                span: span.clone(),
                elem_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
            },
        );

        checker.finalize_hashset_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_hashset_admission must emit InferenceFailed for an unresolved \
             Ty::Var element; got: {:?}",
            checker.errors
        );
    }

    // ── Vec admission finalization ───────────────────────────────────────────

    /// A deferred `Vec` admission whose element type contains `Ty::Error` (via
    /// `contains_error()`) must be dropped silently — no cascade.
    #[test]
    fn finalize_vec_admission_silently_drops_error_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 90..100;
        // Nest Ty::Error inside a Vec element to exercise the contains_error() path,
        // not just a bare Ty::Error match.
        let elem_ty = Ty::Named {
            builtin: None,
            name: "Result".into(),
            args: vec![Ty::Error, Ty::I64],
        };
        checker.deferred_vec_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredVecAdmission {
                span: span.clone(),
                elem_ty,
                source_module: None,
            },
        );

        checker.finalize_vec_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_vec_admission must not emit an error when the element type \
             contains Ty::Error; the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `Vec` admission whose element type contains an unresolved
    /// inference variable after inference must emit `InferenceFailed`.
    #[test]
    fn finalize_vec_admission_emits_inference_failed_for_unresolved_var() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 110..120;
        checker.deferred_vec_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredVecAdmission {
                span: span.clone(),
                elem_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
            },
        );

        checker.finalize_vec_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_vec_admission must emit InferenceFailed for an unresolved \
             Ty::Var element; got: {:?}",
            checker.errors
        );
    }

    // ── HashMap/HashSet layout-symbol rewrite recording ──────────────────────
    //
    // W4.001 Stage C3 hard cutover: the legacy
    // `resolve_hashmap_runtime_symbol` / `resolve_hashset_runtime_symbol`
    // per-V helpers + dual-emit `MethodCallRewrite::RewriteToFunction`
    // arms retired. Resolver-authority via `record_resolved_collection_call`
    // is now the sole admission and dispatch path; coverage lives in
    // `tests/resolved_call_hashmap_coverage.rs` and
    // `tests/resolved_call_hashset_coverage.rs` (catalog-side) plus the
    // C2 `resolved_impl_call_hashmap_layout_descriptor_materialisation`
    // integration test (codegen-side).

    // ── W4.048: descriptor-driven collection method resolver ─────────────────
    //
    // Slice 1 isolation tests for the pure-data descriptor table. These pin the
    // arity / arg-shape / return-shape of every table-driven collection method
    // so a row edit that would silently drift the front-half admission contract
    // fails here before it reaches the behaviour-diff corpus.

    fn arity_of(kind: CollectionKind, method: &str) -> Option<usize> {
        collection_method_desc(kind, method)
            .expect("known method")
            .arity
    }

    #[test]
    fn descriptor_table_preserves_existing_length_arities() {
        for kind in [CollectionKind::HashMap, CollectionKind::HashSet] {
            assert_eq!(arity_of(kind, "len"), None, "{kind:?}::len skips arity");
        }
        assert_eq!(arity_of(CollectionKind::HashSet, "is_empty"), None);
        assert_eq!(arity_of(CollectionKind::HashMap, "is_empty"), Some(0));
    }

    #[test]
    fn descriptor_table_checked_arities() {
        assert_eq!(arity_of(CollectionKind::HashMap, "insert"), Some(2));
        // HashMap `get` is no longer in the descriptor table: it is trait-routed
        // (`<HashMap<K, V> as Index>::get -> Option<V>`) through the explicit
        // `check_hashmap_method` arm, not the collection driver (mirrors Vec).
        assert!(collection_method_desc(CollectionKind::HashMap, "get").is_none());
        assert_eq!(arity_of(CollectionKind::HashMap, "keys"), Some(0));
        assert_eq!(arity_of(CollectionKind::HashMap, "entries"), Some(0));
        assert_eq!(arity_of(CollectionKind::HashSet, "insert"), Some(1));
        assert_eq!(arity_of(CollectionKind::HashSet, "contains"), Some(1));
        assert_eq!(arity_of(CollectionKind::HashSet, "clone"), Some(0));
    }

    #[test]
    fn descriptor_table_arg_and_return_shapes() {
        let hm_insert = collection_method_desc(CollectionKind::HashMap, "insert").unwrap();
        assert_eq!(
            hm_insert.arg_templates,
            &[ArgTemplate::Key, ArgTemplate::Value]
        );
        assert_eq!(hm_insert.ret, RetTemplate::Unit);

        // HashMap `get` is intentionally absent from the descriptor table: the
        // accessor is trait-routed through `Index<K>` (see
        // `descriptor_table_checked_arities`).
        assert!(collection_method_desc(CollectionKind::HashMap, "get").is_none());

        let hm_keys = collection_method_desc(CollectionKind::HashMap, "keys").unwrap();
        assert_eq!(hm_keys.ret, RetTemplate::VecOfKey);
        let hm_values = collection_method_desc(CollectionKind::HashMap, "values").unwrap();
        assert_eq!(hm_values.ret, RetTemplate::VecOfVal);
        let hm_entries = collection_method_desc(CollectionKind::HashMap, "entries").unwrap();
        assert_eq!(hm_entries.ret, RetTemplate::VecOfPair);

        let set_insert = collection_method_desc(CollectionKind::HashSet, "insert").unwrap();
        assert_eq!(set_insert.arg_templates, &[ArgTemplate::Elem]);
        assert_eq!(set_insert.ret, RetTemplate::Bool);

        for clone_kind in [CollectionKind::HashMap, CollectionKind::HashSet] {
            assert_eq!(
                collection_method_desc(clone_kind, "clone").unwrap().ret,
                RetTemplate::SelfTy,
                "{clone_kind:?}::clone returns Self"
            );
        }
    }

    #[test]
    fn descriptor_table_unknown_and_divergent_methods_have_no_row() {
        // Unknown methods → fail-closed fallback (no descriptor row).
        assert!(collection_method_desc(CollectionKind::HashMap, "frobnicate").is_none());
    }

    #[test]
    fn builtin_vec_signatures_match_source_authority() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.register_builtins();
        for spec in crate::vec_authority::method_specs() {
            let sig = checker
                .lookup_builtin_vec_method_sig(&[Ty::I64], &spec.name)
                .unwrap_or_else(|| panic!("missing source signature for Vec::{}", spec.name));
            assert_eq!(
                sig.extern_symbol
                    .as_ref()
                    .map(|symbol| symbol.template.raw.as_str()),
                Some(spec.template.raw.as_str()),
                "Vec::{} signature/template drift",
                spec.name
            );
        }
    }
}
