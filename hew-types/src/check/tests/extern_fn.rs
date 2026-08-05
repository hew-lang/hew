#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

const BORROW_OUTSIDE_EXTERN_MESSAGE: &str =
    "`&T` is only allowed in `extern` function signatures; write `T` in ordinary Hew code";

fn inject_borrow(ty: &mut Spanned<TypeExpr>, span: Span) {
    let inner = std::mem::replace(ty, (TypeExpr::Infer, span.clone()));
    *ty = (TypeExpr::Borrow(Box::new(inner)), span);
}

fn assert_one_borrow_outside_extern(errors: &[TypeError], span: Span) {
    assert_eq!(errors.len(), 1, "expected one error, got: {errors:#?}");
    assert_eq!(
        errors[0].kind,
        TypeErrorKind::BorrowTypeOutsideExternSignature
    );
    assert_eq!(errors[0].span, span);
    assert_eq!(errors[0].message, BORROW_OUTSIDE_EXTERN_MESSAGE);
}

#[test]
fn extern_borrow_signature_registers_exact_types() {
    let output = check_source(
        r#"
        extern "C" {
            fn read(value: &i64) -> &i64;
        }
        "#,
    );
    assert!(output.errors.is_empty(), "errors: {:#?}", output.errors);
    let sig = output.fn_sigs.get("read").expect("extern signature");
    let borrow_i64 = Ty::Borrow {
        pointee: Box::new(Ty::I64),
    };
    assert_eq!(sig.params, vec![borrow_i64.clone()]);
    assert_eq!(sig.return_type, borrow_i64);
}

#[test]
fn duplicate_extern_symbol_rejects_type_and_ownership_drift() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_duplicate")]
            fn first(consume value: string) -> i64;
            #[extern_symbol("hew_duplicate")]
            fn second(value: bytes) -> i64;
        }
        "#,
    );
    let conflicts = output
        .errors
        .iter()
        .filter(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::ConflictingExternDeclaration { symbol_name }
                    if symbol_name == "hew_duplicate"
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(conflicts.len(), 1, "errors: {:#?}", output.errors);
    assert!(conflicts[0].message.contains("consume string"));
    assert!(conflicts[0].message.contains("bytes"));
    assert!(
        conflicts[0].notes.iter().any(|note| note
            .1
            .contains("extern \"C\" symbol declarations are program-wide unique")),
        "notes: {:#?}",
        conflicts[0].notes
    );
}

#[test]
fn duplicate_extern_symbol_rejects_ownership_only_drift() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_duplicate_mode")]
            fn first(consume value: string);
            #[extern_symbol("hew_duplicate_mode")]
            fn second(value: string);
        }
        "#,
    );
    let conflict = output
        .errors
        .iter()
        .find(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::ConflictingExternDeclaration { symbol_name }
                    if symbol_name == "hew_duplicate_mode"
            )
        })
        .unwrap_or_else(|| panic!("errors: {:#?}", output.errors));
    assert!(
        conflict.notes.iter().any(|note| note
            .1
            .contains("extern \"C\" symbol declarations are program-wide unique")),
        "notes: {:#?}",
        conflict.notes
    );
}

#[test]
fn duplicate_extern_symbol_accepts_identical_contracts() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_same")]
            fn first(consume value: string) -> i64;
            #[extern_symbol("hew_same")]
            fn second(consume value: string) -> i64;
        }
        "#,
    );
    assert!(
        !output.errors.iter().any(|error| matches!(
            error.kind,
            TypeErrorKind::ConflictingExternDeclaration { .. }
        )),
        "identical extern contracts must coexist: {:#?}",
        output.errors
    );
}

/// Single-owner property (rc1-F1 stage B): an agreeing re-declaration does
/// not create a second contract — both declaration keys resolve to the ONE
/// established contract, owned by the first declaration.
#[test]
fn agreeing_duplicate_declarations_resolve_to_one_contract() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_one_contract")]
            fn first(consume value: string) -> i64;
            #[extern_symbol("hew_one_contract")]
            fn second(consume value: string) -> i64;
        }
        "#,
    );
    assert!(output.errors.is_empty(), "errors: {:#?}", output.errors);
    let (_, contract) = output
        .extern_contracts
        .established("hew_one_contract")
        .expect("symbol must carry a contract");
    assert_eq!(
        contract.owner.full_path(),
        "first",
        "the first declaration owns the contract"
    );
    let first = output
        .extern_contracts
        .contract_for_declaration("first")
        .expect("minting declaration resolves to the contract");
    let second = output
        .extern_contracts
        .contract_for_declaration("second")
        .expect("agreeing re-declaration adopts the contract");
    assert_eq!(
        first.owner, second.owner,
        "one symbol, one contract: both declaration keys resolve to the same owner"
    );
}

#[test]
fn duplicate_extern_symbol_accepts_cross_module_alias_qualified_contracts() {
    let stream = hew_parser::parse(
        r#"
        pub type Stream<T> {}
        extern "C" {
            #[extern_symbol("hew_cross_module_same")]
            fn first() -> Stream<bytes>;
            #[extern_symbol("hew_cross_module_drift")]
            fn first_drift() -> Stream<bytes>;
        }
        "#,
    );
    assert!(stream.errors.is_empty(), "parse: {:#?}", stream.errors);
    let mut net = hew_parser::parse(
        r#"
        import std::stream;
        extern "C" {
            #[extern_symbol("hew_cross_module_same")]
            fn second() -> stream.Stream<bytes>;
            #[extern_symbol("hew_cross_module_drift")]
            fn second_drift() -> stream.Stream<string>;
        }
        "#,
    );
    assert!(net.errors.is_empty(), "parse: {:#?}", net.errors);
    let import_span = net.program.items[0].1.clone();
    let Item::Import(import) = &mut net.program.items[0].0 else {
        panic!("expected import");
    };
    import.resolved_items = Some(stream.program.items.clone());

    let root_id = ModuleId::root();
    let stream_id = ModuleId::new(vec!["std".to_string(), "stream".to_string()]);
    let net_id = ModuleId::new(vec!["std".to_string(), "net".to_string()]);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: stream_id.clone(),
            items: stream.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add stream module");
    graph
        .add_module(Module {
            id: net_id.clone(),
            items: net.program.items,
            imports: vec![hew_parser::module::ModuleImport {
                target: stream_id.clone(),
                spec: None,
                span: import_span,
            }],
            source_paths: vec![],
            doc: None,
        })
        .expect("add net module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add root module");
    graph.topo_order = vec![stream_id, net_id, root_id];

    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&Program {
        items: vec![],
        module_graph: Some(graph),
        module_doc: None,
    });
    assert!(
        !output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::ConflictingExternDeclaration { symbol_name }
                if symbol_name == "hew_cross_module_same"
        )),
        "alias-qualified copies of one nominal contract must coexist: {:#?}",
        output.errors
    );
    let conflict = output
        .errors
        .iter()
        .find(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::ConflictingExternDeclaration { symbol_name }
                    if symbol_name == "hew_cross_module_drift"
            )
        })
        .expect("cross-module contract drift must be rejected");
    assert_eq!(
        conflict.notes[0].2.as_deref(),
        Some("std.stream"),
        "the established declaration note must retain its own source module"
    );
}

#[test]
fn injected_ordinary_function_borrow_fails_closed() {
    let mut parsed = hew_parser::parse("fn ordinary(value: i64) {}");
    assert!(parsed.errors.is_empty());
    let borrow_span = 19..20;
    let Item::Function(function) = &mut parsed.program.items[0].0 else {
        panic!("expected function");
    };
    inject_borrow(&mut function.params[0].ty, borrow_span.clone());

    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert_one_borrow_outside_extern(&output.errors, borrow_span);
    assert!(output
        .fn_sigs
        .get("ordinary")
        .is_none_or(|sig| { sig.params.iter().all(|ty| !matches!(ty, Ty::Borrow { .. })) }));
}

#[test]
fn injected_ordinary_field_and_alias_borrows_fail_closed() {
    let mut field_program = hew_parser::parse("type Holder { value: i64 }").program;
    let field_span = 21..22;
    let Item::TypeDecl(decl) = &mut field_program.items[0].0 else {
        panic!("expected type declaration");
    };
    let TypeBodyItem::Field { ty, .. } = &mut decl.body[0] else {
        panic!("expected field");
    };
    inject_borrow(ty, field_span.clone());
    let field_output = Checker::new(ModuleRegistry::new(vec![])).check_program(&field_program);
    assert_one_borrow_outside_extern(&field_output.errors, field_span);
    assert!(field_output.type_defs.get("Holder").is_none_or(|def| {
        def.fields
            .get("value")
            .is_none_or(|ty| !matches!(ty, Ty::Borrow { .. }))
    }));

    let mut alias_program = hew_parser::parse("type Alias = i64;").program;
    let alias_span = 13..14;
    let Item::TypeAlias(alias) = &mut alias_program.items[0].0 else {
        panic!("expected type alias");
    };
    inject_borrow(&mut alias.ty, alias_span.clone());
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let alias_output = checker.check_program(&alias_program);
    assert_one_borrow_outside_extern(&alias_output.errors, alias_span);
    assert!(checker
        .type_aliases
        .get("Alias")
        .is_none_or(|ty| !matches!(ty, Ty::Borrow { .. })));
}

#[test]
fn checker_extern_context_does_not_leak_to_ordinary_signature() {
    let mut parsed =
        hew_parser::parse("extern \"C\" { fn get() -> &i64; } fn ordinary() -> i64 { 0 }");
    assert!(parsed.errors.is_empty());
    let borrow_span = 54..55;
    let Item::Function(function) = &mut parsed.program.items[1].0 else {
        panic!("expected ordinary function");
    };
    inject_borrow(
        function.return_type.as_mut().expect("return type"),
        borrow_span.clone(),
    );

    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert_one_borrow_outside_extern(&output.errors, borrow_span);
    assert!(matches!(
        output.fn_sigs["get"].return_type,
        Ty::Borrow { .. }
    ));
    assert!(output
        .fn_sigs
        .get("ordinary")
        .is_none_or(|sig| { !matches!(sig.return_type, Ty::Borrow { .. }) }));
}

// ── W3.001 Stage 2 — #[extern_symbol] ingest into FnSig.extern_symbol ─────────

/// `#[extern_symbol("…")]` on an `extern "C"` block fn populates
/// `FnSig.extern_symbol` with a parsed structured template.
#[test]
fn extern_symbol_on_extern_c_fn_populates_fn_sig_spec() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_vec_push_{T}")]
            fn vec_push(v: i64, x: i64);
        }
        "#,
    );
    let sig = output
        .fn_sigs
        .get("vec_push")
        .expect("extern fn must be registered");
    let spec = sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must populate extern_symbol on the FnSig");
    assert_eq!(spec.template.raw, "hew_vec_push_{T}");
    assert_eq!(
        spec.template.placeholders,
        vec![crate::extern_symbol::PlaceholderName::T]
    );
    assert!(
        output.errors.is_empty(),
        "well-formed template must not emit diagnostics, got: {:#?}",
        output.errors
    );
}

/// A generic declarative runtime method must retain its concrete, expanded
/// linker endpoint at the call site.  The ownership graph is keyed by the same
/// resolved call span, so downstream passes never have to recover either the
/// endpoint or move-out semantics from the source spelling `pop`.
#[test]
fn generic_extern_symbol_call_keeps_exact_endpoint_and_move_out_fact() {
    let parsed = hew_parser::parse(
        r"
        fn take_last(values: Vec<string>) -> string {
            values.pop()
        }
        ",
    );
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);

    let (call_span, call) = output
        .resolved_calls
        .iter()
        .find(|(_, call)| call.method_target.symbol_name == "hew_vec_pop_str")
        .expect("Vec<string>::pop must preserve its exact expanded extern endpoint");
    assert!(matches!(
        call.method_target.family,
        crate::check::dispatch::MethodTargetFamily::Vec(crate::check::dispatch::VecMethod::Pop)
    ));

    let fact = output
        .produced_value_ownership
        .get(call_span)
        .expect("resolved generic extern call must publish one ownership fact");
    assert_eq!(
        fact.ownership,
        crate::runtime_call::ProducedValueOwnership::owned(
            crate::runtime_call::ProducedValueAcquisition::MoveOut,
        )
    );
    assert_eq!(
        fact.receiver_boundary,
        Some(crate::runtime_call::ProducedArgumentBoundary::Borrow)
    );
    assert!(fact.arguments.is_empty());
}

#[test]
fn open_extern_method_keeps_signature_modes_separate_from_endpoint() {
    let output = check_source(
        r#"
        type Router {}

        impl Router {
            #[extern_symbol(hew_test_router_route)]
            fn route(self, consume payload: string, note: string) -> i64 {
                0
            }
        }

        fn route_named(router: Router) -> i64 {
            router.route(note: "trace", payload: "body")
        }

        fn route_positional(router: Router) -> i64 {
            router.route("body", "trace")
        }
        "#,
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);

    let mut facts: Vec<_> = output
        .method_call_rewrites
        .iter()
        .filter_map(|(span, rewrite)| match rewrite {
            MethodCallRewrite::RewriteToFunction {
                c_symbol,
                extern_identity: Some(identity),
                ..
            } if c_symbol == "hew_test_router_route" => {
                assert_eq!(identity.endpoint, "hew_test_router_route");
                assert_eq!(identity.signature_key, "Router::route");
                Some(
                    output
                        .produced_value_ownership
                        .get(span)
                        .expect("open extern call must publish boundary modes"),
                )
            }
            _ => None,
        })
        .collect();
    facts.sort_by_key(|fact| {
        fact.arguments[0] == crate::runtime_call::ProducedArgumentBoundary::Borrow
    });
    assert_eq!(facts.len(), 2);
    assert!(facts.iter().any(|fact| {
        fact.arguments
            == [
                crate::runtime_call::ProducedArgumentBoundary::Borrow,
                crate::runtime_call::ProducedArgumentBoundary::Transfer,
            ]
    }));
    assert!(facts.iter().any(|fact| {
        fact.arguments
            == [
                crate::runtime_call::ProducedArgumentBoundary::Transfer,
                crate::runtime_call::ProducedArgumentBoundary::Borrow,
            ]
    }));
}

#[test]
fn compiled_stdlib_extern_method_uses_exact_contract_for_fresh_result() {
    let parsed = hew_parser::parse(
        r"
        fn encode(value: string) -> bytes {
            value.to_bytes()
        }
        ",
    );
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);

    let (span, identity) = output
        .method_call_rewrites
        .iter()
        .find_map(|(span, rewrite)| match rewrite {
            MethodCallRewrite::RewriteToFunction {
                c_symbol,
                extern_identity: Some(identity),
                ..
            } if c_symbol == "hew_string_to_bytes" => Some((span, identity)),
            _ => None,
        })
        .expect("string.to_bytes must carry its exact extern identity");
    assert_eq!(identity.endpoint, "hew_string_to_bytes");
    assert_eq!(identity.signature_key, "string::to_bytes");
    assert_eq!(identity.declaring_module.as_deref(), Some("std.string"));
    assert!(identity.trusted_compiled_stdlib);

    let fact = output
        .produced_value_ownership
        .get(span)
        .expect("string.to_bytes must publish a result ownership fact");
    assert_eq!(
        fact.ownership,
        crate::runtime_call::ProducedValueOwnership::owned(
            crate::runtime_call::ProducedValueAcquisition::Fresh,
        )
    );
    assert_eq!(
        fact.receiver_boundary,
        Some(crate::runtime_call::ProducedArgumentBoundary::Borrow)
    );
    assert!(fact.arguments.is_empty());
}

/// Extern fns without `#[extern_symbol]` carry `None` (regression
/// guard: the field must remain opt-in and not default to a synthetic
/// template derived from the fn name).
#[test]
fn extern_fn_without_extern_symbol_attribute_has_none_spec() {
    let output = check_source(
        r#"
        extern "C" {
            fn unrelated(x: i64) -> i64;
        }
        "#,
    );
    let sig = output
        .fn_sigs
        .get("unrelated")
        .expect("extern fn must be registered");
    assert!(
        sig.extern_symbol.is_none(),
        "fn without #[extern_symbol] must not carry a spec"
    );
}

/// `#[extern_symbol("…")]` on an inherent impl method populates the
/// spec on BOTH the `fn_sigs` entry and the `TypeDef.methods` entry,
/// so Stage-3 method-call rewrite (which reads from `td.methods`) sees
/// the same template as Stage-2 ingest.
#[test]
fn extern_symbol_on_impl_method_populates_both_fn_sigs_and_type_def_methods() {
    let output = check_source(
        r#"
        type Holder { x: i64 }

        impl Holder {
            #[extern_symbol("hew_holder_clone")]
            fn cloned(self) -> Holder {
                self
            }
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "well-formed template must not emit diagnostics, got: {:#?}",
        output.errors
    );
    let sig = output
        .fn_sigs
        .get("Holder::cloned")
        .expect("impl method must be in fn_sigs");
    let spec = sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must populate extern_symbol on the fn_sigs entry");
    assert_eq!(spec.template.raw, "hew_holder_clone");
    assert!(
        spec.template.is_monomorphic(),
        "no {{T}} placeholder → monomorphic"
    );

    let td = output
        .type_defs
        .get("Holder")
        .expect("Holder type must be registered");
    let method_sig = td
        .methods
        .get("cloned")
        .expect("cloned method must be on TypeDef");
    let method_spec = method_sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must mirror the spec onto TypeDef.methods");
    assert_eq!(method_spec.template.raw, "hew_holder_clone");
}

/// A malformed template surfaces as `InvalidExternSymbolTemplate` with
/// the exact deterministic `reason` string from the parser — this is
/// the Stage-2 diagnostic gate referenced by plan §5.5.
#[test]
fn malformed_extern_symbol_template_emits_invalid_template_diagnostic() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_vec_{Q}")]
            fn bad(x: i64);
        }
        "#,
    );
    let invalid: Vec<_> = output
        .errors
        .iter()
        .filter_map(|e| match &e.kind {
            TypeErrorKind::InvalidExternSymbolTemplate { reason } => Some(reason.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(
        invalid.len(),
        1,
        "expected exactly one InvalidExternSymbolTemplate, got errors: {:#?}",
        output.errors
    );
    assert!(
        invalid[0].contains("`{Q}`"),
        "diagnostic reason must name the offending placeholder, got: {:?}",
        invalid[0]
    );

    // Fail-closed: the FnSig must NOT carry a partial / malformed
    // template — Stage 3 should treat this fn as having no rewrite
    // and route through the legacy path (or surface an
    // unresolved-symbol diagnostic later).
    let sig = output
        .fn_sigs
        .get("bad")
        .expect("extern fn must still be registered for downstream resolution");
    assert!(
        sig.extern_symbol.is_none(),
        "rejected template must leave extern_symbol = None (fail-closed)",
    );
}

/// An empty template is rejected with the exact `"empty template"`
/// reason — Stage-5 diagnostic-precision tests pin against this
/// spelling.
#[test]
fn empty_extern_symbol_template_is_rejected_with_empty_reason() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("")]
            fn empty(x: i64);
        }
        "#,
    );
    let reason = output
        .errors
        .iter()
        .find_map(|e| match &e.kind {
            TypeErrorKind::InvalidExternSymbolTemplate { reason } => Some(reason.clone()),
            _ => None,
        })
        .expect("expected InvalidExternSymbolTemplate diagnostic");
    assert_eq!(reason, "empty template");
}

/// Build the fold fixture as a module graph with per-item source
/// attribution: directory module `pkg` assembles `pkg/pkg.hew` plus peer
/// `pkg/aaa.hew`, and `pkg.aaa` is also imported directly. `divergent`
/// selects whether the two files declare two DIFFERENT same-named types
/// with one shared C symbol (must conflict), or the peer file alone
/// declares the symbol (must resolve to ONE contract through both routes).
fn check_peer_assembled_extern(divergent: bool) -> TypeCheckOutput {
    use std::path::PathBuf;
    let pkg_source = if divergent {
        "type Tok {\n    a: i64;\n}\n\nextern \"C\" {\n    fn hew_zz(t: Tok) -> i64;\n}\n"
    } else {
        "pub fn unrelated() -> i64 {\n    0\n}\n"
    };
    let aaa_source = "type Tok {\n    a: i64;\n    b: i64;\n    c: i64;\n}\n\nextern \"C\" {\n    fn hew_zz(t: Tok) -> i64;\n}\n";
    let pkg_file = PathBuf::from("/nonexistent/oracle/pkg/pkg.hew");
    let aaa_file = PathBuf::from("/nonexistent/oracle/pkg/aaa.hew");

    let pkg_items = hew_parser::parse(pkg_source);
    assert!(pkg_items.errors.is_empty(), "parse: {:?}", pkg_items.errors);
    let aaa_items = hew_parser::parse(aaa_source);
    assert!(aaa_items.errors.is_empty(), "parse: {:?}", aaa_items.errors);

    let root_id = ModuleId::root();
    let pkg_id = ModuleId::new(vec!["pkg".to_string()]);
    let aaa_id = ModuleId::new(vec!["pkg".to_string(), "aaa".to_string()]);
    let mut mg = ModuleGraph::new(root_id.clone());

    // Module `pkg` = pkg.hew items + aaa.hew items (peer assembly).
    let mut pkg_module_items = pkg_items.program.items.clone();
    let pkg_item_sources: Vec<PathBuf> =
        std::iter::repeat_n(pkg_file.clone(), pkg_module_items.len())
            .chain(std::iter::repeat_n(
                aaa_file.clone(),
                aaa_items.program.items.len(),
            ))
            .collect();
    pkg_module_items.extend(aaa_items.program.items.clone());
    mg.item_sources.insert("pkg".to_string(), pkg_item_sources);
    mg.add_module(Module {
        id: pkg_id.clone(),
        items: pkg_module_items,
        imports: vec![],
        source_paths: vec![pkg_file.clone(), aaa_file.clone()],
        doc: None,
    })
    .unwrap();

    // Module `pkg.aaa` = aaa.hew alone (direct submodule import).
    mg.item_sources.insert(
        "pkg.aaa".to_string(),
        vec![aaa_file.clone(); aaa_items.program.items.len()],
    );
    mg.add_module(Module {
        id: aaa_id.clone(),
        items: aaa_items.program.items,
        imports: vec![],
        source_paths: vec![aaa_file],
        doc: None,
    })
    .unwrap();

    mg.add_module(Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    })
    .unwrap();
    mg.topo_order = vec![pkg_id, aaa_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&Program {
        items: vec![],
        module_graph: Some(mg),
        module_doc: None,
    })
}

/// Single-owner nominal identity oracle (rc1-F1 stage C): two same-named,
/// layout-divergent type declarations in different peer files of one
/// directory module declare one C symbol — the contract compare must see
/// two DISTINCT nominal identities and reject. The legacy peer-owner fold
/// rewrote the owner spelling off the module graph without checking the
/// short name resolved to the same declaration, and accepted this.
#[test]
fn peer_files_with_divergent_same_named_types_conflict_on_one_symbol() {
    let output = check_peer_assembled_extern(true);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("conflicting declarations")),
        "layout-divergent same-named nominals on one C symbol must conflict; errors: {:#?}",
        output.errors
    );
}

/// Control: ONE declaration (in the peer file) reached through both
/// assembly routes mints one file-backed nominal identity and therefore
/// one contract — no self-conflict.
#[test]
fn one_peer_declaration_through_two_routes_resolves_one_contract() {
    let output = check_peer_assembled_extern(false);
    assert!(
        output.errors.is_empty(),
        "one declaration, two routes, one identity; errors: {:#?}",
        output.errors
    );
}

/// Third-collision oracle: three peer files whose stems all sanitize to one
/// render (`a-b`, `a+b`, `a.b` -> `a_b`), where the SECOND and THIRD declare
/// structurally different `Tok`s against one C symbol. A single-application
/// render disambiguator hands the third file the second's suffixed render,
/// merging two distinct nominals and silently accepting the divergent ABI.
#[test]
fn third_render_collision_keeps_peer_nominals_distinct() {
    use std::path::PathBuf;
    let primary_source = "pub fn unrelated() -> i64 {\n    0\n}\n";
    let inert_source = "pub fn filler() -> i64 {\n    1\n}\n";
    let two_field_source =
        "type Tok {\n    a: i64;\n    b: i64;\n}\n\nextern \"C\" {\n    fn hew_zz(t: Tok) -> i64;\n}\n";
    let three_field_source = "type Tok {\n    a: i64;\n    b: i64;\n    c: i64;\n}\n\nextern \"C\" {\n    fn hew_zz(t: Tok) -> i64;\n}\n";
    let primary_file = PathBuf::from("/nonexistent/tri/pkg/pkg.hew");
    let inert_file = PathBuf::from("/nonexistent/tri/pkg/a-b.hew");
    let two_field_file = PathBuf::from("/nonexistent/tri/pkg/a+b.hew");
    let three_field_file = PathBuf::from("/nonexistent/tri/pkg/a.b.hew");

    let parsed = |source: &str| {
        let out = hew_parser::parse(source);
        assert!(out.errors.is_empty(), "parse: {:?}", out.errors);
        out.program.items
    };
    let primary_items = parsed(primary_source);
    let inert_items = parsed(inert_source);
    let two_field_items = parsed(two_field_source);
    let three_field_items = parsed(three_field_source);

    let root_id = ModuleId::root();
    let pkg_id = ModuleId::new(vec!["pkg".to_string()]);
    let mut mg = ModuleGraph::new(root_id.clone());

    let mut items = primary_items.clone();
    let mut item_sources: Vec<PathBuf> =
        std::iter::repeat_n(primary_file.clone(), primary_items.len()).collect();
    for (file, file_items) in [
        (&inert_file, &inert_items),
        (&two_field_file, &two_field_items),
        (&three_field_file, &three_field_items),
    ] {
        item_sources.extend(std::iter::repeat_n(file.clone(), file_items.len()));
        items.extend(file_items.iter().cloned());
    }
    mg.item_sources.insert("pkg".to_string(), item_sources);
    mg.add_module(Module {
        id: pkg_id.clone(),
        items,
        imports: vec![],
        source_paths: vec![primary_file, inert_file, two_field_file, three_field_file],
        doc: None,
    })
    .unwrap();
    mg.add_module(Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    })
    .unwrap();
    mg.topo_order = vec![pkg_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        items: vec![],
        module_graph: Some(mg),
        module_doc: None,
    });
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("conflicting declarations")),
        "divergent same-named nominals in the second and third colliding \
         files must conflict on one C symbol; errors: {:#?}",
        output.errors
    );
}

/// Which import graph the re-declaring module sees in
/// [`check_import_lexical_extern`].
enum ImportLexicalShape {
    /// `nt` imports `sm` alone: a bare nominal in its extern re-declaration
    /// resolves through that import to `sm.Tok` and matches the contract.
    SingleImport,
    /// `nt` imports `sm` AND `om`, both declaring `Tok`: the bare spelling is
    /// ambiguous, resolution must refuse, and the contract compare conflicts.
    AmbiguousImports,
    /// `nt` imports only `om`, whose `Tok` is a DIFFERENT type from the
    /// `sm.Tok` that established the contract: resolution succeeds to
    /// `om.Tok` and the compare must still conflict (no false merge).
    ForeignDivergentImport,
    /// `nt` imports `sm::{ Tok as ForeignTok }`: the import binds ONLY the
    /// alias, so bare `Tok` is unbound in `nt` — resolution must refuse and
    /// the contract compare must conflict, never adopt through the alias.
    AliasedNamedImport,
    /// `nt` imports `sm::{ Tok }`: the named import binds `Tok` bare, so the
    /// bare spelling resolves to `sm.Tok` and adopts the contract.
    BareNamedImport,
}

/// Import-lexical extern nominal fixture: module `sm` declares `Tok` and
/// establishes `hew_zz`'s contract; module `nt` re-declares `hew_zz`
/// spelling the parameter type BARE (`Tok`), declaring no `Tok` of its own.
/// The bare name's meaning is decided by `nt`'s import set per `shape`.
fn check_import_lexical_extern(shape: &ImportLexicalShape) -> TypeCheckOutput {
    use std::path::PathBuf;
    let sm_source =
        "pub type Tok {\n    a: i64;\n}\n\nextern \"C\" {\n    fn hew_zz(t: Tok) -> i64;\n}\n";
    let om_source = "pub type Tok {\n    a: i64;\n    b: i64;\n    c: i64;\n}\n";
    let nt_source = "extern \"C\" {\n    fn hew_zz(t: Tok) -> i64;\n}\n";
    let sm_file = PathBuf::from("/nonexistent/implex/sm.hew");
    let om_file = PathBuf::from("/nonexistent/implex/om.hew");
    let nt_file = PathBuf::from("/nonexistent/implex/nt.hew");

    let parsed = |source: &str| {
        let out = hew_parser::parse(source);
        assert!(out.errors.is_empty(), "parse: {:?}", out.errors);
        out.program.items
    };
    let sm_items = parsed(sm_source);
    let om_items = parsed(om_source);
    let nt_items = parsed(nt_source);

    let root_id = ModuleId::root();
    let sm_id = ModuleId::new(vec!["sm".to_string()]);
    let om_id = ModuleId::new(vec!["om".to_string()]);
    let nt_id = ModuleId::new(vec!["nt".to_string()]);
    let mut mg = ModuleGraph::new(root_id.clone());

    let named = |source: &str, alias: Option<&str>| {
        Some(ImportSpec::Names(vec![ImportName {
            name: source.to_string(),
            alias: alias.map(str::to_string),
        }]))
    };
    let nt_imports: Vec<(ModuleId, Option<ImportSpec>)> = match shape {
        ImportLexicalShape::SingleImport => vec![(sm_id.clone(), None)],
        ImportLexicalShape::AmbiguousImports => {
            vec![(sm_id.clone(), None), (om_id.clone(), None)]
        }
        ImportLexicalShape::ForeignDivergentImport => vec![(om_id.clone(), None)],
        ImportLexicalShape::AliasedNamedImport => {
            vec![(sm_id.clone(), named("Tok", Some("ForeignTok")))]
        }
        ImportLexicalShape::BareNamedImport => vec![(sm_id.clone(), named("Tok", None))],
    };

    let add_module = |mg: &mut ModuleGraph,
                      id: &ModuleId,
                      items: &Vec<Spanned<Item>>,
                      file: &PathBuf,
                      imports: Vec<(ModuleId, Option<ImportSpec>)>| {
        mg.item_sources
            .insert(id.path.join("."), vec![file.clone(); items.len()]);
        mg.add_module(Module {
            id: id.clone(),
            items: items.clone(),
            imports: imports
                .into_iter()
                .map(|(target, spec)| hew_parser::module::ModuleImport {
                    target,
                    spec,
                    span: 0..0,
                })
                .collect(),
            source_paths: vec![file.clone()],
            doc: None,
        })
        .unwrap();
    };
    add_module(&mut mg, &sm_id, &sm_items, &sm_file, vec![]);
    add_module(&mut mg, &om_id, &om_items, &om_file, vec![]);
    add_module(&mut mg, &nt_id, &nt_items, &nt_file, nt_imports);

    mg.add_module(Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    })
    .unwrap();
    mg.topo_order = vec![sm_id, om_id, nt_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&Program {
        items: vec![],
        module_graph: Some(mg),
        module_doc: None,
    })
}

/// Import-lexical resolution (rc1-F1 stage C): a bare nominal in an extern
/// re-declaration that the declaring file does not declare itself resolves
/// through the declaring module's OWN import set — `nt` imports `sm`, so its
/// bare `Tok` IS `sm.Tok` and the re-declaration adopts the established
/// contract instead of conflicting on a spelling difference.
#[test]
fn bare_extern_nominal_resolves_through_the_declaring_files_import() {
    let output = check_import_lexical_extern(&ImportLexicalShape::SingleImport);
    assert!(
        output.errors.is_empty(),
        "one declaration behind one import must resolve one identity; errors: {:#?}",
        output.errors
    );
}

/// Fail-closed ambiguity: two imported modules both declare `Tok`, so the
/// bare spelling has no single import-lexical meaning. Resolution must
/// refuse (never pick a winner) and the contract compare must conflict.
#[test]
fn ambiguous_imported_bare_extern_nominal_stays_unresolved_and_conflicts() {
    let output = check_import_lexical_extern(&ImportLexicalShape::AmbiguousImports);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("conflicting declarations")),
        "an ambiguous bare nominal must not silently adopt either owner; errors: {:#?}",
        output.errors
    );
}

/// No false merge on the extern ABI axis: the bare `Tok` resolves cleanly
/// through `nt`'s only import — but to `om.Tok`, a genuinely DIFFERENT type
/// from the `sm.Tok` that established the contract. Same leaf, different
/// identity: still a conflict.
#[test]
fn import_resolved_bare_nominal_with_different_identity_still_conflicts() {
    let output = check_import_lexical_extern(&ImportLexicalShape::ForeignDivergentImport);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("conflicting declarations")),
        "same-leaf different-owner nominals on one C symbol must conflict; errors: {:#?}",
        output.errors
    );
}

/// An aliased item import binds ONLY its alias: `import sm::{ Tok as
/// ForeignTok }` does not make bare `Tok` mean `sm.Tok` in `nt`, so the
/// re-declaration spelling bare `Tok` must CONFLICT with the established
/// `sm.Tok` contract, never merge through the alias's target.
#[test]
fn aliased_item_import_does_not_bind_the_bare_extern_nominal() {
    let output = check_import_lexical_extern(&ImportLexicalShape::AliasedNamedImport);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("conflicting declarations")),
        "bare `Tok` is unbound under `Tok as ForeignTok`; errors: {:#?}",
        output.errors
    );
}

/// Positive control for the binding rule: an UNALIASED item import
/// (`import sm::{ Tok }`) binds `Tok` bare, so the bare spelling resolves
/// to `sm.Tok` and the re-declaration adopts the established contract.
#[test]
fn unaliased_item_import_binds_the_bare_extern_nominal() {
    let output = check_import_lexical_extern(&ImportLexicalShape::BareNamedImport);
    assert!(
        output.errors.is_empty(),
        "`import sm::{{ Tok }}` binds `Tok` bare; errors: {:#?}",
        output.errors
    );
}
