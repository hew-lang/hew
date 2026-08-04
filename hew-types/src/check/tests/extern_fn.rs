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
    assert!(
        output.errors.iter().any(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::ConflictingExternDeclaration { symbol_name }
                    if symbol_name == "hew_duplicate_mode"
            )
        }),
        "errors: {:#?}",
        output.errors
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
