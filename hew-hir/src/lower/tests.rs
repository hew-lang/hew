use super::*;
use hew_parser::ast::Ident;
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn assert_ordered_aggregate_groups(main: &HirFn) {
    let groups: Vec<_> = main
        .body
        .statements
        .iter()
        .filter_map(|stmt| match &stmt.kind {
            HirStmtKind::Destructure { value, fields } => Some((value, fields)),
            _ => None,
        })
        .collect();
    assert_eq!(groups.len(), 4);
    assert!(matches!(
        groups[0].1.as_slice(),
        [
            HirDestructureField {
                nested: false,
                selector: HirDestructureSelector::Tuple(0),
                binding: Some(HirBinding { name: label, ty: ResolvedTy::String, .. }),
            },
            HirDestructureField {
                nested: false,
                selector: HirDestructureSelector::Tuple(1),
                binding: Some(HirBinding { name: bytes, ty: ResolvedTy::Bytes, .. }),
            },
        ] if label == "label" && bytes == "bytes"
    ));
    assert!(matches!(
        groups[1].1.as_slice(),
        [
            HirDestructureField {
                nested: false,
                selector: HirDestructureSelector::Record(x),
                binding: Some(HirBinding { name: x_binding, ty: ResolvedTy::String, .. }),
            },
            HirDestructureField {
                nested: false,
                selector: HirDestructureSelector::Record(payload),
                binding: Some(HirBinding { name: payload_binding, ty: ResolvedTy::Bytes, .. }),
            },
        ] if x == "x" && x_binding == "x" && payload == "payload" && payload_binding == "payload"
    ));
    assert!(matches!(
        groups[2].1.as_slice(),
        [
            HirDestructureField {
                nested: true,
                selector: HirDestructureSelector::Tuple(0),
                binding: Some(HirBinding {
                    ty: ResolvedTy::Tuple(_),
                    ..
                }),
            },
            // `_` names nothing, so the field carries no binding.
            HirDestructureField {
                nested: false,
                selector: HirDestructureSelector::Tuple(1),
                binding: None,
            },
        ]
    ));
    assert!(matches!(
        groups[3].1.as_slice(),
        [
            HirDestructureField {
                nested: false,
                selector: HirDestructureSelector::Tuple(0),
                binding: Some(HirBinding { name: label, ty: ResolvedTy::String, .. }),
            },
            HirDestructureField {
                nested: false,
                selector: HirDestructureSelector::Tuple(1),
                binding: Some(HirBinding { name: payload, ty: ResolvedTy::Bytes, .. }),
            },
        ] if label == "nested_label" && payload == "nested_payload"
    ));
    assert!(groups.iter().all(|(value, _)| matches!(
        value.kind,
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(_),
            ..
        }
    )));
}

#[test]
fn irrefutable_aggregate_patterns_keep_one_ordered_typed_binding_group() {
    let parsed = hew_parser::parse(
        r#"
type Point { x: string, payload: bytes }

fn main() {
    let pair = ("left", b"right");
    let (label, bytes) = pair;
    let point = Point { x: "x", payload: b"p" };
    let { x, payload } = point;
    let nested = (("nested", b"bytes"), 1);
    let ((nested_label, nested_payload), _) = nested;
}
"#,
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut type_checker = Checker::new(ModuleRegistry::new(vec![]));
    let type_output = type_checker.check_program(&parsed.program);
    assert!(
        type_output.errors.is_empty(),
        "type errors: {:#?}",
        type_output.errors
    );
    let lowered = lower_program(
        &parsed.program,
        &type_output,
        &ResolutionCtx,
        TargetArch::host(),
    )
    .into_result()
    .expect("typed aggregate patterns must lower to HIR");
    let main = lowered
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function");
    assert_ordered_aggregate_groups(main);
}

#[test]
fn builtin_receiver_signature_mismatch_is_a_diagnostic_not_a_panic() {
    let parsed = hew_parser::parse(
        r"
trait Sample {
    fn value(self) -> i64;
}

type Broken {}

impl Sample for Broken {
    fn value(self) -> bool { true }
}
",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let diagnostic =
        *check_builtin_callable_impl_program(&parsed.program, &hew_types::DefTable::new())
            .expect_err("the invalid injected impl must fail closed");
    let HirDiagnosticKind::CheckerBoundaryViolation { name, reason } = diagnostic.kind else {
        panic!("expected checker-boundary diagnostic, got {diagnostic:?}");
    };
    assert_eq!(name, "std/builtins.hew callable impls");
    assert!(
        reason.contains("returns `bool`") && reason.contains("requires `i64`"),
        "diagnostic must preserve the checker mismatch: {reason}"
    );
    assert!(
        diagnostic.note.contains(&reason),
        "the displayed diagnostic must include its cause"
    );
}

#[test]
fn trait_method_identity_prefers_local_and_imported_same_leaf_traits_over_prelude_items() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.current_module_name = Some("app".to_string());
    for (lang_item, trait_name, method_name, prelude_owner) in [
        (
            hew_types::LangItem::IteratorNext,
            "Iterator",
            "next",
            "std.builtins.Iterator",
        ),
        (
            hew_types::LangItem::DisplayFmt,
            "Display",
            "fmt",
            "std.builtins.Display",
        ),
    ] {
        let prelude_trait = hew_types::DefId::for_test(prelude_owner);
        let prelude_method = hew_types::DefId::for_test(format!("{prelude_owner}::{method_name}"));
        ctx.lang_items.insert(
            lang_item.key(),
            hew_types::LangItemBinding {
                trait_name: trait_name.to_string(),
                trait_id: prelude_trait,
                method_name: Some(method_name.to_string()),
                method_id: Some(prelude_method),
            },
        );

        let local_trait = hew_types::DefId::for_test(format!("app.{trait_name}"));
        let local_method = hew_types::DefId::for_test(format!("app.{trait_name}::{method_name}"));
        ctx.trait_method_ids.insert(
            format!("app.{trait_name}::{method_name}"),
            (local_trait, local_method),
        );
        assert_eq!(
            ctx.trait_method_identity(trait_name, method_name),
            Some((local_trait, local_method)),
            "a local same-leaf {trait_name} must not be replaced by the prelude lang item"
        );
        ctx.trait_method_ids
            .remove(&format!("app.{trait_name}::{method_name}"));

        let imported_trait = hew_types::DefId::for_test(format!("vendor.{trait_name}"));
        let imported_method =
            hew_types::DefId::for_test(format!("vendor.{trait_name}::{method_name}"));
        ctx.trait_method_ids_by_binding.insert(
            (
                Some("app".to_string()),
                0,
                trait_name.to_string(),
                method_name.to_string(),
            ),
            (imported_trait, imported_method),
        );
        assert_eq!(
            ctx.trait_method_identity(trait_name, method_name),
            Some((imported_trait, imported_method)),
            "an imported same-leaf {trait_name} must not be replaced by the prelude lang item"
        );
        ctx.trait_method_ids_by_binding.remove(&(
            Some("app".to_string()),
            0,
            trait_name.to_string(),
            method_name.to_string(),
        ));
    }
}

#[test]
fn conflicting_impl_body_plan_is_a_checker_boundary_diagnostic_not_a_panic() {
    // The checker normally assigns distinct declarations. Corrupt just
    // that handoff to model an upstream collision: the HIR plan must fail
    // closed in release builds rather than assert while lowering either
    // otherwise-valid body.
    let parsed = hew_parser::parse(
        r"
type Alpha { value: i64 }
type Beta { value: i64 }

impl Alpha {
    fn run(self) -> i64 { self.value }
}

impl Beta {
    fn run(self) -> i64 { self.value }
}

fn main() {}
",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut type_output = checker.check_program(&parsed.program);
    assert!(
        type_output.errors.is_empty(),
        "type errors: {:#?}",
        type_output.errors
    );
    let alpha = type_output
        .impl_method_declaration_ids
        .get("Alpha::run")
        .copied()
        .expect("checker must publish Alpha's implementation declaration");
    type_output
        .impl_method_declaration_ids
        .insert("Beta::run".to_string(), alpha);

    let lowered = lower_program(
        &parsed.program,
        &type_output,
        &ResolutionCtx,
        TargetArch::host(),
    );
    assert!(
        lowered.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                HirDiagnosticKind::CheckerBoundaryViolation { reason, .. }
                    if reason.contains("conflicting pre-lowering symbols")
            )
        }),
        "duplicate impl-body declarations must report a structured boundary violation: {:#?}",
        lowered.diagnostics
    );
    assert!(
        lowered.into_result().is_err(),
        "a conflicting pre-lowering body plan must remain fatal"
    );
}

#[test]
fn two_import_paths_select_one_declaration_owned_impl_body_symbol() {
    let parsed = hew_parser::parse(
        r"
type Widget { value: i64 }

impl Widget {
    fn run(self) -> i64 { self.value }
}
",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let impl_decl = parsed
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Impl(decl) => Some(decl),
            _ => None,
        })
        .expect("fixture impl");
    let declaration = hew_types::DefId::for_test(
        "fixture.owner.Widget::<impl inherent for fixture.owner.Widget>::run",
    );

    for paths in [
        ["Widget", "fixture.owner.Widget"],
        ["fixture.owner.Widget", "Widget"],
    ] {
        let mut output = TypeCheckOutput {
            defs: hew_types::DefTable::fixture(),
            ..TypeCheckOutput::default()
        };
        output
            .impl_method_declaration_ids
            .insert("Widget::run".to_string(), declaration);
        output
            .impl_method_declaration_ids
            .insert("fixture.owner.Widget::run".to_string(), declaration);
        let mut ctx = LowerCtx::new(&output, MONOMORPHISATION_REGISTRY_CAP, TargetArch::host());
        for path in paths {
            plan_impl_block_symbols(&mut ctx, impl_decl, path, &HashSet::new());
        }

        assert!(
            ctx.diagnostics.is_empty(),
            "two paths to one declaration are aliases, not duplicate bodies: {:#?}",
            ctx.diagnostics
        );
        assert_eq!(
            ctx.impl_body_plan
                .symbols
                .get(&declaration)
                .map(String::as_str),
            Some("fixture.owner.Widget::run"),
            "declaration identity must choose the same emitted symbol in either path order"
        );
    }
}

#[test]
fn impl_body_projection_never_retries_through_a_same_leaf_symbol() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    let left = hew_types::DefId::for_test(
        "left.render.Result::<impl inherent for left.render.Result>::echo",
    );
    let right = hew_types::DefId::for_test(
        "right.render.Result::<impl inherent for right.render.Result>::echo",
    );
    ctx.impl_method_body_symbols
        .insert(left, "left.render.Result::echo".to_string());
    ctx.fn_registry.insert(
        "right.render.Result::echo".to_string(),
        FnEntry {
            id: ItemId(1),
            return_ty: ResolvedTy::I64,
            param_tys: Vec::new(),
            linkage: None,
            type_params: Vec::new(),
            builtin_family: None,
        },
    );

    assert_eq!(
        ctx.registered_impl_method_symbol(left).as_deref(),
        Some("left.render.Result::echo")
    );
    assert_eq!(
        ctx.registered_impl_method_symbol(right),
        None,
        "a real same-leaf registry entry cannot substitute for the selected declaration body"
    );
}

#[test]
fn imported_opaque_identity_precedes_short_builtin_fallback() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.type_declarations.extend(
        ["foo.Stream", "foo.Connection", "net.Connection"]
            .into_iter()
            .map(|name| {
                (
                    name.to_string(),
                    hew_types::value_class::DeclaredType {
                        is_opaque: true,
                        ..Default::default()
                    },
                )
            }),
    );

    for qualified in ["foo.Stream", "foo.Connection", "net.Connection"] {
        assert_eq!(
            ctx.resolve_named_type_ref(qualified, Vec::new()),
            ResolvedTy::named_opaque_path(&ctx.defs, qualified, Vec::new()),
            "qualified opaque identity must be preserved exactly"
        );
    }

    ctx.canonical_std_source_type_identities.extend([
        "std.stream.Stream".to_string(),
        "std.stream.Sink".to_string(),
        "std.link_monitor.MonitorRef".to_string(),
    ]);
    for (qualified, builtin) in [
        ("std.stream.Stream", BuiltinType::Stream),
        ("std.stream.Sink", BuiltinType::Sink),
        ("std.link_monitor.MonitorRef", BuiltinType::MonitorRef),
    ] {
        assert_eq!(
                ctx.resolve_named_type_ref(qualified, Vec::new()),
                ResolvedTy::named_builtin(builtin, Vec::new()),
                "an exact canonical std carrier `{qualified}` must retain declaration and builtin identity"
            );
    }

    for qualified in ["stream.Stream", "stream.Sink", "link_monitor.MonitorRef"] {
        assert_eq!(
                ctx.resolve_named_type_ref(qualified, Vec::new()),
                ResolvedTy::named_path(&ctx.defs, qualified, Vec::new()),
                "a user module with the std leaf spelling `{qualified}` must not inherit builtin ABI identity"
            );
    }

    ctx.import_type_name_aliases
        .insert((None, 0, "Stream".to_string()), "foo.Stream".to_string());
    assert_eq!(
        ctx.resolve_named_type_ref("Stream", Vec::new()),
        ResolvedTy::named_opaque_path(&ctx.defs, "foo.Stream", Vec::new()),
        "an unrenamed named import must resolve through its published source identity"
    );

    ctx.import_type_name_aliases.clear();
    ctx.type_declarations.insert(
        "Stream".to_string(),
        hew_types::value_class::DeclaredType {
            is_opaque: true,
            ..Default::default()
        },
    );
    assert_eq!(
        ctx.resolve_named_type_ref("Stream", Vec::new()),
        ResolvedTy::named_opaque_path(&ctx.defs, "Stream", Vec::new()),
        "a flattened file-import declaration must outrank the bare builtin"
    );

    ctx.type_declarations.remove("Stream");
    ctx.current_module_name = Some("std.stream".to_string());
    assert_eq!(
        ctx.qualify_current_module_record_ty(ResolvedTy::named_path(
            &ctx.defs,
            "Stream",
            Vec::new(),
        )),
        ResolvedTy::named_builtin(BuiltinType::Stream, Vec::new()),
        "a checker-authored bare std handle must recover its exact builtin identity"
    );

    ctx.current_module_name = Some("std.net.http".to_string());
    ctx.type_declarations.insert(
        "http.ResponseHandle".to_string(),
        hew_types::value_class::DeclaredType {
            is_opaque: true,
            ..Default::default()
        },
    );
    assert_eq!(
        ctx.qualify_current_module_record_ty(ResolvedTy::named_path(
            &ctx.defs,
            "http.ResponseHandle",
            Vec::new(),
        )),
        ResolvedTy::named_opaque_path(&ctx.defs, "http.ResponseHandle", Vec::new()),
        "a checker-authored qualified opaque identity must recover its declaration discriminator"
    );
}

#[test]
fn checker_stream_compatibility_spelling_requires_exact_std_provenance() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.canonical_std_source_type_identities
        .insert("std.stream.Sink".to_string());
    ctx.current_module_name = Some("std.net.http".to_string());
    let checker_result = ResolvedTy::named_builtin(
        BuiltinType::Result,
        vec![
            ResolvedTy::named_for_test("stream.Sink", vec![ResolvedTy::String]),
            ResolvedTy::String,
        ],
    );
    assert_eq!(
        ctx.qualify_current_module_record_ty(checker_result),
        ResolvedTy::named_builtin(
            BuiltinType::Result,
            vec![
                ResolvedTy::named_builtin(BuiltinType::Sink, vec![ResolvedTy::String],),
                ResolvedTy::String,
            ]
        )
    );

    ctx.current_module_name = Some("std.stream".to_string());
    ctx.canonical_std_source_type_identities
        .insert("std.stream.Stream".to_string());
    let exact_nested = ResolvedTy::named_builtin(
        BuiltinType::Result,
        vec![
            ResolvedTy::Tuple(vec![
                ResolvedTy::Named {
                    args: vec![ResolvedTy::String],
                    head: hew_types::TypeHead::Builtin(BuiltinType::Sink),
                    is_opaque: false,
                },
                ResolvedTy::Named {
                    args: vec![ResolvedTy::String],
                    head: hew_types::TypeHead::Builtin(BuiltinType::Stream),
                    is_opaque: false,
                },
            ]),
            ResolvedTy::String,
        ],
    );
    assert_eq!(
        ctx.qualify_current_module_record_ty(exact_nested),
        ResolvedTy::named_builtin(
            BuiltinType::Result,
            vec![
                ResolvedTy::Tuple(vec![
                    ResolvedTy::named_builtin(BuiltinType::Sink, vec![ResolvedTy::String],),
                    ResolvedTy::named_builtin(BuiltinType::Stream, vec![ResolvedTy::String],),
                ]),
                ResolvedTy::String,
            ]
        ),
        "nested checker facts must normalize to the function signature's carrier ABI"
    );

    ctx.current_module_name = Some("acme.http".to_string());
    assert_eq!(
        ctx.qualify_current_module_record_ty(ResolvedTy::named_for_test(
            "stream.Sink",
            vec![ResolvedTy::String],
        )),
        ResolvedTy::named_for_test("stream.Sink", vec![ResolvedTy::String]),
        "a user `stream.Sink` collision must not inherit std carrier identity"
    );
    assert_eq!(
        ctx.qualify_current_module_record_ty(ResolvedTy::Named {
            args: vec![ResolvedTy::String],
            head: hew_types::TypeHead::Builtin(BuiltinType::Sink),
            is_opaque: false
        }),
        ResolvedTy::named_for_test("stream.Sink", vec![ResolvedTy::String]),
        "even a stale checker builtin bit cannot grant a user same-leaf carrier ABI"
    );
}

#[test]
fn canonical_std_carriers_and_user_package_collisions_keep_distinct_identities() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    let args = vec![ResolvedTy::I64];

    ctx.current_module_name = Some("std.stream".to_string());
    ctx.canonical_std_source_type_identities
        .insert("std.stream.Sink".to_string());
    assert_eq!(
        ctx.resolve_named_type_ref("Sink", args.clone()),
        ResolvedTy::named_builtin(BuiltinType::Sink, args.clone()),
        "the canonical std.stream declaration must recover compiler carrier identity"
    );

    ctx.current_module_name = Some("acme.stream".to_string());
    ctx.source_type_identities
        .insert("acme.stream.Sink".to_string());
    assert_eq!(
        ctx.resolve_named_type_ref("Sink", args.clone()),
        ResolvedTy::named_path(&ctx.defs, "acme.stream.Sink", args.clone()),
        "an acme package's authored Sink<T> must remain a user nominal"
    );
    assert_eq!(
        ctx.resolve_named_type_ref("acme.stream.Sink", args.clone()),
        ResolvedTy::named_path(&ctx.defs, "acme.stream.Sink", args.clone()),
        "a qualified import of the acme carrier collision must remain user-owned"
    );

    // A path spelling is not provenance. A user package may be named
    // `std.stream`; it acquires the carrier ABI only when its concrete
    // source was harvested as a canonical stdlib source above.
    let mut untrusted_std = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    untrusted_std.current_module_name = Some("std.stream".to_string());
    untrusted_std
        .source_type_identities
        .insert("std.stream.Sink".to_string());
    assert_eq!(
        untrusted_std.resolve_named_type_ref("Sink", args.clone()),
        ResolvedTy::named_path(&ctx.defs, "std.stream.Sink", args.clone()),
        "a user module named std.stream is not canonical stdlib provenance"
    );

    ctx.current_module_name = None;
    ctx.canonical_std_source_type_identities
        .insert("std.failure.CrashInfo".to_string());
    ctx.import_type_name_aliases.insert(
        (None, 0, "CrashInfo".to_string()),
        "std.failure.CrashInfo".to_string(),
    );
    assert_eq!(
        ctx.resolve_named_type_ref("CrashInfo", Vec::new()),
        ResolvedTy::named_builtin(BuiltinType::CrashInfo, Vec::new()),
        "an imported std lifecycle payload must not be stolen by the global record registry"
    );
}

#[test]
fn depth_two_source_owners_qualify_same_leaf_types_without_leaf_fallback() {
    let mut std_ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    std_ctx.current_module_name = Some("std.net".to_string());
    std_ctx
        .source_type_identities
        .insert("std.net.Connection".to_string());
    std_ctx.type_declarations.insert(
        "std.net.Connection".to_string(),
        hew_types::value_class::DeclaredType {
            is_opaque: true,
            ..Default::default()
        },
    );

    assert_eq!(
        std_ctx.resolve_named_type_ref("Connection", Vec::new()),
        ResolvedTy::named_opaque_path(&std_ctx.defs, "std.net.Connection", Vec::new()),
        "a bare std.net declaration must retain its full source owner"
    );

    let mut root_ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    root_ctx.type_declarations.insert(
        "std.net.Connection".to_string(),
        hew_types::value_class::DeclaredType {
            is_opaque: true,
            ..Default::default()
        },
    );
    assert_eq!(
        root_ctx.qualify_current_module_record_ty(ResolvedTy::named_path(
            &std_ctx.defs,
            "std.net.Connection",
            Vec::new(),
        )),
        ResolvedTy::named_opaque_path(&std_ctx.defs, "std.net.Connection", Vec::new()),
        "checker-authored closure capture facts must recover an imported opaque identity"
    );
    assert_eq!(
        root_ctx.qualify_current_module_record_ty(ResolvedTy::named_path(
            &std_ctx.defs,
            "acme.net.Connection",
            Vec::new(),
        )),
        ResolvedTy::named_path(&std_ctx.defs, "acme.net.Connection", Vec::new()),
        "a same-leaf user closure capture must not inherit std.net opacity"
    );
    assert_eq!(
        std_ctx.qualify_current_module_record_ty(ResolvedTy::named_path(
            &std_ctx.defs,
            "Connection",
            Vec::new(),
        )),
        ResolvedTy::named_opaque_path(&std_ctx.defs, "std.net.Connection", Vec::new()),
        "checker facts that lose the opaque bit must recover std.net, never net"
    );

    let mut user_ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    user_ctx.current_module_name = Some("acme.net".to_string());
    user_ctx
        .source_type_identities
        .insert("acme.net.Connection".to_string());

    for resolved in [
        user_ctx.resolve_named_type_ref("Connection", Vec::new()),
        user_ctx.qualify_current_module_record_ty(ResolvedTy::named_path(
            &std_ctx.defs,
            "Connection",
            Vec::new(),
        )),
    ] {
        assert_eq!(
            resolved,
            ResolvedTy::named_path(&std_ctx.defs, "acme.net.Connection", Vec::new()),
            "a user depth-two owner sharing std.net's leaf must stay distinct"
        );
    }
}

#[test]
fn checker_import_binding_nominal_facts_use_the_declaring_std_owner() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.current_module_name = Some("std.net.tls".to_string());
    ctx.module_import_bindings.insert(
        (Some("std.net.tls".to_string()), 0, "net".to_string()),
        "std.net".to_string(),
    );
    ctx.canonical_std_source_type_identities
        .insert("std.net.NetError".to_string());

    assert_eq!(
            ctx.qualify_current_module_record_ty(ResolvedTy::named_path(&ctx.defs, "net.NetError", Vec::new(),
            )),
            ResolvedTy::named_path(&ctx.defs, "std.net.NetError", Vec::new()),
            "a checker-produced lexical module binding must agree with the exact std declaration identity"
        );
}

#[test]
fn imported_const_key_uses_the_checker_module_owner() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.module_import_bindings.insert(
        (None, 0, "codec".to_string()),
        "std.net.http.codec".to_string(),
    );

    assert_eq!(
        ctx.imported_module_member_key("codec", "MAX_READS"),
        "std.net.http.codec.MAX_READS"
    );
}

#[test]
fn checker_remote_pid_fact_requires_discriminator_and_preserves_source_names() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    let args = vec![ResolvedTy::user_for_test("Echo", Vec::new())];
    assert_eq!(
        ctx.qualify_current_module_record_ty(ResolvedTy::user_for_test("RemotePid", args.clone(),)),
        ResolvedTy::user_for_test("RemotePid", args.clone()),
        "a bare spelling cannot manufacture the compiler actor-carrier discriminator"
    );
    assert_eq!(
        ctx.qualify_current_module_record_ty(ResolvedTy::named_builtin(
            BuiltinType::RemotePid,
            args.clone()
        )),
        ResolvedTy::named_builtin(BuiltinType::RemotePid, args.clone()),
        "a checker-authored compiler actor carrier retains its value class"
    );

    ctx.root_visible_source_type_short_names
        .insert("RemotePid".to_string());
    assert_eq!(
        ctx.qualify_current_module_record_ty(ResolvedTy::user_for_test("RemotePid", args.clone(),)),
        ResolvedTy::user_for_test("RemotePid", args.clone()),
        "a root source declaration wins over the compiler carrier spelling"
    );
}

#[test]
fn imported_crash_notification_keeps_source_identity_after_record_registration() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.canonical_std_source_type_identities
        .insert("failure.CrashNotification".to_string());
    ctx.record_registry.insert(
        "CrashNotification".to_string(),
        RecordEntry {
            id: ItemId(1),
            type_params: Vec::new(),
            fields: Vec::new(),
        },
    );
    ctx.import_type_name_aliases.insert(
        (None, 0, "CrashNotification".to_string()),
        "failure.CrashNotification".to_string(),
    );

    assert_eq!(
        ctx.resolve_named_type_ref("CrashNotification", Vec::new()),
        ResolvedTy::named_path(&ctx.defs, "failure.CrashNotification", Vec::new()),
        "a published lifecycle import must retain its owner-qualified source identity"
    );

    // Without the checker-published import binding, a globally registered
    // std declaration is layout metadata only and must not grant the bare
    // spelling lifecycle authority.
    ctx.import_type_name_aliases.clear();
    assert_eq!(
        ctx.resolve_named_type_ref("CrashNotification", Vec::new()),
        ResolvedTy::named_path(&ctx.defs, "CrashNotification", Vec::new()),
        "a global std record must not make its bare lifecycle name implicit"
    );

    // An authored root declaration likewise remains an ordinary nominal.
    ctx.root_visible_source_type_short_names
        .insert("CrashNotification".to_string());
    assert_eq!(
        ctx.resolve_named_type_ref("CrashNotification", Vec::new()),
        ResolvedTy::named_path(&ctx.defs, "CrashNotification", Vec::new()),
        "a user-authored same-spelling record must not acquire the lifecycle ABI"
    );
}

#[test]
fn checker_result_type_uses_flat_file_import_identity() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.file_import_root_type_aliases
        .insert("Box".to_string(), "support.file_render.Box".to_string());

    assert_eq!(
        ctx.qualify_current_module_record_ty(ResolvedTy::named_path(&ctx.defs, "Box", Vec::new())),
        ResolvedTy::named_path(&ctx.defs, "support.file_render.Box", Vec::new())
    );
}

#[test]
fn checker_proven_whole_module_lifecycle_alias_canonicalizes_in_hir() {
    let tc_output = TypeCheckOutput {
        import_type_name_aliases: HashMap::from([(
            (None, 0, "f.CrashNotification".to_string()),
            "failure.CrashNotification".to_string(),
        )]),
        ..TypeCheckOutput::default()
    };
    let ctx = LowerCtx::new(
        &tc_output,
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );

    assert_eq!(
        ctx.resolve_named_type_ref("f.CrashNotification", Vec::new()),
        ResolvedTy::named_path(&ctx.defs, "failure.CrashNotification", Vec::new()),
        "HIR must consume the checker's exact qualified lifecycle identity"
    );

    let unproven = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    assert_eq!(
        unproven.resolve_named_type_ref("f.CrashNotification", Vec::new()),
        ResolvedTy::named_path(&ctx.defs, "f.CrashNotification", Vec::new()),
        "module spelling without a checker fact must remain an ordinary nominal"
    );
}

fn named_type_ref(name: &str, args: Vec<Spanned<TypeExpr>>) -> Spanned<TypeExpr> {
    (
        TypeExpr::Named {
            path: hew_parser::ast::Path::single(hew_parser::ast::Ident::new(name), 0..0),
            type_args: (!args.is_empty()).then_some(args),
        },
        0..0,
    )
}

#[test]
fn source_identity_precedes_task_unit_and_cancellation_early_arms() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.root_visible_source_type_short_names.extend([
        "Task".to_string(),
        "Unit".to_string(),
        "CancellationToken".to_string(),
    ]);
    ctx.type_declarations.insert(
        "CancellationToken".to_string(),
        hew_types::value_class::DeclaredType {
            is_opaque: true,
            ..Default::default()
        },
    );

    let i64_arg = || vec![named_type_ref("i64", Vec::new())];
    assert_eq!(
        ctx.lower_type(&named_type_ref("Task", i64_arg())),
        ResolvedTy::named_path(&ctx.defs, "Task", vec![ResolvedTy::I64])
    );
    assert_eq!(
        ctx.lower_type(&named_type_ref("Unit", i64_arg())),
        ResolvedTy::named_path(&ctx.defs, "Unit", vec![ResolvedTy::I64])
    );
    assert_eq!(
        ctx.lower_type(&named_type_ref("CancellationToken", Vec::new())),
        ResolvedTy::named_opaque_path(&ctx.defs, "CancellationToken", Vec::new())
    );
    assert!(
        !ctx.diagnostics
            .iter()
            .any(|diagnostic| matches!(diagnostic.kind, HirDiagnosticKind::TaskNotNameable)),
        "a source-declared Task<T> must not trigger the compiler Task diagnostic"
    );

    ctx.root_visible_source_type_short_names.clear();
    ctx.type_declarations.remove("CancellationToken");
    assert_eq!(
        ctx.lower_type(&named_type_ref("Unit", Vec::new())),
        ResolvedTy::Unit
    );
    assert_eq!(
        ctx.lower_type(&named_type_ref("CancellationToken", Vec::new())),
        ResolvedTy::CancellationToken
    );
    assert_eq!(
        ctx.lower_type(&named_type_ref("Task", i64_arg())),
        ResolvedTy::Unit
    );
    assert!(
        ctx.diagnostics
            .iter()
            .any(|diagnostic| matches!(diagnostic.kind, HirDiagnosticKind::TaskNotNameable)),
        "only the genuine compiler Task spelling must be rejected"
    );
}

#[test]
fn named_import_identity_precedes_task_unit_and_cancellation_early_arms() {
    let tc_output = TypeCheckOutput {
        import_type_name_aliases: HashMap::from([
            ((None, 0, "Task".to_string()), "foo.Task".to_string()),
            ((None, 0, "Unit".to_string()), "foo.Unit".to_string()),
            (
                (None, 0, "CancellationToken".to_string()),
                "foo.CancellationToken".to_string(),
            ),
        ]),
        ..TypeCheckOutput::default()
    };
    let mut ctx = LowerCtx::new(
        &tc_output,
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    ctx.type_declarations.insert(
        "foo.CancellationToken".to_string(),
        hew_types::value_class::DeclaredType {
            is_opaque: true,
            ..Default::default()
        },
    );

    assert_eq!(
        ctx.lower_type(&named_type_ref(
            "Task",
            vec![named_type_ref("i64", Vec::new())],
        )),
        ResolvedTy::named_path(&ctx.defs, "foo.Task", vec![ResolvedTy::I64])
    );
    assert_eq!(
        ctx.lower_type(&named_type_ref(
            "Unit",
            vec![named_type_ref("i64", Vec::new())],
        )),
        ResolvedTy::named_path(&ctx.defs, "foo.Unit", vec![ResolvedTy::I64])
    );
    assert_eq!(
        ctx.lower_type(&named_type_ref("CancellationToken", Vec::new())),
        ResolvedTy::named_opaque_path(&ctx.defs, "foo.CancellationToken", Vec::new())
    );
}

/// Every synthetic-builtin sentinel `ItemId` minted into the
/// `u32::MAX / 2` band is pairwise distinct. A collision is SILENT —
/// two `FnEntry` rows simply overwrite each other in `fn_registry`
/// (the `unlink` × stream-layout alias was caught by inspection, not
/// by a test). This list mirrors the seeding sites exactly:
/// `seed_typed_builtin_fn_registry` (the `supervisor_stop` inline id,
/// `link/monitor/unlink/link_remote/instant::now`) and the pipe
/// layout-witness sentinels (the four `hew_stream_*_layout` entries).
/// Adding a sentinel without extending this list leaves the new id
/// unguarded — extend both together.
#[test]
fn synthetic_builtin_sentinel_ids_are_pairwise_distinct() {
    let ids: Vec<(&str, ItemId)> = vec![
        ("supervisor_stop", ItemId(u32::MAX / 2)),
        ("link", SYNTHETIC_LINK_ITEM),
        ("monitor", SYNTHETIC_MONITOR_ITEM),
        ("unlink", SYNTHETIC_UNLINK_ITEM),
        ("instant::now", SYNTHETIC_INSTANT_NOW_ITEM),
        ("hew_stream_next_layout", SYNTHETIC_STREAM_NEXT_LAYOUT_ITEM),
        (
            "hew_stream_try_next_layout",
            SYNTHETIC_STREAM_TRY_NEXT_LAYOUT_ITEM,
        ),
        ("hew_stream_send_layout", SYNTHETIC_STREAM_SEND_LAYOUT_ITEM),
        (
            "hew_stream_try_send_layout",
            SYNTHETIC_STREAM_TRY_SEND_LAYOUT_ITEM,
        ),
        ("link_remote", SYNTHETIC_LINK_REMOTE_ITEM),
    ];
    for (i, (name_a, id_a)) in ids.iter().enumerate() {
        for (name_b, id_b) in &ids[i + 1..] {
            assert_ne!(
                id_a, id_b,
                "synthetic sentinel ItemId collision: `{name_a}` and \
                     `{name_b}` share {id_a:?} — colliding rows silently \
                     overwrite each other in fn_registry"
            );
        }
    }
}

fn parse_typecheck_and_lower(
    source: &str,
) -> (hew_parser::ast::Program, TypeCheckOutput, LowerOutput) {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let lowered = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());
    (parsed.program, tco, lowered)
}

#[test]
fn checker_admitted_opaque_lifecycle_survives_into_exact_hir_authority() {
    use hew_parser::ast::Program;
    use hew_parser::module::{Module, ModuleGraph, ModulePath};

    let parsed = hew_parser::parse(
        r#"
            #[resource]
            #[opaque]
            pub type FileReadStream {}

            impl FileReadStream {
                fn close(consume self) {
                    unsafe { hew_file_read_stream_close(self) };
                }
            }

            extern "C" {
                fn hew_file_read_stream_open(path: string) -> FileReadStream;
                fn hew_file_read_stream_close(consume stream: FileReadStream);
            }
            "#,
    );
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);

    let module_id = ModulePath::new(["std", "fs"]);
    let root_id = ModulePath::root();
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: module_id.clone(),
            items: parsed.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add std.fs test module");
    graph.topo_order = vec![module_id, root_id];
    let program = Program {
        module_graph: Some(graph),
        items: vec![],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let candidate = output
        .opaque_resource_candidates
        .candidates
        .get(
            &output
                .defs
                .lookup_path("std.fs.FileReadStream")
                .expect("declared resource"),
        )
        .expect("checker must admit the exact generated lifecycle")
        .clone();

    let lowered = lower_program(&program, &output, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}\nitems: {:#?}",
        lowered.diagnostics,
        lowered.module.items
    );
    let lifecycle = lowered
        .module
        .type_classes
        .opaque_resource_lifecycle(&candidate.resource_declaration)
        .expect("HIR must carry the checker-admitted lifecycle");
    assert_eq!(
        lifecycle.resource_declaration,
        candidate.resource_declaration
    );
    assert_eq!(lifecycle.close_declaration, candidate.close_declaration);
    assert_eq!(lifecycle.release_declaration, candidate.release_declaration);
    assert_eq!(lifecycle.release_symbol, "hew_file_read_stream_close");
    assert!(lifecycle.close_symbol.ends_with("FileReadStream::close"));
}

#[test]
fn resource_record_lifecycle_requires_its_exact_emitted_close_body() {
    let (_program, tco, lowered) = parse_typecheck_and_lower(
        r"
            #[resource]
            type Connection { label: string }

            impl Connection {
                fn close(consume self) {}
            }

            fn main() {}
            ",
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "valid exact lifecycle should lower cleanly: {:#?}",
        lowered.diagnostics
    );
    let resource_declaration = lowered
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::TypeDecl(decl) if decl.name == "Connection" => Some(decl.declaration),
            _ => None,
        })
        .unwrap();
    let expected_close = tco
        .impl_method_declaration_ids
        .get("Connection::close")
        .expect("checker close identity");
    let lifecycle = lowered
        .module
        .type_classes
        .lifecycle_registry()
        .resource_record(&resource_declaration)
        .expect("HIR lifecycle admission");
    assert_eq!(&lifecycle.close_declaration, expected_close);

    let mut items = lowered.module.items.clone();
    items.retain(|item| {
            !matches!(item, HirItem::Function(function) if &function.declaration == expected_close)
        });
    let mut table = crate::TypeClassTable::default();
    let mut diagnostics = Vec::new();
    admit_resource_record_lifecycles(
        &items,
        &lowered.module.defs,
        &HashSet::new(),
        &mut table,
        &mut diagnostics,
    );
    assert!(table
        .lifecycle_registry()
        .resource_record(&resource_declaration)
        .is_none());
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        HirDiagnosticKind::CheckerBoundaryViolation { reason, .. }
            if reason.contains("exact emitted body")
    )));
}

#[test]
fn opaque_lifecycle_rejects_a_second_release_hidden_in_control_flow() {
    use hew_parser::ast::Program;
    use hew_parser::module::{Module, ModuleGraph, ModulePath};

    let parsed = hew_parser::parse(
        r#"
            #[resource]
            #[opaque]
            pub type FileReadStream {}

            impl FileReadStream {
                fn close(consume self) {
                    unsafe { hew_file_read_stream_close(self) };
                    if true { unsafe { hew_file_read_stream_close(self) }; }
                }
            }

            extern "C" {
                fn hew_file_read_stream_open(path: string) -> FileReadStream;
                fn hew_file_read_stream_close(consume stream: FileReadStream);
            }
            "#,
    );
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);

    let module_id = ModulePath::new(["std", "fs"]);
    let root_id = ModulePath::root();
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: module_id.clone(),
            items: parsed.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add std.fs test module");
    graph.topo_order = vec![module_id, root_id];
    let program = Program {
        module_graph: Some(graph),
        items: vec![],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert_eq!(output.errors.len(), 1, "{:#?}", output.errors);
    let error = &output.errors[0];
    assert_eq!(error.kind, hew_types::error::TypeErrorKind::UseAfterMove);
    assert_eq!(error.message, "use of moved value `self`");
    assert_eq!(error.source_module.as_deref(), Some("std.fs"));
    // The checker now rejects the duplicate release through the branch.
    // A rejected checker output supplies no declaration classification;
    // HIR must not grant lifecycle authority to this malformed body.
    let candidate = output
        .opaque_resource_candidates
        .candidates
        .get(
            &output
                .defs
                .lookup_path("std.fs.FileReadStream")
                .expect("declared resource"),
        )
        .expect("checker candidate")
        .clone();
    let lowered = lower_program(&program, &output, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered
            .module
            .type_classes
            .opaque_resource_lifecycle(&candidate.resource_declaration)
            .is_none(),
        "a conditional duplicate release must not earn automatic close authority"
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the test keeps static-discard, static-capture, and dynamic dispatch evidence together"
)]
fn receiver_ownership_metadata_controls_static_and_dynamic_dispatch_intent() {
    let (_, tco, lowered) = parse_typecheck_and_lower(
        r"
            #[resource]
            type Builder { value: i64 }

            impl Builder {
                fn close(consume self) {}
            }

            trait Fluent {
                #[returns_receiver]
                fn touch(consume self) -> Self;
            }

            impl Fluent for Builder {
                #[returns_receiver]
                fn touch(consume self) -> Builder { self }
            }

            trait Finish {
                fn finish(consume self) -> i64;
            }

            impl Finish for Builder {
                fn finish(consume self) -> i64 { self.value }
            }

            fn touch_twice<T: Fluent>(consume value: T) {
                value.touch();
                value.touch();
            }

            fn transfer<T: Fluent>(consume value: T) -> T {
                value.touch()
            }

            fn finish_dyn(consume value: dyn Finish) -> i64 {
                value.finish()
            }
            ",
    );
    assert!(
        tco.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::StaticTraitDispatch {
                consumes_receiver: true,
                requires_mutable_receiver: false,
                returns_receiver_identity: true,
                ..
            }
        )),
        "static dispatch must carry both receiver-ownership axes"
    );
    assert!(
        tco.dyn_trait_method_calls
            .values()
            .any(|call| call.signature.consumes_receiver),
        "dynamic dispatch must carry consuming-receiver metadata in its FnSig"
    );

    let touch_twice = function_named(&lowered, "touch_twice");
    for statement in &touch_twice.body.statements {
        let HirStmtKind::Expr(call) = &statement.kind else {
            panic!(
                "expected identity call statement, got {:#?}",
                statement.kind
            );
        };
        let HirExprKind::CallTraitMethodStatic { receiver, .. } = &call.kind else {
            panic!("expected static trait dispatch, got {:#?}", call.kind);
        };
        assert_eq!(
            receiver.intent,
            IntentKind::Read,
            "a discarded exact receiver result preserves the original owner"
        );
    }

    let transfer = function_named(&lowered, "transfer");
    let transfer_call = transfer
        .body
        .tail
        .as_deref()
        .expect("transfer must have a trailing call");
    let HirExprKind::CallTraitMethodStatic { receiver, .. } = &transfer_call.kind else {
        panic!(
            "expected static trait dispatch in transfer, got {:#?}",
            transfer_call.kind
        );
    };
    assert_eq!(
        receiver.intent,
        IntentKind::Consume,
        "a captured exact receiver result transfers the original owner"
    );

    let finish_dyn = function_named(&lowered, "finish_dyn");
    let finish_call = finish_dyn
        .body
        .tail
        .as_deref()
        .expect("finish_dyn must have a trailing call");
    let HirExprKind::CallDynMethod {
        receiver,
        signature,
        ..
    } = &finish_call.kind
    else {
        panic!(
            "expected dynamic trait dispatch in finish_dyn, got {:#?}",
            finish_call.kind
        );
    };
    assert!(signature.consumes_receiver);
    assert_eq!(receiver.intent, IntentKind::Consume);
}

fn main_function_body(output: &LowerOutput) -> &HirBlock {
    let Some(HirItem::Function(function)) = output
        .module
        .items
        .iter()
        .find(|item| matches!(item, HirItem::Function(function) if function.name == "main"))
    else {
        panic!("expected lowered main function: {:#?}", output.module.items);
    };
    &function.body
}

fn const_value_named<'a>(output: &'a LowerOutput, name: &str) -> &'a crate::node::HirConstValue {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Const(item) if item.name == name => Some(&item.value),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected lowered const `{name}`"))
}

fn named_record_ty(name: &str) -> ResolvedTy {
    ResolvedTy::user_for_test(name, vec![])
}

#[test]
fn match_integer_literals_use_platform_sized_scrutinee_type() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            fn signed(x: isize) -> i64 {
                match x {
                    5 => 1,
                    _ => 0,
                }
            }

            fn unsigned(x: usize) -> i64 {
                match x {
                    5 => 1,
                    _ => 0,
                }
            }
            ",
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "platform-sized literal match diagnostics: {:#?}",
        lowered.diagnostics
    );
    let verify_diagnostics = crate::verify_hir(&lowered.module);
    assert!(
        verify_diagnostics.is_empty(),
        "platform-sized literal match verifier diagnostics: {verify_diagnostics:#?}"
    );
    assert_match_literal_ty(&lowered, "signed", &ResolvedTy::Isize);
    assert_match_literal_ty(&lowered, "unsigned", &ResolvedTy::Usize);
}

fn assert_match_literal_ty(output: &LowerOutput, function_name: &str, expected: &ResolvedTy) {
    let function = function_named(output, function_name);
    let Some(tail) = &function.body.tail else {
        panic!("expected `{function_name}` to have a match tail");
    };
    let HirExprKind::Match { arms, .. } = &tail.kind else {
        panic!(
            "expected `{function_name}` tail to be a match, got {:#?}",
            tail.kind
        );
    };
    let Some(first_arm) = arms.first() else {
        panic!("expected `{function_name}` match to have a literal arm");
    };
    match &first_arm.predicate {
        HirMatchArmPredicate::Literal {
            lit: HirLiteral::Integer(5),
            ty,
        } => assert_eq!(ty, expected),
        other => panic!("expected integer literal predicate, got {other:#?}"),
    }
}

fn checker_fact_named<'a>(
    facts: impl Iterator<Item = &'a ClosureCaptureFact>,
    name: &str,
) -> &'a ClosureCaptureFact {
    let mut matches = facts.filter(|fact| fact.name == name);
    let first = matches
        .next()
        .unwrap_or_else(|| panic!("expected checker fact named {name}"));
    assert!(
        matches.next().is_none(),
        "expected exactly one checker fact named {name}"
    );
    first
}

#[test]
fn fold_const_expr_resolves_prior_const_ref() {
    let (_program, _tco, lowered) = parse_typecheck_and_lower(
        r"
            const A: i64 = 10;
            const B: i64 = A + 1;

            fn main() -> i64 {
                B
            }
            ",
    );

    assert!(
        lowered.diagnostics.is_empty(),
        "lower diagnostics: {:#?}",
        lowered.diagnostics
    );
    assert_eq!(
        const_value_named(&lowered, "B"),
        &crate::node::HirConstValue::Integer(11)
    );
}

#[test]
fn fold_const_expr_forward_ref_fails_closed_at_checker() {
    let parsed = hew_parser::parse(
        r"
            const B: i64 = A + 1;
            const A: i64 = 10;

            fn main() -> i64 {
                B
            }
            ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(
        tco.errors
            .iter()
            .any(|error| error.message == "undefined variable `A`"),
        "forward const reference must fail closed before HIR lowering; type errors: {:#?}",
        tco.errors
    );
}

#[test]
fn closure_capture_uses_checker_mode_and_send_fact() {
    let (_program, tco, lowered) = parse_typecheck_and_lower(
        r"
            fn main() {
                let k: i32 = 2;
                let f = |n: i32| n + k;
            }
            ",
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "lower diagnostics: {:#?}",
        lowered.diagnostics
    );

    let checker_fact = checker_fact_named(
        tco.closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter()),
        "k",
    );

    let body = main_function_body(&lowered);
    let HirStmtKind::Let(k_binding, _) = &body.statements[0].kind else {
        panic!("expected first statement to bind k");
    };
    let HirStmtKind::Let(_, Some(closure_expr)) = &body.statements[1].kind else {
        panic!("expected second statement to bind closure");
    };
    let HirExprKind::Closure { captures, .. } = &closure_expr.kind else {
        panic!("expected closure initializer, got {:#?}", closure_expr.kind);
    };
    let hir_capture = captures
        .iter()
        .find(|capture| capture.name == "k")
        .unwrap_or_else(|| panic!("expected HIR capture named k: {captures:#?}"));

    assert_eq!(hir_capture.binding, k_binding.id);
    assert_eq!(hir_capture.acquisition, checker_fact.acquisition);
    assert_eq!(hir_capture.access, checker_fact.access);
    assert_eq!(hir_capture.consumption, checker_fact.consumption);
    assert_eq!(hir_capture.is_send, checker_fact.is_send);
}

#[test]
fn synthesized_actor_handle_lowering_uses_builtin_type_marker() {
    let (_program, _tco, lowered) = parse_typecheck_and_lower(
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let worker = spawn Worker;
            }
            ",
    );

    assert!(
        lowered.diagnostics.is_empty(),
        "lower diagnostics: {:#?}",
        lowered.diagnostics
    );
    let body = main_function_body(&lowered);
    let HirStmtKind::Let(binding, Some(init)) = &body.statements[0].kind else {
        panic!("expected first statement to bind worker");
    };
    assert_eq!(binding.name, "worker");
    assert!(
        matches!(
            &init.ty,
            ResolvedTy::Named { head: name_head @ hew_types::TypeHead::Actor(_), args, .. }
                if name_head.spelling() == "Worker" && args.is_empty()
        ),
        "an actor is the type of its handle: spawn must carry the actor's own \
             name under the handle discriminator, not a wrapper argument: {:?}",
        init.ty
    );
}

/// HIR's recursive mailbox-transfer predicate is the consume-decision half
/// of the actor ownership axis. These cases fix the four independently
/// failable properties of that walk: what it must NOT capture, which
/// references stay shareable, which handles own a release, and how far it
/// descends into an aggregate.
mod mailbox_transfer_gate {
    use super::*;
    use crate::value_class::TypeClassTable;

    fn builtin_handle(name: &str, kind: BuiltinType) -> ResolvedTy {
        let args = vec![named_record_ty("Inner")];
        if kind == BuiltinType::ActorHandle {
            ResolvedTy::actor_for_test(name, args)
        } else {
            ResolvedTy::named_builtin(kind, args)
        }
    }

    fn user_generic_over(inner: ResolvedTy) -> ResolvedTy {
        ResolvedTy::named_for_test("Envelope", vec![inner])
    }

    /// Marker table and structural member sets exactly as the type-decl
    /// pre-pass records them: `Holder` wraps a resource, `Outer` wraps
    /// `Holder`, and `Node` is self-referential through a `Vec`.
    fn tables() -> (TypeClassTable, HashMap<String, Vec<ResolvedTy>>) {
        let mut classes = TypeClassTable::default();
        classes.insert("Socket".to_string(), (ResourceMarker::Resource, None));
        classes.insert("Ticket".to_string(), (ResourceMarker::Linear, None));
        classes.insert("Message".to_string(), (ResourceMarker::None, None));
        classes.insert("Holder".to_string(), (ResourceMarker::None, None));
        classes.insert("Outer".to_string(), (ResourceMarker::None, None));
        classes.insert("Node".to_string(), (ResourceMarker::None, None));

        let mut members: HashMap<String, Vec<ResolvedTy>> = HashMap::new();
        members.insert("Socket".to_string(), vec![ResolvedTy::I64]);
        members.insert("Message".to_string(), vec![ResolvedTy::String]);
        members.insert("Holder".to_string(), vec![named_record_ty("Socket")]);
        members.insert(
            "Outer".to_string(),
            vec![named_record_ty("Holder"), ResolvedTy::I64],
        );
        members.insert(
            "Node".to_string(),
            vec![ResolvedTy::Named {
                args: vec![named_record_ty("Node")],
                head: hew_types::TypeHead::Builtin(BuiltinType::Vec),
                is_opaque: false,
            }],
        );
        (classes, members)
    }

    fn transfers(ty: &ResolvedTy) -> bool {
        let (classes, members) = tables();
        resolved_ty_transfers_ownership_to_mailbox(ty, &classes, &members)
    }

    #[test]
    fn user_shadow_types_and_cow_values_are_not_transfers() {
        for shadow in ["Stream", "Sink"] {
            let user_ty = named_record_ty(shadow);
            assert!(
                !transfers(&user_ty),
                "user type `{shadow}` (builtin: None) must not be consumed as \
                     a mailbox ownership transfer"
            );
            assert!(
                !transfers(&named_record_ty(&format!("mypkg.{shadow}"))),
                "a user module's `{shadow}` is not the stdlib declaration"
            );
            assert!(!transfers(&ResolvedTy::Tuple(vec![
                ResolvedTy::I64,
                user_ty.clone(),
            ])));
            assert!(!transfers(&user_generic_over(user_ty.clone())));
            assert!(!transfers(&ResolvedTy::Array(Box::new(user_ty.clone()), 2)));
            assert!(!transfers(&ResolvedTy::Slice(Box::new(user_ty))));
        }

        assert!(!transfers(&named_record_ty("Message")));
        assert!(!transfers(&named_record_ty("Unregistered")));
        assert!(!transfers(&ResolvedTy::String));
        assert!(!transfers(&ResolvedTy::I64));
        assert!(!transfers(&ResolvedTy::Tuple(vec![
            ResolvedTy::String,
            ResolvedTy::I64,
        ])));
    }

    #[test]
    fn non_owning_actor_references_are_not_transfers() {
        // ChildRef, the actor handle, and the raw runtime word free nothing.
        for (name, kind) in [
            ("ChildRef", BuiltinType::ChildRef),
            ("Worker", BuiltinType::ActorHandle),
        ] {
            assert!(!transfers(&builtin_handle(name, kind)));
        }
        assert!(!transfers(&builtin_handle(
            "HewActor",
            BuiltinType::HewActor
        )));
    }

    #[test]
    fn owning_builtin_handles_transfer_directly_and_nested() {
        // The lambda wrappers and the monitor registration are here BECAUSE
        // they look like references but own a release.
        for (name, kind) in [
            ("Stream", BuiltinType::Stream),
            ("Sink", BuiltinType::Sink),
            ("ActorFn", BuiltinType::ActorFn),
            ("BoxedActor", BuiltinType::BoxedActor),
            ("MonitorRef", BuiltinType::MonitorRef),
        ] {
            let handle = builtin_handle(name, kind);
            assert!(
                transfers(&handle),
                "builtin `{name}` must be consumed as a mailbox ownership transfer"
            );
            assert!(transfers(&ResolvedTy::Tuple(vec![
                ResolvedTy::I64,
                handle.clone(),
            ])));
            assert!(transfers(&user_generic_over(handle.clone())));
            assert!(transfers(&ResolvedTy::Array(Box::new(handle.clone()), 2)));
            assert!(transfers(&ResolvedTy::Slice(Box::new(handle))));
        }
    }

    #[test]
    fn stdlib_declaration_paths_resolve_but_bare_leaf_names_do_not() {
        // A source-declared lifecycle type reaches declared positions
        // spelled by its qualified path with NO builtin tag attached (an
        // actor state field declared `handle: MonitorRef` resolves to
        // `std.link_monitor.MonitorRef`), so the tag test alone misses it.
        assert!(transfers(&named_record_ty("std.link_monitor.MonitorRef")));
        assert!(transfers(&named_record_ty("link_monitor.MonitorRef")));
        assert!(transfers(&named_record_ty("std.stream.Stream")));
        assert!(transfers(&named_record_ty("stream.Sink")));
        // Resolution is by declaration PATH, never by leaf name.
        assert!(!transfers(&named_record_ty("MonitorRef")));
        assert!(!transfers(&named_record_ty(
            "mypkg.link_monitor.MonitorRef"
        )));
    }

    #[test]
    fn nominal_markers_and_named_member_containment_transfer() {
        for nominal in ["Socket", "Ticket"] {
            let ty = named_record_ty(nominal);
            assert!(
                transfers(&ty),
                "`{nominal}` carries a single-owner marker and must be consumed"
            );
            assert!(transfers(&ResolvedTy::Tuple(vec![
                ResolvedTy::I64,
                ty.clone(),
            ])));
            assert!(transfers(&user_generic_over(ty.clone())));
            assert!(transfers(&ResolvedTy::Array(Box::new(ty.clone()), 2)));
            assert!(transfers(&ResolvedTy::Slice(Box::new(ty))));
        }

        // A plain record wrapping a resource transfers it, at depth 1 and 2.
        assert!(transfers(&named_record_ty("Holder")));
        assert!(transfers(&named_record_ty("Outer")));
    }

    #[test]
    fn recursive_member_graphs_terminate_without_transferring() {
        assert!(!transfers(&named_record_ty("Node")));
    }
}

/// Runtime-facing collection, channel, generator, PID, and Option/Result
/// lowering must read the checker-owned builtin discriminator.  A builtin
/// may arrive under a presentation name other than its catalog spelling;
/// conversely, a user source declaration named `Vec`, `Receiver`, etc.
/// must never select the runtime lowering path.
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the table-driven builtin boundary regression intentionally covers every runtime-facing builtin family"
)]
fn builtin_lowering_gates_use_discriminants_not_type_spellings() {
    fn named(name: &str, builtin: Option<BuiltinType>, args: Vec<ResolvedTy>) -> ResolvedTy {
        match builtin {
            Some(BuiltinType::ActorHandle) => ResolvedTy::actor_for_test(name, args),
            Some(builtin) => ResolvedTy::named_builtin(builtin, args),
            None => ResolvedTy::user_for_test(name, args),
        }
    }

    let renamed_map = named(
        "RenamedMap",
        Some(BuiltinType::HashMap),
        vec![ResolvedTy::String, ResolvedTy::I64],
    );
    let user_map = named("HashMap", None, vec![ResolvedTy::String, ResolvedTy::I64]);
    assert!(LowerCtx::is_hashmap_ty(&renamed_map));
    assert!(!LowerCtx::is_hashmap_ty(&user_map));

    let renamed_option = named(
        "MaybeValue",
        Some(BuiltinType::Option),
        vec![ResolvedTy::I64],
    );
    let user_option = named("Option", None, vec![ResolvedTy::I64]);
    assert_eq!(
        LowerCtx::resolved_option_inner(&renamed_option),
        Some(&ResolvedTy::I64)
    );
    assert_eq!(LowerCtx::resolved_option_inner(&user_option), None);

    let renamed_result = named(
        "Outcome",
        Some(BuiltinType::Result),
        vec![ResolvedTy::I64, ResolvedTy::String],
    );
    let user_result = named("Result", None, vec![ResolvedTy::I64, ResolvedTy::String]);
    assert_eq!(
        LowerCtx::resolved_result_parts(&renamed_result),
        Some((&ResolvedTy::I64, &ResolvedTy::String))
    );
    assert_eq!(LowerCtx::resolved_result_parts(&user_result), None);

    let renamed_generator = named(
        "Producer",
        Some(BuiltinType::Generator),
        vec![ResolvedTy::I64, ResolvedTy::String],
    );
    let user_generator = named("Generator", None, vec![ResolvedTy::I64, ResolvedTy::String]);
    assert_eq!(
        LowerCtx::generator_yield_return_parts(&renamed_generator),
        Some((ResolvedTy::I64, ResolvedTy::String))
    );
    assert_eq!(
        LowerCtx::generator_yield_return_parts(&user_generator),
        None
    );

    // An actor is the type of its handle: the identity is the handle's own
    // name, and a same-named user nominal without the discriminator is not
    // an actor handle.
    let handle = named("bank.Account", Some(BuiltinType::ActorHandle), Vec::new());
    let user_nominal = named("bank.Account", None, Vec::new());
    assert_eq!(
        LowerCtx::actor_handle_identity(&handle),
        Some("bank.Account")
    );
    assert_eq!(LowerCtx::actor_handle_identity(&user_nominal), None);

    let mut vec_ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    vec_ctx.expr_types.insert(
        SpanKey::in_module(&(0..0), 0),
        Ty::Named {
            args: vec![Ty::I64],
            head: hew_types::TypeHead::Builtin(BuiltinType::Vec),
        },
    );
    assert_eq!(
        vec_ctx.array_literal_ty(&(0..0)).map(|(_, elem)| elem),
        Some(ResolvedTy::I64),
        "a renamed builtin Vec<T> still owns array literal lowering"
    );

    let mut user_vec_ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    user_vec_ctx.expr_types.insert(
        SpanKey::in_module(&(0..0), 0),
        Ty::user_for_test("Vec", vec![Ty::I64]),
    );
    let stored_user_vec_ty = user_vec_ctx
        .expr_types
        .get(&SpanKey::in_module(&(0..0), 0))
        .and_then(|ty| ResolvedTy::from_ty(ty).ok())
        .map(|ty| user_vec_ctx.qualify_current_module_record_ty(ty));
    assert!(
        matches!(
            stored_user_vec_ty,
            Some(ResolvedTy::Named {
                head: hew_types::TypeHead::Nominal(_)
                    | hew_types::TypeHead::Param(_)
                    | hew_types::TypeHead::Unresolved(_),
                ..
            })
        ),
        "checker/HIR qualification must preserve a user Vec<T>, got {stored_user_vec_ty:?}"
    );
    let user_vec_literal_ty = user_vec_ctx.array_literal_ty(&(0..0));
    assert!(
            user_vec_literal_ty.is_none(),
            "a user Vec<T> must not acquire array literal runtime lowering, got {user_vec_literal_ty:?}"
        );

    let mut map_ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    map_ctx.expr_types.insert(
        SpanKey::in_module(&(0..0), 0),
        Ty::Named {
            args: vec![Ty::String, Ty::I64],
            head: hew_types::TypeHead::Builtin(BuiltinType::HashMap),
        },
    );
    assert_eq!(
        map_ctx.map_literal_hashmap_ty(&(0..0)),
        Some((
            named(
                "RenamedMap",
                Some(BuiltinType::HashMap),
                vec![ResolvedTy::String, ResolvedTy::I64],
            ),
            ResolvedTy::String,
            ResolvedTy::I64,
        )),
        "a renamed builtin HashMap<K, V> still owns map literal lowering"
    );
    let mut user_map_ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    user_map_ctx.expr_types.insert(
        SpanKey::in_module(&(0..0), 0),
        Ty::user_for_test("HashMap", vec![Ty::String, Ty::I64]),
    );
    assert!(
        user_map_ctx.map_literal_hashmap_ty(&(0..0)).is_none(),
        "a user HashMap<K, V> must not acquire map literal runtime lowering"
    );

    let mut select_ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    let renamed_stream = select_ctx.make_expr(
        HirExprKind::Unsupported("test stream".to_string()),
        named(
            "RenamedStream",
            Some(BuiltinType::Stream),
            vec![ResolvedTy::String],
        ),
        IntentKind::Read,
        0..0,
    );
    assert_eq!(
        select_ctx.select_arm_binding_ty(
            &HirSelectArmKind::StreamNext {
                stream: Box::new(renamed_stream),
            },
            &(0..0),
        ),
        Some(LowerCtx::resolved_option_ty(ResolvedTy::String)),
        "a renamed builtin Stream<T> retains stream-next binding shape"
    );
    let user_stream = select_ctx.make_expr(
        HirExprKind::Unsupported("test stream".to_string()),
        named("Stream", None, vec![ResolvedTy::String]),
        IntentKind::Read,
        0..0,
    );
    assert_eq!(
        select_ctx.select_arm_binding_ty(
            &HirSelectArmKind::StreamNext {
                stream: Box::new(user_stream),
            },
            &(0..0),
        ),
        None,
        "a user Stream<T> must not acquire stream-next semantics"
    );

    let task = select_ctx.make_expr(
        HirExprKind::Unsupported("test task".to_string()),
        ResolvedTy::Task(Box::new(ResolvedTy::I64)),
        IntentKind::Read,
        0..0,
    );
    assert_eq!(
        select_ctx.select_arm_binding_ty(
            &HirSelectArmKind::TaskAwait {
                task: Box::new(task),
            },
            &(0..0),
        ),
        Some(ResolvedTy::I64),
        "Task<T> retains its await binding type"
    );

    let object = (Expr::Ident(Ident::new("xs")), 0..2);
    let index = (
        Expr::Literal(Literal::Integer {
            value: 0,
            radix: hew_parser::ast::IntRadix::Decimal,
        }),
        3..4,
    );
    let mut renamed_vec_facts = HashMap::new();
    renamed_vec_facts.insert(
        SpanKey::in_module(&object.1, 0),
        Ty::Named {
            args: vec![Ty::Unit],
            head: hew_types::TypeHead::Builtin(BuiltinType::Vec),
        },
    );
    let mut renamed_vec_diagnostics = Vec::new();
    check_vec_index_element_type(
        &object,
        &index,
        &(0..4),
        &renamed_vec_facts,
        &mut renamed_vec_diagnostics,
    );
    assert!(
        matches!(
            renamed_vec_diagnostics.as_slice(),
            [HirDiagnostic {
                kind: HirDiagnosticKind::VecIndexElementTypeUnsupported { .. },
                ..
            }]
        ),
        "a renamed builtin Vec<Unit> must retain the Vec runtime ABI gate"
    );

    let mut user_vec_facts = HashMap::new();
    user_vec_facts.insert(
        SpanKey::in_module(&object.1, 0),
        Ty::user_for_test("Vec", vec![Ty::Unit]),
    );
    let mut user_vec_diagnostics = Vec::new();
    check_vec_index_element_type(
        &object,
        &index,
        &(0..4),
        &user_vec_facts,
        &mut user_vec_diagnostics,
    );
    assert!(
        user_vec_diagnostics.is_empty(),
        "a user Vec<Unit> must not enter the builtin Vec ABI gate"
    );
}

#[test]
fn missing_closure_type_or_escape_facts_fail_closed() {
    for remove_type in [true, false] {
        let (program, mut tco, _) =
            parse_typecheck_and_lower("fn main() { let value: i64 = 1; let f = || value; }");
        if remove_type {
            tco.expr_types
                .retain(|key, _| !tco.closure_capture_facts.contains_key(key));
        } else {
            tco.closure_escape_facts.clear();
        }
        let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());
        assert!(
            lowered.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind, HirDiagnosticKind::CheckerBoundaryViolation { name, .. }
                    if name == "closure literal"
            )),
            "{:#?}",
            lowered.diagnostics
        );
        assert!(lowered.into_result().is_err());
    }
}

#[test]
fn missing_closure_capture_facts_emit_boundary_diagnostic() {
    let (program, mut tco, _) = parse_typecheck_and_lower(
        r"
            fn main() {
                let k: i32 = 2;
                let f = |n: i32| n + k;
            }
            ",
    );
    assert!(
        !tco.closure_capture_facts.is_empty(),
        "test setup requires checker-produced closure capture facts"
    );
    tco.closure_capture_facts.clear();

    let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());

    assert!(
        lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                if name == "closure literal"
                    && reason == "closure_capture_facts has no record for closure literal span"
        )),
        "missing closure_capture_facts entry must emit root-cause boundary diagnostic; got {:#?}",
        lowered.diagnostics
    );
}

/// When `closure_capture_facts` are missing, `into_result()` must return
/// `Err` — not `Ok` with empty captures.  This pins the fail-closed
/// contract at the public API boundary.
#[test]
fn missing_closure_capture_facts_into_result_is_err() {
    let (program, mut tco, _) = parse_typecheck_and_lower(
        r"
            fn main() {
                let k: i32 = 2;
                let f = |n: i32| n + k;
            }
            ",
    );
    assert!(
        !tco.closure_capture_facts.is_empty(),
        "test setup requires checker-produced closure capture facts"
    );
    tco.closure_capture_facts.clear();

    let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered.into_result().is_err(),
        "into_result() must return Err when closure capture facts are missing"
    );
}

#[test]
fn stdlib_println_resolves_to_i64_overload() {
    let (_program, _tco, lowered) = parse_typecheck_and_lower(
        r"
            fn main() {
                println(42);
            }
            ",
    );
    assert!(
        !lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "println"
        )),
        "println must not fall through to unresolved symbol: {:#?}",
        lowered.diagnostics
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "lower diagnostics: {:#?}",
        lowered.diagnostics
    );

    let body = main_function_body(&lowered);
    let HirStmtKind::Expr(expr) = &body.statements[0].kind else {
        panic!("expected println statement expression");
    };
    let HirExprKind::Call { callee, .. } = &expr.kind else {
        panic!("expected println call, got {:#?}", expr.kind);
    };
    let HirExprKind::BindingRef { name, resolved } = &callee.kind else {
        panic!("expected callee binding ref, got {:#?}", callee.kind);
    };
    assert_eq!(name, "println_i64");
    assert!(
        matches!(resolved, ResolvedRef::Item(_)),
        "expected println_i64 item ref, got {resolved:?}"
    );
}

#[test]
fn stdlib_println_unsupported_type_emits_overload_diagnostic() {
    let parsed = hew_parser::parse(
        r"
            fn main() {
                let w: Widget;
                println(w);
            }
            ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    // This test isolates HIR's no-overload diagnostic and intentionally
    // omits checker semantic facts, but the declaration boundary must
    // still carry the immutable identity view produced for this exact
    // source program.
    let tco = TypeCheckOutput {
        defs: checked.defs,
        ..TypeCheckOutput::default()
    };
    let lowered = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);
    assert!(
        lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::UnresolvedBuiltinOverload { name, arg_ty }
                if name == "println"
                    && matches!(arg_ty, ResolvedTy::Named { head: name_head, .. } if name_head.spelling() == "Widget")
        )),
        "expected unsupported println overload diagnostic, got {:#?}",
        lowered.diagnostics
    );
    assert!(
        !lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "println"
        )),
        "unsupported println overload must not fall through to UnresolvedSymbol: {:#?}",
        lowered.diagnostics
    );
}

#[test]
fn missing_stdlib_module_field_emits_import_missing() {
    let parsed = hew_parser::parse(
        r"
            fn main() {
                let _ = fs.read;
            }
            ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let tco = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    let lowered = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);
    assert!(
        lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::ImportMissing { module, name }
                if module == "std.fs"
                    && name == "fs.read"
                    && diagnostic.note == "add 'import std.fs;' at the top of the file"
        )),
        "expected missing import diagnostic for fs.read, got {:#?}",
        lowered.diagnostics
    );
    assert!(
        !lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "fs"
        )),
        "fs.read must not fall through to UnresolvedSymbol: {:#?}",
        lowered.diagnostics
    );
}

#[test]
fn missing_stdlib_module_call_emits_import_missing() {
    let parsed = hew_parser::parse(
        r#"
            fn main() {
                let _ = fs.read("test.txt");
            }
            "#,
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let tco = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    let lowered = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);
    assert!(
        lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::ImportMissing { module, name }
                if module == "std.fs"
                    && name == "fs.read"
                    && diagnostic.note == "add 'import std.fs;' at the top of the file"
        )),
        "expected missing import diagnostic for fs.read call, got {:#?}",
        lowered.diagnostics
    );
    assert!(
        !lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::MethodCallNoRewrite { method } if method == "read"
        )),
        "fs.read call must not fall through to MethodCallNoRewrite: {:#?}",
        lowered.diagnostics
    );
}

// ── Select arm-binding scoping ──────────────────────────────────────────
//
// Source shared by several tests below: two asks against the same actor
// type. Both arm bodies return the bound name so the arm body types
// agree — `ActorError<E, M>` carries the source's actor-handle type
// inside `Message<_, ..>`, so arms asking different actor types would produce
// distinct arm-body types and fail the select's own arm-unification
// check; that is a real type distinction, not a scoping one, so both
// arms ask the same actor here to isolate binding scoping from it.
// Distinct binding names (`reply` vs `verdict`) let us prove each arm
// has its own BindingId.

const SELECT_SCOPE_SOURCE: &str = r"
        actor Pinger {
            receive fn ping() -> i64 { 1 }
        }
        fn main() {
            let p = spawn Pinger;
            let c = spawn Pinger;
            let result = select {
                reply from p.ping() => reply,
                verdict from c.ping() => verdict,
            };
        }
    ";

/// Helper: walk a lowered HIR expression looking for the first
/// `BindingRef` whose `name` matches `target`. Returns `Some(resolved)`.
fn find_binding_ref_named<'a>(expr: &'a HirExpr, target: &str) -> Option<&'a ResolvedRef> {
    if let HirExprKind::BindingRef { name, resolved } = &expr.kind {
        if name == target {
            return Some(resolved);
        }
    }
    // Recurse into children where an arm body might live.
    if let HirExprKind::Select(sel) = &expr.kind {
        for arm in &sel.arms {
            if let Some(r) = find_binding_ref_named(&arm.body, target) {
                return Some(r);
            }
        }
    }
    None
}

/// Walk each arm's body separately and return the resolved ref for
/// `target` found only within `arm_index`.
fn find_binding_ref_in_arm<'a>(
    select_expr: &'a HirExpr,
    arm_index: usize,
    target: &str,
) -> Option<&'a ResolvedRef> {
    let HirExprKind::Select(sel) = &select_expr.kind else {
        return None;
    };
    let arm = sel.arms.get(arm_index)?;
    find_binding_ref_named(&arm.body, target)
}

/// Return the select expression from `main`'s first statement's let
/// initialiser.
fn main_select_expr(output: &LowerOutput) -> &HirExpr {
    let body = main_function_body(output);
    // The select is the initialiser of the third let (`let _ = select{..}`).
    // Statements: 0=let p, 1=let c, 2=let _ = select.
    let HirStmtKind::Let(_, Some(init)) = &body.statements[2].kind else {
        panic!(
            "expected third statement to be `let _ = select{{..}}`, got {:#?}",
            body.statements
        );
    };
    init
}

#[test]
fn select_actor_ask_arm_body_resolves_own_binding() {
    // Arm body reference to `reply` must resolve to a Binding, not
    // Unresolved — confirming the arm's scope registration works.
    let (_, _, lowered) = parse_typecheck_and_lower(SELECT_SCOPE_SOURCE);
    assert!(
        lowered.diagnostics.is_empty(),
        "unexpected lower diagnostics: {:#?}",
        lowered.diagnostics
    );

    let diagnostics = crate::verify::verify_hir(&lowered.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let select_expr = main_select_expr(&lowered);
    let resolved = find_binding_ref_in_arm(select_expr, 0, "reply")
        .unwrap_or_else(|| panic!("expected BindingRef 'reply' in arm 0"));

    let HirExprKind::Select(select) = &select_expr.kind else {
        panic!("expected select");
    };
    assert_eq!(
        resolved,
        &ResolvedRef::Binding(select.arms[0].binding_id.expect("reply binding")),
        "the arm body must reference its own reply binding"
    );
}

#[test]
fn select_sibling_arm_binding_is_not_visible() {
    // Arm 1's body references `reply` (arm 0's binding name). Since
    // there is no outer `reply` binding, it must produce UnresolvedSymbol.
    let source = r"
            actor Pinger {
                receive fn ping() -> i64 { 1 }
            }
            actor Checker {
                receive fn check() -> i64 { 2 }
            }
            fn main() {
                let p = spawn Pinger;
                let c = spawn Checker;
                let result = select {
                    reply from p.ping() => reply,
                    _verdict from c.check() => reply,
                };
            }
        ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Type errors are expected (reply is unresolved in arm 1 context);
    // we proceed to HIR lowering regardless.
    let tco = checker.check_program(&parsed.program);
    assert!(
        tco.errors.iter().any(|error| {
            error.kind == hew_types::error::TypeErrorKind::UndefinedVariable
                && &source[error.span.clone()] == "reply"
        }),
        "the out-of-scope reply must be rejected by the checker: {:?}",
        tco.errors
    );
    let lowered = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());

    assert!(
        lowered.diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "reply"
        )),
        "arm 1 body referencing arm 0's 'reply' must emit UnresolvedSymbol; got {:#?}",
        lowered.diagnostics
    );
}

#[test]
fn select_arm_binding_not_visible_after_select() {
    // A reference to `reply` after the select (outside the select body)
    // must produce UnresolvedSymbol — the scope is popped on arm exit.
    let source = r"
            actor Pinger {
                receive fn ping() -> i64 { 1 }
            }
            fn main() {
                let p = spawn Pinger;
                let result = select {
                    reply from p.ping() => reply,
                };
                let late = reply;
            }
        ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(
        tco.errors.iter().any(|error| {
            error.kind == hew_types::error::TypeErrorKind::UndefinedVariable
                && &source[error.span.clone()] == "reply"
        }),
        "the out-of-scope reply must be rejected by the checker: {:?}",
        tco.errors
    );
    let lowered = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());

    assert!(
        lowered.diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "reply"
        )),
        "reference to arm binding outside select must emit UnresolvedSymbol; got {:#?}",
        lowered.diagnostics
    );
}

#[test]
fn select_multiple_arms_have_independent_binding_ids() {
    // Two arms with distinct binding names must each resolve to their own
    // BindingId in their respective bodies.
    let (_, _, lowered) = parse_typecheck_and_lower(SELECT_SCOPE_SOURCE);
    assert!(
        lowered.diagnostics.is_empty(),
        "unexpected lower diagnostics: {:#?}",
        lowered.diagnostics
    );

    let diagnostics = crate::verify::verify_hir(&lowered.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let select_expr = main_select_expr(&lowered);
    let reply_ref = find_binding_ref_in_arm(select_expr, 0, "reply")
        .unwrap_or_else(|| panic!("expected BindingRef 'reply' in arm 0"));
    let verdict_ref = find_binding_ref_in_arm(select_expr, 1, "verdict")
        .unwrap_or_else(|| panic!("expected BindingRef 'verdict' in arm 1"));

    let ResolvedRef::Binding(reply_id) = reply_ref else {
        panic!("arm 0 'reply' must be Binding, got {reply_ref:?}");
    };
    let ResolvedRef::Binding(verdict_id) = verdict_ref else {
        panic!("arm 1 'verdict' must be Binding, got {verdict_ref:?}");
    };

    let HirExprKind::Select(select) = &select_expr.kind else {
        panic!("expected select");
    };
    assert_eq!(Some(*reply_id), select.arms[0].binding_id);
    assert_eq!(Some(*verdict_id), select.arms[1].binding_id);
    assert_ne!(
        reply_id, verdict_id,
        "distinct arm bindings must have distinct BindingIds"
    );
}

#[test]
fn select_sources_missing_or_stale_are_rejected() {
    let (program, checked, lowered) = parse_typecheck_and_lower(SELECT_SCOPE_SOURCE);
    assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
    for stale in [false, true] {
        let mut damaged = checked.clone();
        if stale {
            // Both entries are valid actor asks, but belong to the other arm.
            damaged
                .select_sources
                .values_mut()
                .next()
                .expect("checked select")
                .swap(0, 1);
        } else {
            damaged.select_sources.clear();
        }
        let output = lower_program(&program, &damaged, &ResolutionCtx, TargetArch::host());
        assert!(
            output.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                HirDiagnosticKind::CheckerBoundaryViolation { name, .. } if name == "select source"
            )),
            "invalid select source facts must be rejected: {:?}",
            output.diagnostics
        );
        assert!(
            output.into_result().is_err(),
            "invalid select source facts must make lowering fatal"
        );
    }
}

fn function_named<'a>(output: &'a LowerOutput, name: &str) -> &'a HirFn {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => Some(function),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected lowered function `{name}`"))
}

fn first_let_value(function: &HirFn) -> &HirExpr {
    let Some(stmt) = function.body.statements.first() else {
        panic!("expected at least one statement in `{}`", function.name);
    };
    match &stmt.kind {
        HirStmtKind::Let(_, Some(value)) => value,
        other => panic!("expected first statement to be let-with-value, got {other:#?}"),
    }
}

fn enum_variant_name(arm: &HirMatchArm) -> (&str, u32) {
    match &arm.predicate {
        HirMatchArmPredicate::EnumVariant {
            variant_match,
            variant_idx,
        } => (&variant_match.variant_name, *variant_idx),
        other => panic!("expected enum-variant predicate, got {other:#?}"),
    }
}

fn assert_result_try_match(expr: &HirExpr) {
    let HirExprKind::Match { arms, .. } = &expr.kind else {
        panic!("expected `?` to lower to Match, got {:#?}", expr.kind);
    };
    assert_eq!(arms.len(), 2, "Result? match must have Ok and Err arms");
    let (ok_name, _) = enum_variant_name(&arms[0]);
    let (err_name, err_idx) = enum_variant_name(&arms[1]);
    assert_eq!(ok_name, "Ok");
    assert_eq!(err_name, "Err");
    assert_eq!(arms[0].bindings.len(), 1, "Ok arm must bind payload");
    assert_eq!(arms[1].bindings.len(), 1, "Err arm must bind payload");
    match &arms[0].body.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => assert_eq!(*id, arms[0].bindings[0].binding),
        other => panic!("Ok arm must return its payload binding, got {other:#?}"),
    }
    let HirExprKind::Block(block) = &arms[1].body.kind else {
        panic!(
            "Err arm must be a return block, got {:#?}",
            arms[1].body.kind
        );
    };
    assert!(block.tail.is_none(), "Err return block must have no tail");
    let [return_stmt] = block.statements.as_slice() else {
        panic!("Err return block must contain exactly one statement: {block:#?}");
    };
    let HirStmtKind::Return(Some(return_expr)) = &return_stmt.kind else {
        panic!("Err block statement must be Return(Some), got {return_stmt:#?}");
    };
    match &return_expr.kind {
        HirExprKind::MachineVariantCtor {
            machine_name,
            state_idx,
            payload: Some(payload),
        } => {
            assert_eq!(machine_name, "Result");
            assert_eq!(
                u32::try_from(*state_idx).expect("variant index must fit in u32"),
                err_idx
            );
            assert_eq!(payload.len(), 1, "Err ctor must carry one payload");
        }
        other => panic!("Err return must construct Result::Err, got {other:#?}"),
    }
}

fn assert_option_try_match(expr: &HirExpr) {
    let HirExprKind::Match { arms, .. } = &expr.kind else {
        panic!("expected `?` to lower to Match, got {:#?}", expr.kind);
    };
    assert_eq!(arms.len(), 2, "Option? match must have Some and None arms");
    let (some_name, _) = enum_variant_name(&arms[0]);
    let (none_name, none_idx) = enum_variant_name(&arms[1]);
    assert_eq!(some_name, "Some");
    assert_eq!(none_name, "None");
    assert_eq!(arms[0].bindings.len(), 1, "Some arm must bind payload");
    assert!(
        arms[1].bindings.is_empty(),
        "None arm has no payload binding"
    );
    match &arms[0].body.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => assert_eq!(*id, arms[0].bindings[0].binding),
        other => panic!("Some arm must return its payload binding, got {other:#?}"),
    }
    let HirExprKind::Block(block) = &arms[1].body.kind else {
        panic!(
            "None arm must be a return block, got {:#?}",
            arms[1].body.kind
        );
    };
    let [return_stmt] = block.statements.as_slice() else {
        panic!("None return block must contain exactly one statement: {block:#?}");
    };
    let HirStmtKind::Return(Some(return_expr)) = &return_stmt.kind else {
        panic!("None block statement must be Return(Some), got {return_stmt:#?}");
    };
    match &return_expr.kind {
        HirExprKind::MachineVariantCtor {
            machine_name,
            state_idx,
            payload: None,
        } => {
            assert_eq!(machine_name, "Option");
            assert_eq!(
                u32::try_from(*state_idx).expect("variant index must fit in u32"),
                none_idx
            );
        }
        other => panic!("None return must construct Option::None, got {other:#?}"),
    }
}

#[test]
fn postfix_try_result_lowers_to_enum_match_returning_err() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            fn pass(r: Result<i64, i64>) -> Result<i64, i64> {
                let x: i64 = r?;
                .Ok(x)
            }
            ",
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        lowered.diagnostics
    );
    let pass = function_named(&lowered, "pass");
    assert_result_try_match(first_let_value(pass));
}

#[test]
fn postfix_try_missing_checker_expr_type_is_diagnosed_and_unsupported() {
    let source = r"
            fn pass(r: Result<i64, i64>) -> Result<i64, i64> {
                let x: i64 = r?;
                .Ok(x)
            }
        ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let try_start = source.find("r?").expect("source must contain `r?`");
    let try_span = try_start..try_start + 2;
    assert!(
        tco.expr_types
            .remove(&SpanKey::in_module(&try_span, 0))
            .is_some(),
        "checker must publish the `?` expression type"
    );

    let lowered = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                if name == "`?` expression" && reason == "missing expr_types entry"
        )),
        "missing checker type must be diagnosed: {:#?}",
        lowered.diagnostics
    );
    let pass = function_named(&lowered, "pass");
    assert!(
        matches!(first_let_value(pass).kind, HirExprKind::Unsupported(_)),
        "missing checker type must lower `?` as unsupported"
    );
}

#[test]
fn postfix_try_preserves_opaque_payload_representation() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            #[opaque]
            type Handle {}

            fn pass(r: Result<Handle, string>) -> Result<Handle, string> {
                let handle = r?;
                .Ok(handle)
            }
            ",
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        lowered.diagnostics
    );

    let pass = function_named(&lowered, "pass");
    let try_expr = first_let_value(pass);
    assert_result_try_match(try_expr);
    let HirExprKind::Match { arms, .. } = &try_expr.kind else {
        unreachable!("assert_result_try_match already checked the expression shape");
    };
    for (surface, ty) in [
        ("try expression", &try_expr.ty),
        ("Ok payload binding", &arms[0].bindings[0].ty),
        ("Ok payload body", &arms[0].body.ty),
    ] {
        assert!(
            matches!(
                ty,
                ResolvedTy::Named { head: name_head, is_opaque: true, .. } if name_head.spelling() == "Handle"
            ),
            "{surface} must preserve the opaque Handle discriminator; got {ty:#?}"
        );
    }
}

#[test]
fn postfix_try_option_lowers_to_enum_match_returning_none() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            fn pass(o: Option<i64>) -> Option<i64> {
                let x: i64 = o?;
                .Some(x)
            }
            ",
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        lowered.diagnostics
    );
    let pass = function_named(&lowered, "pass");
    assert_option_try_match(first_let_value(pass));
}

fn lower_canonical_encoding_fixture(
    format: &str,
    source: &str,
    module_source: &str,
) -> LowerOutput {
    use hew_parser::module::{Module, ModuleGraph, ModulePath};
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut program = parsed.program;
    let Item::Import(import) = &mut program.items[0].0 else {
        panic!("import fixture")
    };
    let imported = hew_parser::parse(module_source);
    assert!(imported.errors.is_empty(), "{:?}", imported.errors);
    import.resolved_items = Some(imported.program.items.clone().into());
    import.resolved_source_paths = vec![std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join(format!("std/encoding/{format}/{format}.hew"))];
    let module = ModulePath::new(["std", "encoding", format]);
    let root = ModulePath::root();
    let mut graph = ModuleGraph::new(root.clone());
    graph
        .add_module(Module {
            id: module.clone(),
            items: imported.program.items,
            imports: Vec::new(),
            source_paths: import.resolved_source_paths.clone(),
            doc: None,
        })
        .unwrap();
    // Every compiled program carries the std Option/Result method bodies.
    let mut order = Vec::new();
    for (name, prelude_source) in [
        ("option", include_str!("../../../std/option.hew")),
        ("result", include_str!("../../../std/result.hew")),
    ] {
        let id = ModulePath::new(["std", name]);
        graph
            .add_module(Module {
                id: id.clone(),
                items: hew_parser::parse(prelude_source).program.items,
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: None,
            })
            .unwrap();
        order.push(id);
    }
    order.extend([module, root]);
    graph.topo_order = order;
    program.module_graph = Some(graph);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&program);
    assert!(tco.errors.is_empty(), "{:?}", tco.errors);
    let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(lowered.diagnostics.is_empty(), "{:#?}", lowered.diagnostics);
    lowered
}

#[test]
fn selected_encoding_import_keeps_checked_identity_through_payload_extraction() {
    for (format, builtin) in [
        ("json", BuiltinType::JsonValue),
        ("yaml", BuiltinType::YamlValue),
    ] {
        let source = format!(
            r#"
                import std.encoding.{format}.{{self, Value}};
                fn required_field(obj: Value, key: string) -> Result<Value, string> {{
                    match obj.get_field(key) {{
                        .Ok(.Some(value)) => .Ok(value),
                        .Ok(.None) => .Err("missing field"),
                        .Err(error) => .Err(error),
                    }}
                }}
                fn result_probe(obj: Value) -> Result<Value, string> {{
                    let child = required_field(obj, "field")?;
                    .Ok(child)
                }}
                fn option_probe(value: Option<Value>) -> Option<Value> {{
                    let child = value?;
                    .Some(child)
                }}
                fn result_expect_probe(consume value: Result<Value, string>) -> Value {{
                    let child = value.expect("the payload decoded");
                    child
                }}
                fn option_expect_probe(consume value: Option<Value>) -> Value {{
                    let child = value.expect("the payload is present");
                    child
                }}
                fn result_unwrap_or_probe(consume value: Result<Value, string>, consume fallback: Value) -> Value {{
                    let child = value.unwrap_or(fallback);
                    child
                }}
                fn option_unwrap_or_probe(consume value: Option<Value>, consume fallback: Value) -> Value {{
                    let child = value.unwrap_or(fallback);
                    child
                }}
                fn field_probe(value: Value) -> Value {{
                    let child = value.get_field("key").expect("a").expect("b");
                    child
                }}
            "#
        );
        let lowered = lower_canonical_encoding_fixture(
            format,
            &source,
            r"
                #[opaque] pub type Value {}
                impl Value {
                    pub fn get_field(self, key: string) -> Result<Option<Value>, string> {
                        .Ok(.Some(self))
                    }
                }
            ",
        );
        let expected = ResolvedTy::Named {
            args: Vec::new(),
            head: hew_types::TypeHead::Builtin(builtin),
            is_opaque: true,
        };
        let required = function_named(&lowered, "required_field");
        assert_eq!(required.params[0].ty, expected);
        // `?` desugars to a match whose payload binding keeps the
        // selected encoding identity.
        for name in ["result_probe", "option_probe"] {
            let expression = first_let_value(function_named(&lowered, name));
            let HirExprKind::Match { arms, .. } = &expression.kind else {
                panic!("payload extraction must lower to a match: {expression:#?}")
            };
            assert_eq!(expression.ty, expected);
            assert_eq!(arms[0].bindings[0].ty, expected);
            assert_eq!(arms[0].body.ty, expected);
        }
        // The std Option/Result methods are calls whose result keeps it.
        for name in [
            "result_expect_probe",
            "option_expect_probe",
            "result_unwrap_or_probe",
            "option_unwrap_or_probe",
            "field_probe",
        ] {
            let expression = first_let_value(function_named(&lowered, name));
            let HirExprKind::Call { args, .. } = &expression.kind else {
                panic!("payload extraction must lower to a method call: {expression:#?}")
            };
            assert_eq!(expression.ty, expected);
            if name == "field_probe" {
                let option = ResolvedTy::named_builtin(BuiltinType::Option, vec![expected.clone()]);
                assert_eq!(args[0].ty, option);
            }
        }
    }
}

#[test]
fn imported_encoding_mutators_keep_the_checked_writeback_contract() {
    for (format, builtin) in [
        ("json", BuiltinType::JsonValue),
        ("yaml", BuiltinType::YamlValue),
    ] {
        let source = format!(
            r#"
                import std.encoding.{format}.{{self, Value}};
                fn push_probe(var value: Value, child: Value) -> Result<(), string> {{ value.push(child) }}
                fn set_probe(var value: Value, child: Value) -> Result<(), string> {{ value.set("key", child) }}
                fn read_probe(value: Value) -> i64 {{ value.count() }}
            "#
        );
        let lowered = lower_canonical_encoding_fixture(
            format,
            &source,
            r"
                #[opaque] pub type Value {}
                pub trait ValueMethods {
                    fn push(var self, child: Value) -> Result<(), string>;
                    fn set(var self, key: string, child: Value) -> Result<(), string>;
                    fn count(self) -> i64;
                }
                impl ValueMethods for Value {
                    fn push(var self, child: Value) -> Result<(), string> { self = child; .Ok(()) }
                    fn set(var self, key: string, child: Value) -> Result<(), string> { self = child; .Ok(()) }
                    fn count(self) -> i64 { 0 }
                }
            ",
        );
        let expected = ResolvedTy::Named {
            args: vec![],
            head: hew_types::TypeHead::Builtin(builtin),
            is_opaque: true,
        };
        for (name, arity) in [("push_probe", 1), ("set_probe", 2)] {
            let call = function_named(&lowered, name).body.tail.as_ref().unwrap();
            let HirExprKind::VarSelfMethodCall {
                receiver,
                receiver_ty,
                call_target: CallTarget::ImplMethod(declaration),
                args,
                ret_ty,
                ..
            } = &call.kind
            else {
                panic!("{format} {name} must retain receiver writeback: {call:#?}")
            };
            assert_eq!(receiver.intent, IntentKind::Consume);
            assert_eq!(receiver.ty, expected);
            assert_eq!(*receiver_ty, expected);
            assert_eq!(args.len(), arity);
            let callee = lowered
                .module
                .items
                .iter()
                .find_map(|item| match item {
                    HirItem::Function(function) if &function.declaration == declaration => {
                        Some(function)
                    }
                    _ => None,
                })
                .unwrap();
            assert_eq!(callee.var_self_receiver, Some(callee.params[0].id));
            assert_eq!(
                callee.return_ty,
                ResolvedTy::Tuple(vec![ret_ty.clone(), expected.clone()])
            );
        }
        assert!(matches!(
            function_named(&lowered, "read_probe")
                .body
                .tail
                .as_ref()
                .unwrap()
                .kind,
            HirExprKind::Call { .. }
        ));
        assert!(crate::verify_hir(&lowered.module).is_empty());
    }
}

#[test]
fn encoding_spelling_and_opacity_cannot_replace_checked_declaration_authority() {
    let mut ctx = LowerCtx::new(
        &TypeCheckOutput::default(),
        MONOMORPHISATION_REGISTRY_CAP,
        TargetArch::host(),
    );
    for builtin in [BuiltinType::JsonValue, BuiltinType::YamlValue] {
        let name = builtin.canonical_name();
        ctx.type_declarations.insert(
            name.to_string(),
            hew_types::value_class::DeclaredType {
                is_opaque: true,
                ..Default::default()
            },
        );
        ctx.canonical_std_source_type_identities
            .insert(name.to_string());
        let opaque = ResolvedTy::named_opaque_path(&ctx.defs, name, vec![]);
        assert_eq!(ctx.resolve_named_type_ref(name, vec![]), opaque);
        assert_eq!(ctx.qualify_current_module_record_ty(opaque.clone()), opaque);
    }
}

#[test]
fn postfix_try_in_non_result_returning_fn_stays_fail_closed() {
    let source = r"
            fn bad(r: Result<i64, i64>) -> i64 {
                let x: i64 = r?;
                x
            }
        ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(
        !tco.errors.is_empty(),
        "checker must reject `?` in i64-returning fn"
    );
    let lowered = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered.diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::NotYetImplemented { owning_pass, .. }
                if owning_pass == "question-operator"
        )),
        "HIR must fail closed for rejected `?`; got {:#?}",
        lowered.diagnostics
    );
}

// ── Enum-layout discovery tests ──────────────────────────────────────────

/// Lower the §0 probe (`Maybe<i64>` instantiated at a call site and
/// matched) and assert that the HIR enum-layout registry contains exactly
/// the expected entry. The `Some` variant's payload field must be
/// `ResolvedTy::I64` (not `ResolvedTy::named_for_test("T", [])` —
/// the raw type-param symbol). This pins the substitution contract.
///
/// LESSONS: `type-info-survival` (P0) — read type from `expr_types`,
/// not the un-substituted HIR node type. `checker-authority` (P0).
#[test]
fn generic_enum_option_i64_registered_in_enum_layouts() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            enum Maybe<T> { Some(T), None }
            fn main() -> i64 {
                let x: Maybe<i64> = Maybe.Some(42);
                match x {
                    Maybe.Some(v) => v,
                    Maybe.None => 0,
                }
            }
            ",
    );

    let layouts = &lowered.module.enum_layouts;
    assert_eq!(
        layouts.len(),
        1,
        "exactly one enum-layout entry expected for Maybe<i64>; got {layouts:#?}"
    );

    let layout = &layouts[0];
    assert_eq!(
        layout.key.origin_name, "Maybe",
        "enum origin name must be 'Maybe'"
    );
    assert_eq!(
        layout.key.type_args,
        vec![ResolvedTy::I64],
        "type_args must be [I64]"
    );
    assert_eq!(
        layout.mangled_name, "Maybe$$i64",
        "mangled name must follow shared scheme"
    );

    // Two variants: Some(T→i64) and None.
    assert_eq!(layout.variants.len(), 2, "Maybe has two variants");
    let some_variant = layout
        .variants
        .iter()
        .find(|v| v.name == "Some")
        .expect("Some variant must be present");
    assert_eq!(
        some_variant.field_tys,
        vec![ResolvedTy::I64],
        "Some variant payload must be substituted to I64, not a type-param symbol"
    );
    let none_variant = layout
        .variants
        .iter()
        .find(|v| v.name == "None")
        .expect("None variant must be present");
    assert!(
        none_variant.field_tys.is_empty(),
        "None variant must have no payload fields"
    );
}

#[test]
fn authored_generic_local_records_shadow_generic_builtin_spellings() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            type Container<T> { value: T }
            type OutputSink<T> { value: T }

            fn keep_container(value: Container<i64>) -> Container<i64> { value }
            fn keep_sink(value: OutputSink<i64>) -> OutputSink<i64> { value }
            ",
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "generic local records must lower cleanly: {:#?}",
        lowered.diagnostics
    );

    for (function_name, nominal_name) in
        [("keep_container", "Container"), ("keep_sink", "OutputSink")]
    {
        let function = function_named(&lowered, function_name);
        for ty in [&function.params[0].ty, &function.return_ty] {
            assert_eq!(
                ty,
                &ResolvedTy::user_for_test(nominal_name, vec![ResolvedTy::I64]),
                "`{nominal_name}<i64>` authored at the root must remain a user nominal"
            );
        }
    }
}

/// A STDLIB bare `None` — with NO user `enum Option<T>` in source — must
/// lower to a unit ctor whose stamped result type carries the concrete
/// `Option<i64>` args, so codegen's mangled key resolves to `Option$$i64`.
///
/// Note: `enum_layouts` alone does NOT discriminate — the match scrutinee
/// (`f()` typed `Option<i64>`) registers `Option$$i64` regardless. The
/// load-bearing assertion is the `None` ctor node's stamped `ty`.
#[test]
fn stdlib_option_none_registers_in_enum_layouts() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            fn f() -> Option<i64> { .None }
            fn main() -> i64 { match f() { .Some(x) => x, .None => 7 } }
            ",
    );

    // The match scrutinee registers Option$$i64 even on tip; assert it is
    // present (the layout the None ctor must resolve against).
    assert!(
        lowered
            .module
            .enum_layouts
            .iter()
            .any(|l| l.mangled_name == "Option$$i64"),
        "Option$$i64 must be in enum_layouts; got {:#?}",
        lowered.module.enum_layouts
    );

    // Discriminating assertion: the bare `None` ctor in `f`'s body must
    // stamp the concrete Option<i64> type args, not a bare Option.
    let f_func = lowered
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(func) if func.name == "f" => Some(func),
            _ => None,
        })
        .expect("lowered module must contain fn f");
    let tail = f_func
        .body
        .tail
        .as_ref()
        .expect("fn f body must have a trailing `None` expression");
    let HirExprKind::MachineVariantCtor { machine_name, .. } = &tail.kind else {
        panic!(
            "expected `None` to lower to a unit MachineVariantCtor, got {:#?}",
            tail.kind
        );
    };
    assert_eq!(machine_name, "Option", "ctor must target Option");
    match &tail.ty {
        ResolvedTy::Named {
            head: name_head,
            args,
            ..
        } => {
            assert_eq!(name_head.spelling(), "Option");
            assert_eq!(
                args.as_slice(),
                &[ResolvedTy::I64],
                "bare `None` ctor must stamp concrete Option<i64> args (not bare \
                     Option), so codegen's mangled key resolves to Option$$i64; got {:#?}",
                tail.ty
            );
        }
        other => panic!("expected Named Option<i64>, got {other:#?}"),
    }
}

/// A plain monomorphic enum must NOT appear in `enum_layouts` — the
/// registry is only for generic-enum instantiations.
#[test]
fn monomorphic_enum_does_not_appear_in_enum_layouts() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            enum Colour { Red, Green, Blue }
            fn main() -> i64 {
                let c: Colour = Colour.Red;
                match c {
                    Colour.Red => 1,
                    Colour.Green => 2,
                    Colour.Blue => 3,
                }
            }
            ",
    );

    assert!(
        lowered.module.enum_layouts.is_empty(),
        "monomorphic enums must not appear in enum_layouts; got {:#?}",
        lowered.module.enum_layouts
    );
}

/// Nested generic instantiation: `Maybe<Maybe<i64>>` must produce two
/// registry entries — one for `Maybe<Maybe<i64>>` and one for
/// `Maybe<i64>`. The worklist transitively expands type args so that the
/// inner instantiation is discovered even though only the outer type
/// appears at the call site.
///
/// This exercises the fixpoint path in `try_register_enum_instantiation`.
#[test]
fn nested_generic_enum_option_option_i64_registers_both_instantiations() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            enum Maybe<T> { Some(T), None }
            fn main() -> i64 {
                let inner: Maybe<i64> = Maybe.Some(5);
                let outer: Maybe<Maybe<i64>> = Maybe.Some(inner);
                match outer {
                    Maybe.Some(v) => match v {
                        Maybe.Some(n) => n,
                        Maybe.None => 0,
                    },
                    Maybe.None => -1,
                }
            }
            ",
    );

    let layouts = &lowered.module.enum_layouts;
    // Both Maybe<i64> and Maybe<Maybe<i64>> must be registered.
    assert!(
        layouts.len() >= 2,
        "expected at least two enum-layout entries for Maybe<i64> and \
             Maybe<Maybe<i64>>; got {layouts:#?}"
    );

    let has_option_i64 = layouts
        .iter()
        .any(|l| l.key.origin_name == "Maybe" && l.key.type_args == vec![ResolvedTy::I64]);
    let has_option_option_i64 = layouts.iter().any(|l| {
        l.key.origin_name == "Maybe"
            && l.key.type_args == vec![ResolvedTy::named_for_test("Maybe", vec![ResolvedTy::I64])]
    });

    assert!(
        has_option_i64,
        "registry must contain Maybe<i64>; got {layouts:#?}"
    );
    assert!(
        has_option_option_i64,
        "registry must contain Maybe<Maybe<i64>>; got {layouts:#?}"
    );
}

// ── Walker recursion regression tests ────────────────────────────────────
//
// These tests construct HIR nodes directly and call the private walker
// functions.  They guard against a future variant arm accidentally dropping
// recursion — exhaustivity catches a *missing* arm, but not an arm that
// exists yet skips sub-expressions.

/// A lambda actor's `close()` releases the handle and is an ordinary call:
/// it needs no `await` in either position (U383).
#[test]
fn lambda_actor_close_produces_unit_in_value_and_statement_positions() {
    for operation in ["a.close();", "let value: () = a.close();"] {
        let source = format!("fn main() {{ let a = actor |x: i64| {{}}; {operation} }}");
        let (_, checked, lowered) = parse_typecheck_and_lower(&source);
        assert!(checked.errors.is_empty(), "{:?}", checked.errors);
        assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
    }
}

// ─── Imported impl-method signature safety (cross-module lowering) ───

fn named_type(name: &str) -> TypeExpr {
    TypeExpr::Named {
        path: hew_parser::ast::Path::single(hew_parser::ast::Ident::new(name), 0..0),
        type_args: None,
    }
}

/// The literal `Self` receiver of an imported impl method is the impl's
/// own opaque handle — it must be admitted, or every imported impl method
/// with a `self` parameter is skip-listed and any call fails with
/// `CallableUnsupportedInMir`.
#[test]
fn imported_impl_self_receiver_is_admitted() {
    let no_registered_types = |_: &str| false;
    let no_generics = HashSet::new();
    assert!(imported_impl_signature_type_is_safe(
        &named_type("Self"),
        "Result",
        &no_generics,
        &no_registered_types,
    ));
}

/// A signature type naming an UNREGISTERED imported user type must stay
/// rejected even after the `Self` admission: the `Self` fix widens only
/// the receiver, never a sibling parameter the importer cannot resolve.
#[test]
fn imported_impl_unregistered_user_type_stays_rejected() {
    let no_registered_types = |_: &str| false;
    let no_generics = HashSet::new();
    assert!(!imported_impl_signature_type_is_safe(
        &named_type("SomeUnregisteredImportedType"),
        "Result",
        &no_generics,
        &no_registered_types,
    ));
    // The full signature [Self, Unregistered] is unsafe as a whole: the
    // per-type predicate drives an `.any(!safe)` skip in the emit arm.
    let sig = [
        named_type("Self"),
        named_type("SomeUnregisteredImportedType"),
    ];
    assert!(sig.iter().any(|ty| !imported_impl_signature_type_is_safe(
        ty,
        "Result",
        &no_generics,
        &no_registered_types,
    )));
}

/// A generic type parameter in scope on the impl block (`B` on
/// `impl<I, A, B> Iterator for Map<I, A, B>`) is a carrier resolved at
/// monomorphisation time — admit it even though it has no backing
/// declaration, so a generic adapter impl-method (`next -> Option<B>`)
/// lowers as a per-instantiation origin instead of being skip-listed.
#[test]
fn imported_impl_generic_param_is_admitted() {
    let no_registered_types = |_: &str| false;
    let generics: HashSet<String> = ["I", "A", "B"].iter().map(|s| (*s).to_string()).collect();
    // Bare carrier `B`.
    assert!(imported_impl_signature_type_is_safe(
        &named_type("B"),
        "Map",
        &generics,
        &no_registered_types,
    ));
    // `Option<B>` — the actual `next` return shape: composite over a carrier.
    let option_b = TypeExpr::Option(Box::new((named_type("B"), 0..0)));
    assert!(imported_impl_signature_type_is_safe(
        &option_b,
        "Map",
        &generics,
        &no_registered_types,
    ));
    // A carrier NOT in scope is still rejected: admission is gated on the
    // declared param set, not on being a single uppercase letter.
    assert!(!imported_impl_signature_type_is_safe(
        &named_type("C"),
        "Map",
        &generics,
        &no_registered_types,
    ));
}

/// A user record sharing a prelude generic enum's name (`type Result {
/// handle: i64 }`) must not poison the enum-layout registries or the
/// handler return-type resolution: the actor ask still registers the
/// builtin `Result<Result, ActorError>` instantiation and the handler's
/// return type resolves to the USER record (matching the checker), not
/// the builtin enum.
#[test]
fn record_shadowing_builtin_result_keeps_actor_ask_lowerable() {
    let (_program, _tco, lowered) = parse_typecheck_and_lower(
        r#"
            type QueryReply { handle: i64, }

            actor Db {
                var n: i64 = 0,
                receive fn query(sql: string) -> QueryReply {
                    n = n + 1;
                    QueryReply { handle: n }
                }
            }

            fn main() {
                let db = spawn Db(n: 0);
                match db.query("SELECT 1") {
                    .Ok(r) => println(f"handle={r.handle}"),
                    .Err(_) => println("ask failed"),
                }
            }
            "#,
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "shadowed-Result actor ask must lower clean: {:#?}",
        lowered.diagnostics
    );
    // The handler return type follows the checker: the user record.
    let Some(HirItem::Actor(actor)) = lowered
        .module
        .items
        .iter()
        .find(|item| matches!(item, HirItem::Actor(a) if a.name == "Db"))
    else {
        panic!("expected lowered Db actor");
    };
    let handler = actor
        .receive_handlers
        .iter()
        .find(|h| h.name == "query")
        .expect("query handler");
    assert_eq!(
        handler.return_ty,
        ResolvedTy::named_for_test("QueryReply", vec![]),
        "handler return type must resolve to the user record, not the builtin enum"
    );
    // The ask site registered the builtin `Result<Result, ActorError>`
    // layout — the record name must not have clobbered the prelude's
    // `enum_type_params` entry (which would silently no-op registration).
    assert!(
        lowered
            .module
            .enum_layouts
            .iter()
            .any(|layout| layout.key.origin_name == "Result"
                && layout.key.type_args.first()
                    == Some(&ResolvedTy::named_for_test("QueryReply", vec![]))),
        "ask-site Result<Result, ActorError> layout missing from enum_layouts: {:?}",
        lowered
            .module
            .enum_layouts
            .iter()
            .map(|layout| &layout.key)
            .collect::<Vec<_>>()
    );
}

/// A pub enum declared in a non-root (imported) module whose variant name
/// matches a builtin variant name must not be overwritten by the builtin
/// in `machine_ctor_registry`.  Without the local-shadows-global fix that
/// extends `user_declared_variant_names` to non-root pub items, the
/// `builtin_enum_specs` registration pass sees `!user_declared_variant_names
/// .contains("NotFound")` as true and overwrites the user's
/// `AppErr::NotFound` (Tuple kind) entry with `LookupError::NotFound`
/// (Unit kind).  The Tuple-variant call `NotFound(msg)` inside the module
/// body then hits `report_variant_ctor_call_shape_mismatch` and emits a
/// "unit variant called as a function" diagnostic.
#[test]
fn nonroot_pub_enum_variant_shadows_same_named_builtin_in_hir() {
    use hew_parser::module::{Module, ModuleGraph, ModulePath};

    let mod_src = hew_parser::parse(
        r"
            pub enum AppErr { NotFound(string) }

            pub fn make_error(msg: string) -> AppErr {
                .NotFound(msg)
            }
            ",
    );
    assert!(
        mod_src.errors.is_empty(),
        "module parse errors: {:?}",
        mod_src.errors
    );

    let root_id = ModulePath::root();
    let mod_id = ModulePath::new(["errmod"]);
    let module = Module {
        id: mod_id.clone(),
        items: mod_src.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(module).unwrap();
    mg.topo_order = vec![mod_id, root_id];
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&program);
    assert!(
        tco.errors.is_empty(),
        "type errors (should be none): {:?}",
        tco.errors
    );

    let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered.diagnostics.is_empty(),
        "HIR diagnostics must be empty — a 'unit variant NotFound called as a \
             function' diagnostic means the builtin LookupError::NotFound overwrote \
             AppErr::NotFound in machine_ctor_registry: {:#?}",
        lowered.diagnostics
    );
}

#[test]
fn same_leaf_user_enums_keep_user_constructor_identity() {
    let (_, _, lowered) = parse_typecheck_and_lower(
        r"
            enum UserLinkError { UserLink, }
            enum UserLookupError { UserLookup, }
            enum UserMonitorError { UserMonitor, }
            enum UserCrashAction { UserAction, }
            enum UserCrashKind { UserKind, }

            fn user_link() -> UserLinkError { UserLinkError.UserLink }
            fn user_lookup() -> UserLookupError { UserLookupError.UserLookup }
            fn user_monitor() -> UserMonitorError { UserMonitorError.UserMonitor }
            fn user_action() -> UserCrashAction { UserCrashAction.UserAction }
            fn user_kind() -> UserCrashKind { UserCrashKind.UserKind }
            ",
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "same-leaf user enum lowering diagnostics: {:#?}",
        lowered.diagnostics
    );

    for (function_name, expected_type) in [
        ("user_link", "UserLinkError"),
        ("user_lookup", "UserLookupError"),
        ("user_monitor", "UserMonitorError"),
        ("user_action", "UserCrashAction"),
        ("user_kind", "UserCrashKind"),
    ] {
        let function = function_named(&lowered, function_name);
        let tail = function.body.tail.as_deref().expect("constructor tail");
        let HirExprKind::MachineVariantCtor { machine_name, .. } = &tail.kind else {
            panic!("expected enum constructor tail, got {:#?}", tail.kind);
        };
        assert_eq!(machine_name, expected_type);
        assert!(
            matches!(
                &tail.ty,
                ResolvedTy::Named { head: name_head @ (hew_types::TypeHead::Nominal(_) | hew_types::TypeHead::Param(_) | hew_types::TypeHead::Unresolved(_)), .. } if name_head.spelling() == expected_type
            ),
            "{function_name} retained non-user type identity: {:?}",
            tail.ty
        );
    }
}

#[test]
fn canonical_builtin_enum_owners_round_trip_exactly() {
    let output = TypeCheckOutput::default();
    let ctx = LowerCtx::new(&output, MONOMORPHISATION_REGISTRY_CAP, TargetArch::host());
    for expected_type in [
        "std.builtins.LinkError",
        "std.builtins.LookupError",
        "std.link_monitor.MonitorError",
        "std.failure.CrashAction",
        "std.failure.CrashKind",
    ] {
        assert_eq!(
            ctx.canonical_monomorphic_builtin_enum_name(expected_type, None, false),
            Some(expected_type)
        );
    }
}

#[test]
fn unrelated_qualified_same_leaf_is_not_a_builtin_alias() {
    let output = TypeCheckOutput::default();
    let ctx = LowerCtx::new(&output, MONOMORPHISATION_REGISTRY_CAP, TargetArch::host());
    assert_eq!(
        ctx.canonical_monomorphic_builtin_enum_name("std.other.LinkError", None, false),
        None
    );
    assert_eq!(
        ctx.canonical_monomorphic_builtin_enum_name("app.failure.CrashKind", None, false),
        None
    );
    for false_owner in [
        "std.lookup_error.LookupError",
        "std.link_monitor.LinkError",
        "std.link_monitor.CrashKind",
    ] {
        assert_eq!(
            ctx.canonical_monomorphic_builtin_enum_name(false_owner, None, false),
            None,
            "false checker/bootstrap owner must not be accepted: {false_owner}"
        );
    }
}

#[test]
fn named_import_enum_alias_resolves_variant_through_exact_source_owner() {
    use hew_parser::module::{Module, ModuleGraph, ModulePath};

    let source = hew_parser::parse(
        r"
            pub enum Color { Red, Green, Blue(i64), }
            ",
    );
    assert!(
        source.errors.is_empty(),
        "source parse errors: {:?}",
        source.errors
    );
    let mut root = hew_parser::parse(
        r"
            import hew.aliassrc.{ Color as Hue };

            fn color_value(h: Hue) -> i64 {
                match h {
                    Hue.Red => 1,
                    Hue.Green => 2,
                    Hue.Blue(n) => n,
                }
            }

            fn main() {
                let a: Hue = Hue.Red;
                let b: Hue = Hue.Blue(42);
                println(color_value(a));
                println(color_value(b));
            }
            ",
    );
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );
    for (item, _) in &mut root.program.items {
        if let Item::Import(import) = item {
            import.resolved_items = Some(source.program.items.clone().into());
        }
    }

    let root_id = ModulePath::root();
    let source_id = ModulePath::new(["hew", "aliassrc"]);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: source_id.clone(),
            items: source.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add source module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: root.program.items.clone(),
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("add root module");
    graph.topo_order = vec![source_id, root_id];
    let program = Program {
        items: root.program.items,
        module_graph: Some(graph),
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    assert!(
        !tco.warnings
            .iter()
            .any(|warning| { warning.message.contains("unused import: `aliassrc`") }),
        "using `Hue` must credit the originating selective import: {:#?}",
        tco.warnings
    );
    assert_eq!(
        tco.import_type_name_aliases
            .get(&(None, 0, "Hue".to_string()))
            .map(String::as_str),
        Some("hew.aliassrc.Color"),
        "the root alias must carry the exact enum declaration owner"
    );

    let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered.diagnostics.is_empty(),
        "the `Hue` binding must resolve `Hue.Red` and `Hue.Blue` through \
             `hew.aliassrc.Color`: {:#?}",
        lowered.diagnostics
    );
}
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the import-order regression constructs both complete module graphs inline"
)]
fn same_leaf_enum_aliases_keep_their_source_owners_in_both_import_orders() {
    use hew_parser::module::{Module, ModuleGraph, ModulePath};

    // Deliberately disagree on both ordinal and payload shape. A flat
    // `Color::Red` registry key would make one import order diagnose the
    // tuple call as a struct ctor and the other mis-tag the struct ctor.
    let alpha_source = r"
            pub enum Color { AlphaOnly, Red(i64), }
            pub enum Switch { Empty, Shared, }
        ";
    let beta_source = r"
            pub enum Color { Red { value: i64 }, BetaOnly, }
            pub enum Switch { Shared, Full, }
        ";
    let root_with_alpha_first = r"
            import hew.alpha.{ Color as Hue, Switch };
            import hew.beta.{ Color as Shade };

            fn alpha_value(value: Hue) -> i64 {
                match value { Hue.AlphaOnly => 2, Hue.Red(v) => v }
            }
            fn beta_value(value: Shade) -> i64 {
                match value { Shade.Red { value } => value, Shade.BetaOnly => 4 }
            }
            fn main() {
                println(alpha_value(Hue.Red(11)));
                println(beta_value(Shade.Red { value: 22 }));
                let _switch = Switch.Shared;
            }
        ";
    let root_with_beta_first = r"
            import hew.beta.{ Color as Shade, Switch };
            import hew.alpha.{ Color as Hue };

            fn alpha_value(value: Hue) -> i64 {
                match value { Hue.AlphaOnly => 2, Hue.Red(v) => v }
            }
            fn beta_value(value: Shade) -> i64 {
                match value { Shade.Red { value } => value, Shade.BetaOnly => 4 }
            }
            fn main() {
                println(alpha_value(Hue.Red(11)));
                println(beta_value(Shade.Red { value: 22 }));
                let _switch = Switch.Shared;
            }
        ";

    for (root_source, alpha_first_in_topo) in
        [(root_with_alpha_first, true), (root_with_beta_first, false)]
    {
        let alpha = hew_parser::parse(alpha_source);
        let beta = hew_parser::parse(beta_source);
        let mut root = hew_parser::parse(root_source);
        assert!(
            alpha.errors.is_empty(),
            "alpha parse errors: {:?}",
            alpha.errors
        );
        assert!(
            beta.errors.is_empty(),
            "beta parse errors: {:?}",
            beta.errors
        );
        assert!(
            root.errors.is_empty(),
            "root parse errors: {:?}",
            root.errors
        );
        for (item, _) in &mut root.program.items {
            let Item::Import(import) = item else {
                continue;
            };
            import.resolved_items = Some(
                match import.path.to_string().as_str() {
                    "hew.alpha" => alpha.program.items.clone(),
                    "hew.beta" => beta.program.items.clone(),
                    path => panic!("unexpected import path: {path:?}"),
                }
                .into(),
            );
        }

        let root_id = ModulePath::root();
        let alpha_id = ModulePath::new(["hew", "alpha"]);
        let beta_id = ModulePath::new(["hew", "beta"]);
        let mut graph = ModuleGraph::new(root_id.clone());
        graph
            .add_module(Module {
                id: alpha_id.clone(),
                items: alpha.program.items,
                imports: vec![],
                source_paths: vec![],
                doc: None,
            })
            .expect("add alpha module");
        graph
            .add_module(Module {
                id: beta_id.clone(),
                items: beta.program.items,
                imports: vec![],
                source_paths: vec![],
                doc: None,
            })
            .expect("add beta module");
        graph
            .add_module(Module {
                id: root_id.clone(),
                items: root.program.items.clone(),
                imports: vec![],
                source_paths: vec![],
                doc: None,
            })
            .expect("add root module");
        graph.topo_order = if alpha_first_in_topo {
            vec![alpha_id, beta_id, root_id]
        } else {
            vec![beta_id, alpha_id, root_id]
        };
        let program = Program {
            items: root.program.items,
            module_graph: Some(graph),
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);
        assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
        assert_eq!(
            tco.import_type_name_aliases
                .get(&(None, 0, "Hue".to_string()))
                .map(String::as_str),
            Some("hew.alpha.Color")
        );
        assert_eq!(
            tco.import_type_name_aliases
                .get(&(None, 0, "Shade".to_string()))
                .map(String::as_str),
            Some("hew.beta.Color")
        );

        let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());
        assert!(
            lowered.diagnostics.is_empty(),
            "same-leaf alias resolution must be owner-stable regardless of \
                 declaration order: {:#?}",
            lowered.diagnostics
        );
        let dump = crate::dump_hir(&lowered.module);
        assert!(
            dump.contains("machine-variant-ctor hew.alpha.Color[1]"),
            "tuple constructor must retain alpha's exact owner and ordinal: {dump}"
        );
        assert!(
            dump.contains("machine-variant-ctor hew.beta.Color[0]"),
            "struct constructor must retain beta's exact owner and ordinal: {dump}"
        );
        assert!(
            !dump.contains("machine-variant-ctor Color["),
            "imported constructors must never fall back to a legacy short owner: {dump}"
        );
        let expected_machine_ctor = if alpha_first_in_topo {
            "machine-variant-ctor hew.alpha.Switch[1]"
        } else {
            "machine-variant-ctor hew.beta.Switch[0]"
        };
        assert!(
            dump.contains(expected_machine_ctor),
            "machine state must retain the selectively imported exact owner and ordinal: {dump}"
        );
        assert!(
            !dump.contains("machine-variant-ctor Switch["),
            "imported machine states must never use a legacy short owner: {dump}"
        );
    }
}

/// Same as `nonroot_pub_enum_variant_shadows_same_named_builtin_in_hir`
/// but with a PRIVATE enum.  The checker's `pre_register_type_decl`
/// (registration.rs:1359) inserts bare variant `fn_sigs` for ALL non-root
/// `TypeDecl`s regardless of visibility, so a program where a private enum
/// uses a builtin-named variant is accepted by the checker.  HIR must
/// match: the bare form of the variant constructor in the module body
/// must resolve to the user's private enum, not to the builtin unit
/// variant.
#[test]
fn nonroot_private_enum_variant_shadows_same_named_builtin_in_hir() {
    use hew_parser::module::{Module, ModuleGraph, ModulePath};

    let mod_src = hew_parser::parse(
        r"
            enum AppErr { NotFound(string) }

            pub fn make_error(msg: string) -> AppErr {
                .NotFound(msg)
            }
            ",
    );
    assert!(
        mod_src.errors.is_empty(),
        "module parse errors: {:?}",
        mod_src.errors
    );

    let root_id = ModulePath::root();
    let mod_id = ModulePath::new(["errmod"]);
    let module = Module {
        id: mod_id.clone(),
        items: mod_src.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(module).unwrap();
    mg.topo_order = vec![mod_id, root_id];
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&program);
    assert!(
        tco.errors.is_empty(),
        "type errors (should be none): {:?}",
        tco.errors
    );

    let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered.diagnostics.is_empty(),
        "HIR diagnostics must be empty — a 'unit variant NotFound called as a \
             function' diagnostic means the builtin LookupError::NotFound overwrote \
             the private AppErr::NotFound in machine_ctor_registry: {:#?}",
        lowered.diagnostics
    );
}

/// Builds a tiny module `{mod_name}` declaring `pub actor Conn { receive
/// fn ping() -> i64 { <ping_result> } }` plus a sibling `pub actor
/// {holder_name} { let conn: Conn, ... }` whose state field references
/// `Conn` by bare name — the shape `canonicalize_actor_ref_field_ty`
/// scopes to the declaring module. The `get` handler deliberately does
/// NOT `await`/`match` on `conn`: that ask-reply inference path is
/// unrelated to this fix and, independent of
/// `canonicalize_actor_ref_field_ty`, does not yet resolve correctly
/// across two modules that both declare an actor with the same
/// receive-fn name in this raw multi-module harness (a separate,
/// pre-existing checker-layer gap). This isolates exactly what
/// `canonicalize_actor_ref_field_ty` decides: the lowered type of the
/// `conn` state field itself.
fn conn_holder_module(
    mod_name: &str,
    holder_name: &str,
    ping_result: i64,
) -> hew_parser::module::Module {
    let source = format!(
        "pub actor Conn {{ receive fn ping() -> i64 {{ {ping_result} }} }}\n\
             pub actor {holder_name} {{ let conn: Conn, receive fn get() -> i64 {{ 0 }} }}\n"
    );
    let parsed = hew_parser::parse(&source);
    assert!(
        parsed.errors.is_empty(),
        "module `{mod_name}` parse errors: {:?}",
        parsed.errors
    );
    hew_parser::module::Module {
        id: hew_parser::module::ModulePath::new([mod_name.to_string()]),
        items: parsed.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    }
}

/// The lowered `conn` state field type of the named actor, or panics.
fn holder_conn_field_ty(lowered: &LowerOutput, holder_name: &str) -> ResolvedTy {
    let Some(HirItem::Actor(actor)) = lowered
        .module
        .items
        .iter()
        .find(|item| matches!(item, HirItem::Actor(a) if a.name == holder_name))
    else {
        panic!("expected lowered {holder_name} actor");
    };
    actor
        .state_fields
        .iter()
        .find(|f| f.name == "conn")
        .expect("conn field")
        .ty
        .clone()
}

/// The canonical actor-handle shape an actor field resolves to: the actor's
/// own qualified name carrying the handle discriminator.
fn localpid_of(defs: &hew_types::DefTable, qualified_actor_name: &str) -> ResolvedTy {
    ResolvedTy::named_actor_path(defs, qualified_actor_name, Vec::new())
}

/// Regression for the ambiguous-short-name case: two DIFFERENT modules
/// each declare their own `pub actor Conn` and each has a sibling actor
/// with a state field that references `Conn` by bare name. Before the
/// fix, `canonicalize_actor_ref_field_ty`'s global short-name sweep over
/// `actor_type_names` found two candidates for bare `Conn` and
/// canonicalized NEITHER, leaving both fields fail-closed at MIR.
/// Scoping resolution to `{decl_module}.{name}` means each module's bare
/// `Conn` resolves to that SAME module's `Conn` — independently, with no
/// cross-module ambiguity, because the lookup never leaves the module
/// being lowered.
///
/// This exercises HIR lowering directly, bypassing `hew build`/`hew
/// run`'s parsed-`import`-statement visibility gate — a separate,
/// pre-existing checker-layer limitation unrelated to this fix: a plain
/// `import pkg;` does not publish `pkg`'s types unqualified, and that
/// gate's "not in scope" diagnostic fires on any caller of a module
/// whose actor has a same-module bare-actor-typed field, regardless of
/// whether the caller ever names the colliding type.
#[test]
fn same_short_name_actors_in_different_modules_canonicalize_independently() {
    use hew_parser::module::{ModuleGraph, ModulePath};

    let modules = [
        conn_holder_module("a", "HolderA", 1),
        conn_holder_module("b", "HolderB", 2),
    ];
    let module_ids: Vec<ModulePath> = modules.iter().map(|m| m.id.clone()).collect();
    let root_id = ModulePath::root();
    let mut mg = ModuleGraph::new(root_id.clone());
    for module in modules {
        mg.add_module(module).unwrap();
    }
    mg.topo_order = module_ids.into_iter().chain([root_id]).collect();
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&program);
    assert!(
        tco.errors.is_empty(),
        "type errors (should be none): {:?}",
        tco.errors
    );

    let lowered = lower_program(&program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        lowered.diagnostics.is_empty(),
        "HIR diagnostics must be empty: {:#?}",
        lowered.diagnostics
    );

    for (mod_name, holder_name) in [("a", "HolderA"), ("b", "HolderB")] {
        assert_eq!(
            holder_conn_field_ty(&lowered, holder_name),
            localpid_of(&tco.defs, &format!("{mod_name}.Conn")),
            "module {mod_name}'s bare `Conn` field must canonicalize to \
                 {mod_name}.Conn, never the OTHER module's same-named actor \
                 or an unresolved bare name"
        );
    }
}

/// A variant-bearing `#[resource]` `#[opaque]` declaration has no
/// single-representation lifecycle boundary to admit. Every sibling
/// rejection in `admit_declared_opaque_resource_lifecycles`'s filter
/// emits a `CheckerBoundaryViolation`; this pins that the variants case
/// does the same instead of falling out of the iterator silently.
///
/// The surface parser rejects `#[opaque] enum` outright (`#[opaque]`
/// requires an empty-body `type`), so this shape is unreachable through
/// ordinary source syntax — the HIR item is hand-built here to exercise
/// the defence-in-depth diagnostic directly.
#[test]
fn opaque_resource_with_variants_emits_checker_boundary_violation() {
    use crate::HirNodeId;
    let decl = HirTypeDecl {
        kind: HirTypeDeclKind::Enum,
        id: ItemId(0),
        node: HirNodeId(0),
        declaration: hew_types::DefId::for_test("app.Handle"),
        name: "Handle".to_string(),
        defining_module: None,
        marker: ResourceMarker::Resource,
        is_opaque: true,
        is_indirect: false,
        consuming_methods: Vec::new(),
        type_params: Vec::new(),
        fields: Vec::new(),
        variants: vec![HirVariant {
            name: "A".to_string(),
            kind: HirVariantKind::Unit,
        }],
        span: 0..0,
    };
    let items = vec![HirItem::TypeDecl(decl)];
    let graph = hew_types::OpaqueResourceCandidateGraph::default();
    let mut type_classes = crate::value_class::TypeClassTable::new();
    let mut diagnostics = Vec::new();
    admit_declared_opaque_resource_lifecycles(
        &items,
        &graph,
        &hew_types::DefTable::fixture(),
        &mut type_classes,
        &mut diagnostics,
    );

    let violation = diagnostics.iter().find(|d| {
            matches!(&d.kind, HirDiagnosticKind::CheckerBoundaryViolation { name, .. } if name == "app.Handle")
        });
    assert!(
        violation.is_some(),
        "variant-bearing opaque resource must emit CheckerBoundaryViolation; \
             diagnostics: {diagnostics:?}"
    );
    let HirDiagnosticKind::CheckerBoundaryViolation { reason, .. } = &violation.unwrap().kind
    else {
        unreachable!()
    };
    assert!(
        reason.contains("variants have no single-representation lifecycle boundary to admit"),
        "reason must name the variants-lifecycle-boundary gap; got: {reason:?}"
    );
}
