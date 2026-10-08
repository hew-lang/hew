use super::*;

#[test]
fn machine_event_type_is_an_owned_member() {
    let source = "machine Tank {\n    events {\n        Tick;\n    }\n    state Idle;\n    on Tick: Idle => Idle;\n}\n\nfn feed(event: Tank.Event) -> i64 {\n    1\n}\n";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let machine = output
        .defs
        .lookup_path("Tank")
        .expect("machine declaration");
    let event = output
        .defs
        .member_of_kind(
            machine,
            hew_parser::ast::sym::EVENT,
            crate::DeclarationKind::MachineEventType,
        )
        .expect("machine-owned event type");
    assert_eq!(output.defs.owner(event), Some(machine));
    assert_eq!(output.defs.path(event), "Tank.Event");
    assert!(output.defs.lookup_path("TankEvent").is_none());
    let start = source.find("Tank.Event").unwrap();
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(start..start + 4), 0)),
        Some(&crate::check::scope::Resolution::Nominal(
            crate::NominalId::from_minted_declaration(machine)
        ))
    );
    assert_eq!(
        output
            .resolutions
            .get(&SpanKey::in_module(&(start + 5..start + 10), 0)),
        Some(&crate::check::scope::Resolution::Nominal(
            crate::NominalId::from_minted_declaration(event)
        ))
    );
}

#[test]
fn generated_machine_parameters_keep_distinct_binding_spans() {
    let source = "machine First {\n    events {\n        Go;\n    }\n    state Idle;\n    on Go: Idle => Idle;\n}\n\nmachine Second {\n    events {\n        Go;\n    }\n    state Idle;\n    on Go: Idle => Idle;\n}\n";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let normalized = output.normalized_machines.as_ref().expect("normalization");
    let mut bindings = std::collections::HashSet::new();
    let mut generated = 0;
    for (item, _) in &normalized.program.items {
        let Item::Impl(implementation) = item else {
            continue;
        };
        for method in &implementation.methods {
            if !matches!(
                method.origin,
                hew_parser::ast::DeclarationOrigin::MachineStep
                    | hew_parser::ast::DeclarationOrigin::MachineCompanion
            ) {
                continue;
            }
            generated += 1;
            for param in &method.params {
                let binder = SpanKey::in_module(&param.name_span, 0);
                let resolution = output
                    .resolutions
                    .get(&binder)
                    .expect("generated parameter binder row");
                assert!(matches!(
                    resolution,
                    crate::check::scope::Resolution::Local(_)
                ));
                assert!(bindings.insert(resolution), "generated binder collision");
                assert!(
                    output.resolutions.iter().any(|(use_span, use_resolution)| {
                        *use_span != binder
                            && use_resolution == resolution
                            && normalized
                                .source_spans
                                .contains_key(&(use_span.start..use_span.end))
                    }),
                    "generated parameter use did not join its binder: {param:?}"
                );
            }
        }
    }
    assert_eq!(generated, 4, "two machines each generate two methods");
    assert_eq!(
        bindings.len(),
        6,
        "each generated parameter has one identity"
    );
}

fn checked_machine(body: &str, helper: &str) -> TypeCheckOutput {
    let source = format!(
        "{helper}\n machine Gate {{ events {{ Open; }} emits {{ Changed {{ label: string; }} }} state Closed {{ label: string; }} state Opened {{ label: string; }} on Open: Closed => Opened {{ {body} .Opened {{ label: state.label }} }} default {{ state }} }} fn main() {{ var gate: Gate = .Closed {{ label: \"start\" }}; let _report = gate.step(.Open); }}"
    );
    let parsed = hew_parser::parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program)
}

#[test]
fn machine_normalizes_owning_values_and_checked_staged_calls() {
    let output = checked_machine(
        "emit Changed { label: upper(state.label) };",
        "fn upper(value: string) -> string { value.to_upper() }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output
        .normalized_machines
        .as_ref()
        .unwrap()
        .program
        .items
        .iter()
        .all(|(item, _)| !matches!(item, Item::Machine(_))));
    assert!(output.method_call_rewrites.values().any(|rewrite| matches!(
        rewrite,
        MethodCallRewrite::RewriteToFunction {
            receiver_update: ReceiverUpdate::Staged,
            ..
        }
    )));
}

#[test]
fn machine_rejects_direct_and_transitive_effects() {
    for (body, helper) in [
        ("println(state.label);", ""),
        ("announce(state.label);", "fn announce(value: string) { println(value); }"),
        ("announce(state.label);", "fn announce(value: string) { hidden(value); } fn hidden(value: string) { println(value); }"),
    ] {
        let output = checked_machine(body, helper);
        assert!(output.errors.iter().any(|error| error.message.contains("not demonstrably pure")), "effects must be rejected: {:?}", output.errors);
    }
}

#[test]
fn machine_requires_guard_fallback_and_target_payload() {
    for source in [
        "machine Gate {\n    events {\n        Open;\n    }\n    state Closed;\n    state Opened;\n    on Open: Closed => Opened when true;\n    on Open: Opened => Opened;\n}\n\nfn main() {}\n",
        "machine Gate {\n    events {\n        Open;\n    }\n    state Closed;\n    state Opened;\n    on Open: Closed => Opened {\n        .Closed\n    }\n    default { state }\n}\n\nfn main() {}\n",
        "machine Gate {\n    events {\n        Open;\n    }\n    state Closed;\n    state Opened { label: string; }\n    on Open: Closed => Opened { label: 3 }\n    default { state }\n}\n\nfn main() {}\n",
    ] {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
        assert!(!output.errors.is_empty(), "invalid machine was admitted: {source}");
    }
}

#[test]
fn machine_step_report_uses_normal_must_use_diagnostic() {
    let source = "machine Gate {\n    events {\n        Open;\n    }\n    state Closed;\n    state Opened;\n    on Open: Closed => Opened;\n    default { state }\n}\n\nfn main() {\n    var gate: Gate = .Closed;\n    gate.step(.Open);\n}\n";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|warning| warning.message.contains("machine step report")),
        "{:?}",
        output.warnings
    );
}

#[test]
fn ordinary_machine_preserves_original_declaration_occurrence() {
    let parsed = hew_parser::parse("machine Gate {\n    events {\n        Open;\n    }\n    state Closed;\n    state Opened;\n    default { state }\n}\n\npub fn after() -> i64 {\n    7\n}\n\nfn main() {}\n");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let machine_span = parsed.program.items[0].1.clone();
    let function_span = parsed.program.items[1].1.clone();
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    for (span, ordinal, kind) in [
        (machine_span, 0, crate::DeclarationKind::Machine),
        (function_span, 1, crate::DeclarationKind::Function),
    ] {
        let occurrence = crate::DeclarationOccurrence::new_with_synthetic_ordinal(
            output.defs.root_module(),
            &span,
            ordinal,
            kind,
            0,
        );
        assert!(
            output.defs.declaration(occurrence).is_some(),
            "authored occurrence lost: {occurrence:?}"
        );
    }
}

/// A shared import DAG must normalize in time proportional to its size. The
/// root-module identity check once compared item lists structurally, which
/// recursed into every import's resolved body and visited a diamond chain's
/// shared bodies once per path, so a chain of sixty modules never finished.
#[test]
fn normalize_walks_a_shared_import_dag_once() {
    use std::sync::Arc;

    use super::super::machine_normalize;
    use hew_parser::ast::Item;

    fn import_item(name: &str, body: &Arc<Vec<Spanned<Item>>>) -> Spanned<Item> {
        let parsed = hew_parser::parse(&format!("import {name};"));
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let (item, span) = parsed.program.items.into_iter().next().expect("one import");
        let Item::Import(mut decl) = item else {
            panic!("expected an import item");
        };
        decl.resolved_items = Some(Arc::clone(body));
        (Item::Import(decl), span)
    }

    let machine = hew_parser::parse(
        "machine Gate {\n    events {\n        Open;\n    }\n    state Closed;\n    state Opened;\n    on Open: Closed => Opened;\n    default { state }\n}\n",
    );
    assert!(machine.errors.is_empty(), "{:?}", machine.errors);

    // bodies[i] imports bodies[i - 1] and bodies[i - 2]: a diamond chain whose
    // path count grows like the Fibonacci numbers.
    let mut bodies: Vec<Arc<Vec<Spanned<Item>>>> = vec![
        Arc::new(machine.program.items.clone()),
        Arc::new(machine.program.items.clone()),
    ];
    for depth in 2..60 {
        let items = vec![
            import_item(&format!("m{}", depth - 1), &bodies[depth - 1]),
            import_item(&format!("m{}", depth - 2), &bodies[depth - 2]),
        ];
        bodies.push(Arc::new(items));
    }
    let mut root_items = vec![
        import_item("m59", &bodies[59]),
        import_item("m58", &bodies[58]),
    ];
    root_items.extend(machine.program.items.clone());

    let root_id = ModulePath::root();
    let root_module = Module {
        id: root_id.clone(),
        items: root_items.clone(),
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module).unwrap();
    mg.topo_order = vec![root_id];
    let program = Program {
        module_graph: Some(mg),
        items: root_items,
        module_doc: None,
    };

    let normalized = machine_normalize::normalize(&program)
        .expect("normalization succeeds")
        .expect("a machine is present");
    let root = &normalized.program.module_graph.as_ref().unwrap().modules[&ModulePath::root()];
    assert_eq!(
        root.items.len(),
        normalized.program.items.len(),
        "the root module carries the program's normalized items"
    );
}

/// After a transition head a lone `{ n }` is the block yielding `n`; the
/// refusal sits on that body and shows the payload spelling. Writing the
/// payload is accepted.
#[test]
fn lone_shorthand_transition_body_names_the_payload_spelling() {
    let source = "machine Counter {\n    events {\n        Go;\n    }\n    state Idle { n: i64; }\n    state Busy { n: i64; }\n    on Go: Idle => Busy { n }\n    on Go: Busy => Idle { n: 0 }\n}\n";
    let check = |source: &str| {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
        Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program)
    };
    let output = check(source);
    let refusal = output
        .errors
        .iter()
        .find(|error| error.message.contains("must produce that state"))
        .expect("the block body produces no state");
    assert_eq!(refusal.span.start, source.find("{ n }").unwrap());
    assert_eq!(
        refusal.suggestions,
        vec![
            "the braces were read as a block yielding `n`; to set the field, write `{ n: n }`"
                .to_string()
        ]
    );
    let output = check(&source.replace("Busy { n }", "Busy { n: state.n }"));
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}
