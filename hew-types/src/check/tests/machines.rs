use super::*;

fn checked_machine(body: &str, helper: &str) -> TypeCheckOutput {
    let source = format!(
        "{helper}\n machine Gate {{ events {{ Open, }} emits {{ Changed {{ label: string }}, }} state Closed {{ label: string }}, state Opened {{ label: string }}, on Open: Closed => Opened {{ {body} .Opened {{ label: state.label }} }} default {{ state }} }} fn main() {{ var gate: Gate = .Closed {{ label: \"start\" }}; let _report = gate.step(.Open); }}"
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
        "machine Gate { events { Open, } state Closed, state Opened, on Open: Closed => Opened when true { .Opened } on Open: Opened => Opened { .Opened } } fn main() {}",
        "machine Gate { events { Open, } state Closed, state Opened, on Open: Closed => Opened { .Closed } default { state } } fn main() {}",
        "machine Gate { events { Open, } state Closed, state Opened { label: string }, on Open: Closed => Opened { .Opened { label: 3 } } default { state } } fn main() {}",
    ] {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
        assert!(!output.errors.is_empty(), "invalid machine was admitted: {source}");
    }
}

#[test]
fn machine_step_report_uses_normal_must_use_diagnostic() {
    let source = "machine Gate { events { Open, } state Closed, state Opened, on Open: Closed => Opened { .Opened } default { state } } fn main() { var gate: Gate = .Closed; gate.step(.Open); }";
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
    let parsed = hew_parser::parse("machine Gate { events { Open, } state Closed, state Opened, default { state } } pub fn after() -> i64 { 7 } fn main() {}");
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
            output.identity.root_module(),
            &span,
            ordinal,
            kind,
            0,
        );
        assert!(
            output.identity.declaration(occurrence).is_some(),
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
        "machine Gate { events { Open, } state Closed, state Opened, on Open: Closed => Opened { .Opened } default { state } }",
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

    let root_id = ModuleId::root();
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
    let root = &normalized.program.module_graph.as_ref().unwrap().modules[&ModuleId::root()];
    assert_eq!(
        root.items.len(),
        normalized.program.items.len(),
        "the root module carries the program's normalized items"
    );
}
