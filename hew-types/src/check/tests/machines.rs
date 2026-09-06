use super::*;

fn checked_machine(body: &str, helper: &str) -> TypeCheckOutput {
    let source = format!(
        "{helper}\n machine Gate {{ events {{ Open, }} emits {{ Changed {{ label: string }}, }} state Closed {{ label: string }}, state Opened {{ label: string }}, on Open: Closed => Opened {{ {body} .Opened {{ label: self.label }} }} default {{ state }} }} fn main() {{ var gate: Gate = .Closed {{ label: \"start\" }}; let _report = gate.step(.Open); }}"
    );
    let parsed = hew_parser::parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program)
}

#[test]
fn machine_normalizes_owning_values_and_checked_staged_calls() {
    let output = checked_machine(
        "emit Changed { label: upper(self.label) };",
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
        ("println(self.label);", ""),
        ("announce(self.label);", "fn announce(value: string) { println(value); }"),
        ("announce(self.label);", "fn announce(value: string) { hidden(value); } fn hidden(value: string) { println(value); }"),
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
