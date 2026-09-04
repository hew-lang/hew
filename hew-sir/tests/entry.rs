//! Entry selection is a join on HIR's resolved entry declaration.
//!
//! These tests move the fact away from the declaration spelled `main` and
//! prove SIR follows the fact, then remove the fact entirely and prove SIR
//! does not fall back to a name.

use hew_hir::{lower_program_host_target, HirItem, HirModule, ResolutionCtx};
use hew_sir::{lower_module, verify_module};
use hew_types::{module_registry::ModuleRegistry, Checker, DefId};

fn lower_hir(source: &str) -> (HirModule, hew_types::TypeCheckOutput) {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "source must parse before the SIR entry test: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let type_check_output = checker.check_program(&parsed.program);
    let hir = lower_program_host_target(&parsed.program, &type_check_output, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "source must lower to HIR before the SIR entry test: {:#?}",
        hir.diagnostics
    );
    (hir.module, type_check_output)
}

fn declaration_of(module: &HirModule, name: &str) -> DefId {
    module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => {
                Some(function.declaration.clone())
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("source must define `{name}`"))
}

const TWO_ROOT_FUNCTIONS: &str = r"
    fn start() -> i64 {
        7
    }

    fn main() -> i64 {
        0
    }
    ";

#[test]
fn hir_publishes_the_root_entry_declaration_once() {
    let (hir, _type_facts) = lower_hir(TWO_ROOT_FUNCTIONS);
    assert_eq!(
        hir.entry_declaration.as_ref(),
        Some(&declaration_of(&hir, "main")),
        "HIR applies the language entry rule and publishes the resolved declaration"
    );
}

#[test]
fn an_entry_fact_naming_a_non_main_declaration_selects_and_lowers_that_callable() {
    let (mut hir, type_facts) = lower_hir(TWO_ROOT_FUNCTIONS);
    hir.entry_declaration = Some(declaration_of(&hir, "start"));

    let lowered = lower_module(&hir, &type_facts);
    let entry = lowered
        .module
        .entry_callable
        .expect("the published entry declaration must select a callable");
    assert_eq!(
        lowered
            .module
            .callable(entry)
            .map(|callable| callable.symbol.as_str()),
        Some("start"),
        "entry selection must follow the HIR fact, not the `main` spelling"
    );
    assert!(
        lowered.module.function_index().function(entry).is_some(),
        "the selected entry must reach a lowered SIR body"
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "an entry that is not spelled `main` must still verify: {:#?}",
        verify_module(&lowered.module)
    );
}

/// Negative control for the test above: with the fact left where HIR put it,
/// the very same source selects `main`. The selection therefore tracks the
/// fact and nothing else.
#[test]
fn the_unmodified_entry_fact_still_selects_main() {
    let lowered = {
        let (hir, type_facts) = lower_hir(TWO_ROOT_FUNCTIONS);
        lower_module(&hir, &type_facts)
    };
    let entry = lowered
        .module
        .entry_callable
        .expect("the root `main` declaration must select a callable");
    assert_eq!(
        lowered
            .module
            .callable(entry)
            .map(|callable| callable.symbol.as_str()),
        Some("main")
    );
}

/// Fail-closed control: a module with no entry fact has no entry callable even
/// though a root declaration spelled `main` is right there in the table.
#[test]
fn removing_the_entry_fact_leaves_no_entry_callable_to_rediscover_by_name() {
    let (mut hir, type_facts) = lower_hir(TWO_ROOT_FUNCTIONS);
    hir.entry_declaration = None;

    let lowered = lower_module(&hir, &type_facts);
    assert!(
        lowered
            .module
            .callables
            .iter()
            .any(|callable| callable.symbol == "main"),
        "the fixture must still contain a callable whose symbol is `main`"
    );
    assert_eq!(
        lowered.module.entry_callable, None,
        "without the HIR fact SIR must not rediscover an entry by symbol"
    );
}

/// A fact that points outside the root unit is admitted by lowering and
/// rejected by the verifier, rather than being silently dropped into `None`.
#[test]
fn an_entry_fact_naming_a_non_root_declaration_is_rejected_by_the_verifier() {
    let (mut hir, type_facts) = lower_hir(TWO_ROOT_FUNCTIONS);
    let start = declaration_of(&hir, "start");
    let start_item = hir
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.declaration == start => Some(function.id),
            _ => None,
        })
        .expect("source must define `start`");
    hir.root_item_ids.remove(&start_item);
    hir.entry_declaration = Some(start);

    let lowered = lower_module(&hir, &type_facts);
    assert!(
        lowered.module.entry_callable.is_some(),
        "a non-root entry fact must reach the verifier, not vanish"
    );
    let diagnostics = verify_module(&lowered.module);
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            hew_sir::SirDiagnosticKind::InvalidEntryCallable { .. }
        )),
        "a non-root entry callable must be rejected: {diagnostics:#?}"
    );
}
