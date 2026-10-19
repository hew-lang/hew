use hew_hir::{collect_scope_extents, collect_site_spans, HirModule};
use hew_types::{module_registry::ModuleRegistry, Checker};

use crate::support::checker_pipeline::{parse_source, program_with_imported_module};

const MACHINE: &str = r"pub machine Divider {
    events { Divide; }
    state Ready { divisor: i64; }
    state Done { value: i64; }
    on Divide: Ready => Done { value: 10 / state.divisor }
    default { state }
}
";

const IMPORTED_MACHINE: &str = r"pub machine Toggle {
    events { Flip; }
    state Off;
    state On;
    on Flip: Off => On;
    on Flip: On => Off;
}
";

fn lower(program: &hew_parser::ast::Program) -> HirModule {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let checked = checker.check_program(program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let output = hew_hir::lower_program_host_target(program, &checked, &hew_hir::ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    output.module
}

#[test]
fn machine_failure_sites_and_scopes_name_authored_source() {
    let module = lower(&parse_source(MACHINE).program);
    let division = "10 / state.divisor";
    let start = MACHINE.find(division).expect("division source");
    let sites = collect_site_spans(&module);
    assert!(
        sites.values().any(|source| {
            source.source_module.is_none()
                && source.span.start == start
                && MACHINE
                    .get(source.span.clone())
                    .is_some_and(|written| written.trim_end() == division)
        }),
        "the potentially failing divide must name its authored expression"
    );
    assert!(sites
        .values()
        .filter(|site| site.source_module.is_none())
        .all(|source| source.span.end <= MACHINE.len()));
    let scopes = collect_scope_extents(&module);
    assert!(!scopes.is_empty());
    assert!(scopes
        .values()
        .filter(|scope| scope.source_module.is_none())
        .all(|scope| scope.span.end <= MACHINE.len()));
    assert!(
        module
            .checked_sites
            .values()
            .any(|site| site.end > MACHINE.len()),
        "semantic keys must retain generated identities"
    );
}

#[test]
fn machine_source_attribution_is_independent_of_unrelated_module_formatting() {
    let root = format!("import m;\n{MACHINE}");
    let origins = |padding: &str| {
        let imported = format!("{padding}pub fn unrelated() -> i64 {{ 7 }}");
        let module = lower(&program_with_imported_module(&imported, &root));
        let mut sites: Vec<_> = collect_site_spans(&module)
            .into_values()
            .filter(|source| source.source_module.is_none())
            .map(|source| (source.span.start, source.span.end))
            .collect();
        sites.sort_unstable();
        let mut scopes: Vec<_> = collect_scope_extents(&module)
            .into_values()
            .filter(|scope| scope.source_module.is_none())
            .map(|scope| (scope.span.start, scope.span.end))
            .collect();
        scopes.sort_unstable();
        (sites, scopes)
    };
    assert_eq!(origins(""), origins(&"\n".repeat(4096)));
}

#[test]
fn imported_machine_source_attribution_keeps_its_module() {
    let root = "fn main() {}";
    let module = lower(&program_with_imported_module(IMPORTED_MACHINE, root));
    let sites = collect_site_spans(&module);
    let imported: Vec<_> = sites
        .values()
        .filter(|source| source.source_module.as_deref() == Some("m"))
        .collect();
    assert!(!imported.is_empty());
    assert!(imported
        .iter()
        .all(|source| source.span.end <= IMPORTED_MACHINE.len()));
    assert!(sites
        .values()
        .filter(|source| source.source_module.is_none())
        .all(|source| source.span.end <= root.len()));
    let scopes = collect_scope_extents(&module);
    assert!(scopes
        .values()
        .any(|scope| scope.source_module.as_deref() == Some("m")));
    assert!(scopes
        .values()
        .filter(|scope| scope.source_module.as_deref() == Some("m"))
        .all(|scope| scope.span.end <= IMPORTED_MACHINE.len()));
}
