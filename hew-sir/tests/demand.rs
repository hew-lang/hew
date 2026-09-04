//! Body lowering is demand-driven from the module's entry callable, and the
//! dump reports every declaration that failed to lower.

use std::fmt::Write as _;

use hew_hir::{lower_program_host_target, HirModule, ResolutionCtx};
use hew_sir::{
    dump_lowering, lower_module, lower_module_with_demand, verify_module, LoweredModule,
    SirLoweringDemand, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_hir(source: &str) -> (HirModule, hew_types::TypeCheckOutput) {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "source must parse before the SIR demand test: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let type_check_output = checker.check_program(&parsed.program);
    let hir = lower_program_host_target(&parsed.program, &type_check_output, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "source must lower to HIR before the SIR demand test: {:#?}",
        hir.diagnostics
    );
    (hir.module, type_check_output)
}

fn lower_source(source: &str) -> LoweredModule {
    {
        let (hir, type_facts) = lower_hir(source);
        lower_module(&hir, &type_facts)
    }
}

fn status_of<'a>(lowered: &'a LoweredModule, name: &str) -> &'a SirLoweringStatus {
    lowered
        .statuses
        .iter()
        .find_map(|source| (source.name == name).then_some(&source.status))
        .unwrap_or_else(|| panic!("source must declare `{name}`"))
}

/// `var` bindings are outside the initial SIR surface, which makes them a
/// stable way to write a body that cannot lower.
const UNSUPPORTED_BODY: &str = "var accumulator = value; accumulator";

#[test]
fn an_unreachable_unsupported_function_does_not_block_the_reachable_component() {
    let lowered = lower_source(&format!(
        r"
        fn reachable(value: i64) -> i64 {{
            value + 1
        }}

        fn stranded(value: i64) -> i64 {{
            {UNSUPPORTED_BODY}
        }}

        fn main() -> i64 {{
            reachable(41)
        }}
        "
    ));

    assert!(
        matches!(status_of(&lowered, "main"), SirLoweringStatus::Lowered),
        "the entry must lower: {:#?}",
        lowered.statuses
    );
    assert!(
        matches!(status_of(&lowered, "reachable"), SirLoweringStatus::Lowered),
        "the reachable callee must lower: {:#?}",
        lowered.statuses
    );
    assert!(
        matches!(
            status_of(&lowered, "stranded"),
            SirLoweringStatus::NotReached
        ),
        "an unreachable body must not even be attempted: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "the reachable component must verify: {:#?}",
        verify_module(&lowered.module)
    );
    assert_eq!(
        lowered.module.functions.len(),
        2,
        "only the entry and its reachable callee should have bodies: {:#?}",
        lowered
            .module
            .functions
            .iter()
            .map(|function| function.name.as_str())
            .collect::<Vec<_>>()
    );
}

/// Counterfactual for the test above: the same `stranded` body, now called
/// from the entry, really is outside the surface. Without this the
/// `NotReached` assertion could pass for a body that lowers perfectly well.
#[test]
fn the_same_body_reached_from_the_entry_is_reported_unsupported() {
    let lowered = lower_source(&format!(
        r"
        fn stranded(value: i64) -> i64 {{
            {UNSUPPORTED_BODY}
        }}

        fn main() -> i64 {{
            stranded(41)
        }}
        "
    ));

    assert!(
        matches!(
            status_of(&lowered, "stranded"),
            SirLoweringStatus::Unsupported { .. }
        ),
        "a demanded body outside the surface must report why: {:#?}",
        lowered.statuses
    );
}

#[test]
fn the_dump_reports_every_unsupported_body_with_a_reason() {
    // Seven is deliberately past the six-entry detail limit the CLI used to
    // truncate at, so a regression to summarising would drop at least one.
    let mut helpers = String::new();
    for index in 0..7 {
        write!(
            helpers,
            "        fn helper{index}(value: i64) -> i64 {{\n            {UNSUPPORTED_BODY}\n        }}\n"
        )
        .expect("write to String");
    }
    let calls = (0..7)
        .map(|index| format!("helper{index}({index})"))
        .collect::<Vec<_>>()
        .join(" + ");
    let lowered = lower_source(&format!(
        "{helpers}\n        fn main() -> i64 {{\n            {calls}\n        }}\n"
    ));

    let dump = dump_lowering(&lowered);
    for index in 0..7 {
        let stanza = format!("; fn helper{index}\n; unsupported: ");
        assert!(
            dump.contains(&stanza),
            "every unsupported body must appear with its reason; `helper{index}` is missing from:\n{dump}"
        );
    }
    assert_eq!(
        dump.matches("; unsupported: ").count(),
        7,
        "the dump must report each failure exactly once:\n{dump}"
    );
    assert!(
        dump.contains("fn main("),
        "the dump must still carry the IR it could lower:\n{dump}"
    );
}

/// Negative control: a program whose bodies all lower produces no stanzas, so
/// the marker the test above looks for is not simply always present.
#[test]
fn a_fully_lowered_program_dumps_no_unsupported_stanza() {
    let lowered = lower_source(
        r"
        fn add_one(value: i64) -> i64 {
            value + 1
        }

        fn main() -> i64 {
            add_one(41)
        }
        ",
    );

    let dump = dump_lowering(&lowered);
    assert!(
        !dump.contains("; unsupported: "),
        "a fully lowered program must report no failure:\n{dump}"
    );
    assert!(dump.contains("fn add_one("));
}

/// A module with no entry is not a program: it demands nothing, and the dump
/// says so rather than looking like an empty compilation.
#[test]
fn a_module_without_an_entry_lowers_no_bodies_and_says_why() {
    let (mut hir, type_facts) = lower_hir(
        r"
        fn add_one(value: i64) -> i64 {
            value + 1
        }

        fn main() -> i64 {
            add_one(41)
        }
        ",
    );
    hir.entry_exit_plan = None;

    let lowered = lower_module(&hir, &type_facts);
    assert!(
        lowered.module.functions.is_empty(),
        "no entry means no demand: {:#?}",
        lowered
            .module
            .functions
            .iter()
            .map(|function| function.name.as_str())
            .collect::<Vec<_>>()
    );
    assert!(dump_lowering(&lowered).contains("; no entry callable"));
}

/// Every-callable demand is the coverage question: it lowers bodies the entry
/// never reaches and names the refusal for a header the table would not
/// admit. Entry demand over the same source is the control: it must keep
/// reporting all three as unreached, because nothing about the compile route
/// moved.
#[test]
fn every_callable_demand_lowers_stranded_bodies_and_names_refused_headers() {
    let source = format!(
        r"
        fn stranded_ok(value: i64) -> i64 {{
            value + 1
        }}

        fn stranded_bad(value: i64) -> i64 {{
            {UNSUPPORTED_BODY}
        }}

        fn refused_header(text: string) -> i64 {{
            text.len()
        }}

        fn main() -> i64 {{
            0
        }}
        "
    );
    let (hir, type_facts) = lower_hir(&source);

    let entry = lower_module(&hir, &type_facts);
    for name in ["stranded_ok", "stranded_bad", "refused_header"] {
        assert!(
            matches!(status_of(&entry, name), SirLoweringStatus::NotReached),
            "entry demand must leave `{name}` unreached: {:#?}",
            entry.statuses
        );
    }

    let every = lower_module_with_demand(&hir, &type_facts, SirLoweringDemand::EveryCallable);
    assert!(
        matches!(status_of(&every, "main"), SirLoweringStatus::Lowered),
        "{:#?}",
        every.statuses
    );
    assert!(
        matches!(status_of(&every, "stranded_ok"), SirLoweringStatus::Lowered),
        "an unreached but admissible body must be lowered on demand: {:#?}",
        every.statuses
    );
    assert!(
        matches!(
            status_of(&every, "stranded_bad"),
            SirLoweringStatus::Unsupported { .. }
        ),
        "an unreached body outside the surface must report why: {:#?}",
        every.statuses
    );
    let SirLoweringStatus::Unsupported { reason } = status_of(&every, "refused_header") else {
        panic!(
            "a refused header must surface its refusal under every-callable demand: {:#?}",
            every.statuses
        );
    };
    assert!(
        reason.contains("string"),
        "the refusal must name the offending parameter type: {reason}"
    );
    let stranded = &every
        .statuses
        .iter()
        .find(|status| status.name == "stranded_ok")
        .expect("stranded_ok is declared")
        .declaration;
    assert!(
        matches!(
            every.status_for_declaration(stranded),
            Some(SirLoweringStatus::Lowered)
        ),
        "statuses must be addressable by declaration identity"
    );
}
