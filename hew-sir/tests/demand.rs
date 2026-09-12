//! Body lowering is demand-driven from the module's entry callable, and the
//! dump reports every declaration that failed to lower.

use std::fmt::Write as _;

use hew_hir::{lower_program_host_target, HirItem, HirModule, ResolutionCtx};
use hew_sir::{
    dump_lowering, lower_module, lower_module_with_demand, lower_module_with_roots, verify_module,
    LoweredModule, SirLoweringDemand, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker, DefId};

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

fn callable_paths(lowered: &LoweredModule) -> Vec<&str> {
    lowered
        .module
        .callables
        .iter()
        .map(|callable| callable.declaration.full_path())
        .collect()
}

fn status_of<'a>(lowered: &'a LoweredModule, name: &str) -> &'a SirLoweringStatus {
    lowered
        .statuses
        .iter()
        .find_map(|source| (source.name == name).then_some(&source.status))
        .unwrap_or_else(|| panic!("source must declare `{name}`"))
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
        .unwrap_or_else(|| panic!("HIR module must declare `{name}`"))
}

/// Deliberately break one body after checking. Demand tests must not depend on
/// a valid language feature remaining unimplemented.
fn invalid_bodies(source: &str, names: &[&str]) -> (HirModule, hew_types::TypeCheckOutput) {
    let (mut hir, facts) = lower_hir(source);
    for item in &mut hir.items {
        let HirItem::Function(function) = item else {
            continue;
        };
        if names.contains(&function.name.as_str()) {
            let tail = function.body.tail.as_mut().expect("fixture tail");
            tail.kind = hew_hir::HirExprKind::BindingRef {
                name: "missing".into(),
                resolved: hew_hir::ResolvedRef::Binding(hew_hir::BindingId(u32::MAX)),
            };
        }
    }
    (hir, facts)
}

/// A malformed header names a type the checked module never declared.
fn invalidate_header(hir: &mut HirModule, name: &str) {
    let function = hir
        .items
        .iter_mut()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => Some(function),
            _ => None,
        })
        .expect("fixture declares the requested header");
    function.params[0].ty =
        hew_types::ResolvedTy::named_user("UnregisteredDemandType".to_string(), Vec::new());
}

fn lower_invalid_bodies(source: &str, names: &[&str]) -> LoweredModule {
    let (hir, facts) = invalid_bodies(source, names);
    lower_module(&hir, &facts)
}

#[test]
fn an_unreachable_unsupported_function_does_not_block_the_reachable_component() {
    let lowered = lower_invalid_bodies(
        r"
        fn reachable(value: i64) -> i64 {
            value + 1
        }

        fn stranded(value: i64) -> i64 {
            value
        }

        fn main() -> i64 {
            reachable(41)
        }
        ",
        &["stranded"],
    );

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
    let lowered = lower_invalid_bodies(
        r"
        fn stranded(value: i64) -> i64 {
            value
        }

        fn main() -> i64 {
            stranded(41)
        }
        ",
        &["stranded"],
    );

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
            "        fn helper{index}(value: i64) -> i64 {{\n            value\n        }}\n"
        )
        .expect("write to String");
    }
    let calls = (0..7)
        .map(|index| format!("helper{index}({index})"))
        .collect::<Vec<_>>()
        .join(" + ");
    let names = (0..7)
        .map(|index| format!("helper{index}"))
        .collect::<Vec<_>>();
    let lowered = lower_invalid_bodies(
        &format!("{helpers}\n        fn main() -> i64 {{\n            {calls}\n        }}\n"),
        &names.iter().map(String::as_str).collect::<Vec<_>>(),
    );

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
        dump.contains("fn __hew_fn_main("),
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
    assert!(dump.contains("fn __hew_fn_add_one("));
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

#[test]
fn an_explicit_root_lowers_only_its_resolved_library_call_closure() {
    let (hir, type_facts) = invalid_bodies(
        r"
        fn helper(value: i64) -> i64 {
            value + 1
        }

        fn library_root() -> i64 {
            helper(41)
        }

        fn stranded(value: i64) -> i64 {
            value
        }
        ",
        &["stranded"],
    );
    let root = declaration_of(&hir, "library_root");

    let lowered = lower_module_with_roots(&hir, &type_facts, std::slice::from_ref(&root))
        .expect("a monomorphic library root must be selectable by exact declaration identity");

    for name in ["library_root", "helper"] {
        assert!(
            matches!(status_of(&lowered, name), SirLoweringStatus::Lowered),
            "the selected root closure must lower `{name}`: {:#?}",
            lowered.statuses
        );
    }
    assert!(
        matches!(
            status_of(&lowered, "stranded"),
            SirLoweringStatus::NotReached
        ),
        "explicit roots must not turn unrelated declarations into demand: {:#?}",
        lowered.statuses
    );
    assert!(verify_module(&lowered.module).is_empty());
}

#[test]
fn explicit_roots_are_unioned_with_the_resolved_entry() {
    let (hir, type_facts) = lower_hir(
        r"
        fn published() -> i64 {
            7
        }

        fn stranded() -> i64 {
            9
        }

        fn main() -> i64 {
            0
        }
        ",
    );
    let published = declaration_of(&hir, "published");

    let lowered = lower_module_with_roots(&hir, &type_facts, &[published])
        .expect("an exact monomorphic root must be admitted alongside entry");

    for name in ["main", "published"] {
        assert!(matches!(
            status_of(&lowered, name),
            SirLoweringStatus::Lowered
        ));
    }
    assert!(matches!(
        status_of(&lowered, "stranded"),
        SirLoweringStatus::NotReached
    ));
}

#[test]
fn explicit_root_refusals_name_each_requested_declaration() {
    let (mut hir, type_facts) = lower_hir(
        r"
        fn generic<T>(value: T) -> T {
            value
        }

        fn refused(value: i64) -> i64 {
            0
        }

        fn vanished() -> i64 {
            0
        }
        ",
    );
    invalidate_header(&mut hir, "refused");
    let generic = declaration_of(&hir, "generic");
    let refused = declaration_of(&hir, "refused");
    let vanished = declaration_of(&hir, "vanished");
    hir.items.retain(
        |item| !matches!(item, HirItem::Function(function) if function.declaration == vanished),
    );

    let errors = lower_module_with_roots(
        &hir,
        &type_facts,
        &[vanished.clone(), refused.clone(), generic.clone()],
    )
    .expect_err("generic, ineligible, and absent declarations must fail closed as roots");

    assert_eq!(
        errors.len(),
        3,
        "each refused identity needs its own reason"
    );
    let generic_error = errors
        .iter()
        .find(|error| error.declaration == generic)
        .expect("generic root refusal must retain its declaration");
    assert!(generic_error.to_string().contains("concrete"));
    let refused_error = errors
        .iter()
        .find(|error| error.declaration == refused)
        .expect("ineligible root refusal must retain its declaration");
    assert!(refused_error.to_string().contains("UnregisteredDemandType"));
    let missing_error = errors
        .iter()
        .find(|error| error.declaration == vanished)
        .expect("missing root refusal must retain its declaration");
    assert!(missing_error.to_string().contains("not present"));
}

/// Every-callable demand is the coverage question: it lowers bodies the entry
/// never reaches and names the refusal for a header the table would not
/// admit. Entry demand over the same source is the control: it must keep
/// reporting all three as unreached, because nothing about the compile route
/// moved.
#[test]
fn every_callable_demand_lowers_stranded_bodies_and_names_refused_headers() {
    let source = r"
        fn stranded_ok(value: i64) -> i64 {
            value + 1
        }

        fn stranded_bad(value: i64) -> i64 {
            value
        }

        fn refused_header(value: i64) -> i64 {
            0
        }

        fn main() -> i64 {
            0
        }
        ";
    let (mut hir, type_facts) = invalid_bodies(source, &["stranded_bad"]);
    invalidate_header(&mut hir, "refused_header");

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
        reason.contains("UnregisteredDemandType"),
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

/// Header admission follows the program, not the prelude's declaration list:
/// `std.builtins` lowers into every module, so an uncalled `NodeConfig::at`
/// would otherwise put its record shape, its `Vec<string>` glue and their
/// `string` rows into every inventory.
#[test]
fn prelude_callables_are_admitted_only_where_the_program_calls_them() {
    let uninterested = lower_source(
        r"
        fn double(n: i64) -> i64 { n * 2 }
        fn main() -> i64 { double(21) }
        ",
    );
    let paths = callable_paths(&uninterested);
    for absent in ["NodeConfig", "duration", "ActorRequestOwner"] {
        assert!(
            paths.iter().all(|path| !path.contains(absent)),
            "a program that calls no prelude declaration must carry none: `{absent}` in {paths:?}"
        );
    }

    let interested = lower_source(
        r#"
        fn main() {
            let config = NodeConfig.at("127.0.0.1:9000");
            println(config.bind);
        }
        "#,
    );
    let paths = callable_paths(&interested);
    assert!(
        paths.iter().any(|path| path.contains("NodeConfig")),
        "the prelude declaration the program calls must be admitted: {paths:?}"
    );
    assert!(
        paths.iter().all(|path| !path.contains("duration")),
        "its uncalled siblings must still stay out: {paths:?}"
    );
}

#[test]
fn a_demanded_header_publishes_nested_shapes_and_an_unreached_one_publishes_none() {
    let (hir, facts) = lower_hir(
        r#"
        type Payload { text: string }
        type Unused { flag: bool }
        fn unrelated<Payload>(value: Payload) -> Payload { value }
        fn stranded(value: Result<Option<Option<Payload>>, string>) {
            defer { println("selected"); }
        }
        fn uncalled(value: Unused) -> Unused { value }
        fn main() {}
        "#,
    );
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    // `stranded` is selected as an export root, so its header is demanded
    // while nothing calls it. Its body never reads the parameter, so every
    // shape below can only have come from admitting the signature.
    let lowered = lower_module_with_roots(&hir, &facts, &[declaration_of(&hir, "stranded")])
        .expect("an exact monomorphic declaration is selectable as a root");
    assert!(matches!(
        status_of(&lowered, "stranded"),
        SirLoweringStatus::Lowered
    ));
    assert!(matches!(
        status_of(&lowered, "main"),
        SirLoweringStatus::Lowered
    ));
    let callable = lowered
        .module
        .callables
        .iter()
        .find(|callable| callable.declaration == declaration_of(&hir, "stranded"))
        .expect("the selected root must have a header");
    let result = lowered
        .module
        .variant_shape_for_type(&callable.signature.params[0].ty)
        .expect("the header result must have a shape");
    let outer = lowered
        .module
        .variant_shape_for_type(&result.variants[0].fields[0].ty)
        .expect("the nested outer Option must have a shape");
    let inner = lowered
        .module
        .variant_shape_for_type(&outer.variants[0].fields[0].ty)
        .expect("the nested inner Option must have a shape");
    let payload = lowered
        .module
        .aggregate_shape_for_type(&inner.variants[0].fields[0].ty)
        .expect("the nested record payload must have a shape");
    assert_eq!(payload.fields[0].ty, hew_types::ResolvedTy::String);
    // Negative control: nothing demands `uncalled`, so it costs the module no
    // header and its parameter type costs it no shape.
    assert!(matches!(
        status_of(&lowered, "uncalled"),
        SirLoweringStatus::NotReached
    ));
    assert!(lowered
        .module
        .callables
        .iter()
        .all(|callable| callable.declaration != declaration_of(&hir, "uncalled")));
    assert!(lowered.module.aggregate_shapes.iter().all(|shape| shape
        .instance
        .nominal
        .display_name()
        != "Unused"));
    assert!(
        verify_module(&lowered.module).is_empty(),
        "{:#?}",
        verify_module(&lowered.module)
    );
}
