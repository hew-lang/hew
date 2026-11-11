//! Flat postfix chains parse, check, lower to HIR and SIR, and verify on a
//! 1 MiB thread, including chains rewritten inside a machine transition.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{lower_module, verify_module, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// [`lowers_and_verifies`] on a thread with `stack` bytes of stack.
fn lowers_and_verifies_on(stack: usize, source: &str, call: &str) {
    let source = format!("{source}\nfn main() {{ println({call}); }}\n");
    std::thread::Builder::new()
        .stack_size(stack)
        .spawn(move || {
            let parsed = hew_parser::parse(&source);
            assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
            let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
            let types = checker.check_program(&parsed.program);
            assert!(types.errors.is_empty(), "{:#?}", types.errors);
            let hir = lower_program_host_target(&parsed.program, &types, &ResolutionCtx);
            assert!(hir.diagnostics.is_empty(), "{:#?}", hir.diagnostics);
            let module = lower_module(&hir.module, &types);
            assert!(
                module
                    .statuses
                    .iter()
                    .filter(|source| source.name == "f" || source.name == "main")
                    .all(|source| matches!(source.status, SirLoweringStatus::Lowered)),
                "{:#?}",
                module.statuses
            );
            let violations = verify_module(&module.module);
            assert!(violations.is_empty(), "{violations:?}");
        })
        .expect("spawn")
        .join()
        .expect("a long flat shape overflowed or failed to lower");
}

/// Links in each long postfix chain: deep enough that any walk recursing once
/// per link overflows a 1 MiB stack.
const LINKS: usize = 1_024;

/// The stack each postfix chain verifies on.
const SMALL_STACK: usize = 1024 * 1024;

/// A record indexed by `i64`, each index yielding `Output`.
fn grid(output: &str, get: &str, at: &str) -> String {
    format!(
        "type Grid {{ v: i64; }}\n\
         impl Index<i64> for Grid {{\n\
         type Output = {output};\n\
         fn get(self, index: i64) -> Option<{output}> {{ {get} }}\n\
         fn at(self, index: i64) -> {output} {{ {at} }}\n\
         }}"
    )
}

#[test]
fn a_long_method_chain_lowers_on_a_small_stack() {
    let links = ".trim()".repeat(LINKS);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!("fn f(s: string) -> string {{ s{links} }}"),
        "f(\" s \")",
    );
}

#[test]
fn long_user_method_chains_preserve_owned_receivers_on_a_small_stack() {
    let links = ".keep().take()".repeat(LINKS / 2);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!(
            "type Cell {{ value: string; }}\n\
             impl Cell {{\n\
             fn keep(self) -> Cell {{ self }}\n\
             fn take(consume self) -> Cell {{ self }}\n\
             }}\n\
             fn f() -> string {{ Cell {{ value: \"kept\" }}{links}.value }}"
        ),
        "f()",
    );
}

#[test]
fn receiver_temporaries_remain_owned_across_argument_and_callee_failures() {
    for receiver in ["self", "consume self"] {
        for argument in ["fail()", "\"suffix\""] {
            lowers_and_verifies_on(
                SMALL_STACK,
                &format!(
                    "type Cell {{ value: string; }}\n\
                     impl Cell {{\n\
                     fn keep(self) -> Cell {{ self }}\n\
                     fn append({receiver}, suffix: string) -> Cell {{\n\
                         if suffix == \"suffix\" {{ panic(\"callee failed\") }}\n\
                         Cell {{ value: self.value + suffix }}\n\
                     }}\n\
                     }}\n\
                     fn fail() -> string {{ panic(\"argument failed\") }}\n\
                     fn f() -> string {{ Cell {{ value: \"owned\" }}.keep().keep().append({argument}).value }}"
                ),
                "f()",
            );
        }
    }
}

#[test]
fn a_long_index_chain_lowers_on_a_small_stack() {
    let links = "[0]".repeat(LINKS);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!(
            "{}\nfn f(g: Grid) -> i64 {{ g{links}.v }}",
            grid("Grid", ".Some(self)", "self")
        ),
        "f(Grid { v: 1 })",
    );
}

#[test]
fn a_long_field_chain_lowers_on_a_small_stack() {
    // A record cannot hold itself, so each field link reaches the next record
    // through a vector index.
    let links = ".kids[0]".repeat(LINKS / 2);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!(
            "type Node {{ kids: Vec<Node>; v: i64; }}\n\
             fn f(n: Node) -> i64 {{ n{links}.v }}\n\
             fn leaf() -> Node {{ Node {{ kids: Vec.new(), v: 1 }} }}"
        ),
        "f(leaf())",
    );
}

#[test]
fn a_long_try_chain_lowers_on_a_small_stack() {
    let links = "[0]?".repeat(LINKS / 2);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!(
            "{}\nfn f(g: Grid) -> Option<i64> {{ let last = g{links}; .Some(last.v) }}",
            grid("Option<Grid>", ".Some(.Some(self))", ".Some(self)")
        ),
        "f(Grid { v: 1 }) ?? 0",
    );
}

#[test]
fn a_long_cast_chain_lowers_on_a_small_stack() {
    let links = " as i64".repeat(LINKS);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!("fn f(x: i64) -> i64 {{ x{links} }}"),
        "f(1)",
    );
}

#[test]
fn a_long_optional_chain_lowers_on_a_small_stack() {
    let links = "?.s()".repeat(LINKS / 2);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!(
            "type N {{ v: i64; }}\n\
             impl N {{ fn s(self) -> Option<N> {{ .Some(self) }} }}\n\
             fn f(o: Option<N>) -> i64 {{ let last = o{links}; (last ?? N {{ v: 0 }}).v }}"
        ),
        "f(.Some(N { v: 1 }))",
    );
}

#[test]
fn a_long_coalesce_chain_lowers_on_a_small_stack() {
    let links = " ?? x".repeat(LINKS / 2);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!("fn f(x: Option<i64>) -> i64 {{ x{links} ?? 0 }}"),
        "f(.None)",
    );
}

#[test]
fn a_machine_transition_rewrites_a_postfix_chain_on_a_small_stack() {
    let links = ".trim()".repeat(256);
    lowers_and_verifies_on(
        SMALL_STACK,
        &format!(
            r#"machine Quiet {{
            events {{ Begin; }}
            state Idle;
            state Active {{ label: string; }}
            on Begin: Idle => Active {{ label: "quiet"{links} }}
            default {{ state }}
        }}
        fn f() -> i64 {{ 1 }}"#
        ),
        "f()",
    );
}
