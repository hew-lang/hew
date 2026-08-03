//! Produced-value authority coverage for call scrutinees.
//!
//! These are the 33 scenarios formerly pinned to the retired
//! `__hew_call_scrutinee` preflight. Each case now names the exact source call,
//! asserts its completed HIR ownership fact, and checks the successor MIR
//! boundary: non-owned facts cannot mint a generic typed-publication owner,
//! while `Unknown` fails closed at an ownership-demanding sink.

use std::collections::HashMap;
use std::ops::Deref;

use hew_hir::verify::complete_produced_value_facts;
use hew_hir::{
    collect_site_spans, lower_program, HirProducedValueFact, HirProducedValueProducer,
    HirSiteSource, ResolutionCtx, SiteId,
};
use hew_mir::{lower_hir_module, IrPipeline, MirDiagnosticKind};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{
    Checker, ProducedValueAcquisition as Acquisition, ProducedValueOwnership as Ownership,
};

struct AuthorityPipeline {
    mir: IrPipeline,
    source: String,
    facts: HashMap<SiteId, HirProducedValueFact>,
    spans: HashMap<SiteId, HirSiteSource>,
}

impl Deref for AuthorityPipeline {
    type Target = IrPipeline;

    fn deref(&self) -> &Self::Target {
        &self.mir
    }
}

fn pipeline(source: &str) -> AuthorityPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let facts = complete_produced_value_facts(&output.module);
    let spans = collect_site_spans(&output.module);
    let mir = lower_hir_module(&output.module);
    AuthorityPipeline {
        mir,
        source: source.to_string(),
        facts,
        spans,
    }
}

fn sites_at(p: &AuthorityPipeline, expression: &str) -> Vec<SiteId> {
    let mut sites: Vec<_> = p
        .spans
        .iter()
        .filter_map(|(site, source)| {
            let is_call = p.facts.get(site).is_some_and(|fact| {
                matches!(
                    fact.producer,
                    HirProducedValueProducer::Call
                        | HirProducedValueProducer::CallDynMethod
                        | HirProducedValueProducer::CallTraitMethodStatic
                        | HirProducedValueProducer::VarSelfMethodCall
                        | HirProducedValueProducer::ResolvedImplCall
                )
            });
            (is_call
                && p.source
                    .get(source.span.clone())
                    .is_some_and(|text| text.trim() == expression))
            .then_some(*site)
        })
        .collect();
    sites.sort_unstable();
    assert_eq!(
        sites.len(),
        1,
        "expected one HIR site for `{expression}`, found {sites:?}"
    );
    sites
}

fn ownership_at(p: &AuthorityPipeline, expression: &str) -> Ownership {
    let site = sites_at(p, expression)[0];
    p.facts
        .get(&site)
        .unwrap_or_else(|| panic!("missing produced-value fact for `{expression}` at {site}"))
        .ownership
}

fn produced_owner_mints_at(p: &AuthorityPipeline, expression: &str) -> usize {
    let sites = sites_at(p, expression);
    p.raw_mir
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.statements)
        .filter(|statement| {
            matches!(
                statement,
                hew_mir::MirStatement::Bind { name, site, .. }
                    if name == "__hew_produced_value" && sites.contains(site)
            )
        })
        .count()
}

fn assert_authority(p: &AuthorityPipeline, expression: &str, expected: Ownership) {
    assert_eq!(
        ownership_at(p, expression),
        expected,
        "wrong completed ownership at `{expression}`"
    );
    if !matches!(expected, Ownership::Owned { .. }) {
        assert_eq!(
            produced_owner_mints_at(p, expression),
            0,
            "a non-owned produced-value fact must not mint a typed owner at `{expression}`"
        );
    }
}

fn assert_owned(p: &AuthorityPipeline, expression: &str, acquisition: Acquisition) {
    assert_authority(p, expression, Ownership::owned(acquisition));
}

fn assert_resolved_capture_call(p: &AuthorityPipeline, expression: &str) {
    let ownership = ownership_at(p, expression);
    assert!(
        matches!(ownership, Ownership::Owned { .. } | Ownership::Borrowed),
        "captured forwarder call must be resolved, got {ownership:?} at `{expression}`"
    );
    if matches!(ownership, Ownership::Borrowed) {
        assert_eq!(produced_owner_mints_at(p, expression), 0);
    }
}

fn diagnostic_count(p: &IrPipeline, construct_fragment: &str) -> usize {
    p.diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                &diagnostic.kind,
                MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct.contains(construct_fragment)
            )
        })
        .count()
}

fn unresolved_ownership_count(p: &IrPipeline) -> usize {
    diagnostic_count(p, "call-scrutinee ownership is unresolved")
}

fn captured_move_count(p: &IrPipeline) -> usize {
    diagnostic_count(p, "whole-value move of captured generator/closure value")
}

fn foreign_transfer_count(p: &IrPipeline) -> usize {
    diagnostic_count(
        p,
        "ownership transfer of a proven-foreign value into a callee-owned parameter",
    )
}

fn payload_move_reject_count(p: &IrPipeline) -> usize {
    p.diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                diagnostic.kind,
                MirDiagnosticKind::ProjectedPayloadMoveFromReadablePlace { .. }
            )
        })
        .count()
}

fn assert_clean(p: &IrPipeline) {
    assert!(
        p.diagnostics.is_empty(),
        "expected clean MIR lowering, got {:#?}",
        p.diagnostics
    );
}

const FORWARDER: &str = r"
    fn passthru(x: Result<string, string>) -> Result<string, string> { x }
";

const PARSER: &str = r"
    fn wrap(s: string) -> Result<string, string> { Ok(s) }
";

#[test]
fn forwarder_borrow_only_match_publishes_borrowed_fact() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) -> i64 {{
            match passthru(r) {{ Ok(_) => 1, Err(_) => 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_authority(&p, "passthru(r)", Ownership::Borrowed);
    assert_clean(&p);
}

#[test]
fn forwarder_while_let_publishes_borrowed_fact() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) {{
            while let Ok(_v) = passthru(r) {{ break; }}
         }}"
    );
    let p = pipeline(&src);
    assert_authority(&p, "passthru(r)", Ownership::Borrowed);
    assert_clean(&p);
}

#[test]
fn forwarder_let_else_publishes_borrowed_fact() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) -> i64 {{
            let Ok(_v) = passthru(r) else {{ return 0 }};
            1
         }}"
    );
    let p = pipeline(&src);
    assert_authority(&p, "passthru(r)", Ownership::Borrowed);
    assert_clean(&p);
}

#[test]
fn forwarder_if_let_publishes_borrowed_fact() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) -> i64 {{
            if let Ok(_v) = passthru(r) {{ 1 }} else {{ 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_authority(&p, "passthru(r)", Ownership::Borrowed);
    assert_clean(&p);
}

#[test]
fn forwarder_discarded_statement_publishes_borrowed_fact() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) {{
            passthru(r);
         }}"
    );
    let p = pipeline(&src);
    assert_authority(&p, "passthru(r)", Ownership::Borrowed);
    assert_clean(&p);
}

#[test]
fn forwarder_over_fresh_ctor_preserves_nonfresh_outer_authority() {
    let src = format!(
        "{FORWARDER}
         fn fresh_ctor() -> Result<string, string> {{ Ok(\"x\") }}
         fn use_it() -> i64 {{
            match passthru(fresh_ctor()) {{ Ok(_) => 1, Err(_) => 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_owned(&p, "fresh_ctor()", Acquisition::Fresh);
    let outer = ownership_at(&p, "passthru(fresh_ctor())");
    assert!(
        matches!(
            outer,
            Ownership::Borrowed
                | Ownership::Owned {
                    acquisition: Acquisition::Retained
                }
        ),
        "a forwarder must not relabel the outer call as a fresh allocation: {outer:?}"
    );
    assert_clean(&p);
}

#[test]
fn params_only_inline_literal_arg_publishes_fresh_fact() {
    let src = format!(
        "{PARSER}
         fn use_it() -> i64 {{
            match wrap(\"hello\") {{ Ok(_) => 1, Err(_) => 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_owned(&p, "wrap(\"hello\")", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn params_only_let_bound_local_arg_publishes_fresh_fact() {
    let src = format!(
        "{PARSER}
         fn use_it() -> i64 {{
            let v = \"1.2.3\";
            match wrap(v) {{ Ok(_) => 1, Err(_) => 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_owned(&p, "wrap(v)", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn params_only_mixed_args_publish_checker_fresh_fact() {
    let src = r#"
        type Holder { b: string; }
        fn wrap2(a: string, b: string) -> Result<string, string> { Ok(a) }
        fn use_it(h: Holder) -> i64 {
            match wrap2("lit", h.b) { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_owned(&p, "wrap2(\"lit\", h.b)", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn params_only_caller_param_publishes_fresh_aggregate_fact() {
    let src = format!(
        "{PARSER}
         fn use_it(s: string) -> i64 {{
            match wrap(s) {{ Ok(_) => 1, Err(_) => 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_owned(&p, "wrap(s)", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn unknown_extern_arg_stays_unknown_beneath_fresh_wrapper() {
    let src = format!(
        "{PARSER}
         extern \"C\" {{
            fn ext_make() -> string;
         }}
         fn use_it() -> i64 {{
            match wrap(ext_make()) {{ Ok(_) => 1, Err(_) => 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_authority(&p, "ext_make()", Ownership::Unknown);
    assert_owned(&p, "wrap(ext_make())", Acquisition::Fresh);
    assert_eq!(foreign_transfer_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn params_only_aliased_local_arg_publishes_fresh_fact() {
    let src = format!(
        "{PARSER}
         fn use_it() -> i64 {{
            let v = \"x\";
            let w = v;
            match wrap(w) {{ Ok(_) => 1, Err(_) => 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_owned(&p, "wrap(w)", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn params_only_reread_local_arg_publishes_fresh_fact() {
    let src = format!(
        "{PARSER}
         fn take(s: string) -> i64 {{ 1 }}
         fn use_it() -> i64 {{
            let v = \"x\";
            let n = match wrap(v) {{ Ok(_) => 1, Err(_) => 0 }};
            n + take(v)
         }}"
    );
    let p = pipeline(&src);
    assert_owned(&p, "wrap(v)", Acquisition::Fresh);
    assert_authority(&p, "take(v)", Ownership::NoOwner);
    assert_clean(&p);
}

#[test]
fn params_only_pattern_binder_arg_publishes_fresh_fact() {
    let src = format!(
        "{PARSER}
         fn make() -> Result<string, string> {{ Ok(\"x\") }}
         fn use_it() -> i64 {{
            match make() {{
                Ok(inner) => match wrap(inner) {{ Ok(_) => 1, Err(_) => 0 }},
                Err(_) => 0,
            }}
         }}"
    );
    let p = pipeline(&src);
    assert_owned(&p, "make()", Acquisition::Fresh);
    assert_owned(&p, "wrap(inner)", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn extern_result_bound_module_fn_publishes_retained_fact() {
    let src = r#"
        extern "C" {
            fn ext_encode(payload: string) -> string;
        }
        fn last_err() -> Result<string, string> { Err("e") }
        fn try_encode(payload: string) -> Result<string, string> {
            let token = ext_encode(payload);
            match last_err() {
                Ok(_) => Ok(token),
                err => err,
            }
        }
        fn use_it() -> i64 {
            match try_encode("{}") { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_authority(&p, "ext_encode(payload)", Ownership::Unknown);
    assert_owned(&p, "last_err()", Acquisition::Fresh);
    assert_owned(&p, "try_encode(\"{}\")", Acquisition::Retained);
    assert_clean(&p);
}

#[test]
fn forwarder_reused_in_loop_remains_borrowed() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) {{
            var i = 0;
            while i < 2 {{
                match passthru(r) {{ Ok(_) => {{}}, Err(_) => {{}} }}
                i = i + 1;
            }}
         }}"
    );
    let p = pipeline(&src);
    assert_authority(&p, "passthru(r)", Ownership::Borrowed);
    assert_clean(&p);
}

#[test]
fn fresh_producer_match_publishes_fresh_fact() {
    let src = r#"
        fn make_fresh() -> Result<string, string> { Ok("x") }
        fn use_it() -> i64 {
            match make_fresh() { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_owned(&p, "make_fresh()", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn opaque_only_module_fn_stays_unknown_and_fails_closed() {
    let src = r#"
        extern "C" {
            fn ext_make() -> Result<string, string>;
        }
        fn wrap() -> Result<string, string> { ext_make() }
        fn use_it() -> i64 {
            match wrap() { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_authority(&p, "ext_make()", Ownership::Unknown);
    assert_authority(&p, "wrap()", Ownership::Unknown);
    assert_eq!(unresolved_ownership_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn direct_heap_extern_scrutinee_stays_unknown_and_fails_closed() {
    let src = r#"
        extern "C" {
            fn ext_make() -> Result<string, string>;
        }
        fn use_it() -> i64 {
            match ext_make() { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_authority(&p, "ext_make()", Ownership::Unknown);
    assert_eq!(unresolved_ownership_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn spoofed_recv_symbol_extern_stays_unknown_and_fails_closed() {
    let src = r#"
        extern "C" {
            fn hew_channel_recv_layout(ch: i64) -> Result<string, string>;
        }
        fn use_it() -> i64 {
            match hew_channel_recv_layout(0) { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_authority(&p, "hew_channel_recv_layout(0)", Ownership::Unknown);
    assert_eq!(unresolved_ownership_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn twin_call_forwarder_move_out_uses_borrowed_authority() {
    let src = format!(
        "{FORWARDER}
         fn sink(s: string) -> i64 {{ 1 }}
         fn use_it(r: Result<string, string>) -> i64 {{
            match passthru(r) {{ Ok(inner) => sink(inner), Err(_) => 0 }}
         }}"
    );
    let p = pipeline(&src);
    assert_authority(&p, "passthru(r)", Ownership::Borrowed);
    assert_eq!(payload_move_reject_count(&p), 0, "{:#?}", p.diagnostics);
    assert_clean(&p);
}

#[test]
fn owned_record_getter_move_out_publishes_clone_fact() {
    let src = r"
        type Rec { s: string; }
        fn take(r: Rec) -> i64 { 1 }
        fn use_it(ys: Vec<Rec>) -> i64 {
            match ys.get(0) { Some(v) => take(v), None => 0 }
        }
    ";
    let p = pipeline(src);
    assert_owned(&p, "ys.get(0)", Acquisition::Clone);
    assert_eq!(payload_move_reject_count(&p), 0, "{:#?}", p.diagnostics);
    assert_clean(&p);
}

#[test]
fn opaque_only_module_fn_move_out_stays_unknown_and_fails_closed() {
    let src = r#"
        extern "C" {
            fn ext_make() -> Result<string, string>;
        }
        fn wrap() -> Result<string, string> { ext_make() }
        fn sink(s: string) -> i64 { 1 }
        fn use_it() -> i64 {
            match wrap() { Ok(inner) => sink(inner), Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_authority(&p, "wrap()", Ownership::Unknown);
    assert_eq!(unresolved_ownership_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn method_call_forwarder_move_out_uses_borrowed_authority() {
    let src = r"
        type Holder { tag: i64; }
        impl Holder {
            fn forward(self, x: Result<string, string>) -> Result<string, string> { x }
        }
        fn sink(s: string) -> i64 { 1 }
        fn use_it(h: Holder, r: Result<string, string>) -> i64 {
            match h.forward(r) { Ok(inner) => sink(inner), Err(_) => 0 }
        }
    ";
    let p = pipeline(src);
    assert_authority(&p, "h.forward(r)", Ownership::Borrowed);
    assert_eq!(payload_move_reject_count(&p), 0, "{:#?}", p.diagnostics);
    assert_clean(&p);
}

#[test]
fn closure_match_forwarder_over_capture_hits_capture_move_gate() {
    let src = r"
        fn wrap(s: Vec<i64>) -> Result<Vec<i64>, Vec<i64>> { Ok(s) }
        fn runner(s: Vec<i64>) {
            let f = || {
                match wrap(s) {
                    Ok(_) => match Ok(1) { Ok(_) => {}, Err(_) => {} },
                    Err(_) => {},
                }
            };
            f();
            f();
        }
    ";
    let p = pipeline(src);
    assert_owned(&p, "wrap(s)", Acquisition::Fresh);
    assert_eq!(captured_move_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn closure_match_literal_arg_publishes_fresh_fact() {
    let src = r#"
        fn wrap(s: string) -> Result<string, string> { Ok(s) }
        fn use_it() -> i64 {
            let f = || {
                match wrap("lit") { Ok(_) => 1, Err(_) => 0 }
            };
            f()
        }
    "#;
    let p = pipeline(src);
    assert_owned(&p, "wrap(\"lit\")", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn closure_local_arg_publishes_fresh_fact() {
    let src = r#"
        fn wrap(s: string) -> Result<string, string> { Ok(s) }
        fn use_it() -> i64 {
            let f = || {
                let v = "x";
                match wrap(v) { Ok(_) => 1, Err(_) => 0 }
            };
            f()
        }
    "#;
    let p = pipeline(src);
    assert_owned(&p, "wrap(v)", Acquisition::Fresh);
    assert_clean(&p);
}

#[test]
fn closure_while_let_forwarder_hits_capture_move_gate() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) {{
            let f = || {{
                while let Ok(_v) = passthru(r) {{ break; }}
            }};
            f();
         }}"
    );
    let p = pipeline(&src);
    assert_resolved_capture_call(&p, "passthru(r)");
    assert_eq!(captured_move_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn closure_let_else_forwarder_hits_capture_move_gate() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) -> i64 {{
            let f = || {{
                let Ok(_v) = passthru(r) else {{ return 0 }};
                1
            }};
            f()
         }}"
    );
    let p = pipeline(&src);
    assert_resolved_capture_call(&p, "passthru(r)");
    assert_eq!(captured_move_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn closure_if_let_forwarder_hits_capture_move_gate() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) -> i64 {{
            let f = || {{
                if let Ok(_v) = passthru(r) {{ 1 }} else {{ 0 }}
            }};
            f()
         }}"
    );
    let p = pipeline(&src);
    assert_resolved_capture_call(&p, "passthru(r)");
    assert_eq!(captured_move_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn closure_discarded_forwarder_hits_capture_move_gate() {
    let src = format!(
        "{FORWARDER}
         fn use_it(r: Result<string, string>) {{
            let f = || {{
                passthru(r);
            }};
            f();
         }}"
    );
    let p = pipeline(&src);
    assert_resolved_capture_call(&p, "passthru(r)");
    assert_eq!(captured_move_count(&p), 1, "{:#?}", p.diagnostics);
    assert_eq!(p.diagnostics.len(), 1, "{:#?}", p.diagnostics);
}

#[test]
fn generator_body_calls_publish_checker_fresh_facts() {
    let local_src = r#"
        fn wrap(s: string) -> Result<string, string> { Ok(s) }
        fn use_it() -> i64 {
            var total = 0;
            for v in gen {
                let s = "x";
                let n = match wrap(s) { Ok(_) => 1, Err(_) => 0 };
                yield n;
            } {
                total = total + v;
            }
            total
        }
    "#;
    let local = pipeline(local_src);
    assert_owned(&local, "wrap(s)", Acquisition::Fresh);
    assert_clean(&local);

    let literal_src = r#"
        fn wrap(s: string) -> Result<string, string> { Ok(s) }
        fn use_it() -> i64 {
            var total = 0;
            for v in gen {
                let n = match wrap("lit") { Ok(_) => 1, Err(_) => 0 };
                yield n;
            } {
                total = total + v;
            }
            total
        }
    "#;
    let literal = pipeline(literal_src);
    assert_owned(&literal, "wrap(\"lit\")", Acquisition::Fresh);
    assert_clean(&literal);
}

#[test]
fn guard_buried_return_forwarder_publishes_retained_fact() {
    let src = r#"
        fn evil(p: Result<string, string>, k: i64) -> Result<string, string> {
            let d = match k {
                0 if { return p; } => 0,
                _ => 1,
            };
            if d > 0 { Ok("fresh") } else { Ok("fresh") }
        }
        fn use_it(p: Result<string, string>) -> i64 {
            match evil(p, 0) { Ok(_) => 1, Err(_) => 0 }
        }
    "#;
    let p = pipeline(src);
    assert_owned(&p, "evil(p, 0)", Acquisition::Retained);
    assert_clean(&p);
}
