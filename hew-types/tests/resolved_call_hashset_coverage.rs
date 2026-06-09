mod common;

use std::collections::HashSet;

use hew_types::check::dispatch::{ImplId, TyPattern};
use hew_types::check::MethodCallRewrite;

use common::typecheck;

fn rewrite_symbols(output: &hew_types::TypeCheckOutput) -> HashSet<String> {
    output
        .method_call_rewrites
        .values()
        .filter_map(|rewrite| match rewrite {
            MethodCallRewrite::RewriteToFunction { c_symbol, .. } => Some(c_symbol.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn layout_element_hashset_methods_dual_emit_resolved_calls() {
    let output = typecheck(
        r"
        record Point { x: i64, y: i64 }

        fn main() {
            let s: HashSet<Point> = HashSet::new();
            s.insert(Point { x: 1, y: 2 });
            let _has = s.contains(Point { x: 1, y: 2 });
            let _removed = s.remove(Point { x: 1, y: 2 });
            let _len = s.len();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "layout-element HashSet canary should typecheck; got: {:#?}",
        output.errors
    );

    // Stage C3 cutover: HashSet dual-emit retired; resolved_calls is the sole authority.
    let symbols = rewrite_symbols(&output);
    for legacy in [
        "hew_hashset_insert_layout",
        "hew_hashset_contains_layout",
        "hew_hashset_remove_layout",
        "hew_hashset_len_layout",
    ] {
        assert!(
            !symbols.contains(legacy),
            "Stage C3: legacy MethodCallRewrite symbol {legacy} must no longer be emitted; got {symbols:#?}",
        );
    }

    assert_eq!(
        output.resolved_calls.len(),
        4,
        "every legacy-green HashSet runtime rewrite should have a parallel ResolvedCall: {:#?}",
        output.resolved_calls
    );

    let methods: HashSet<String> = output
        .resolved_calls
        .values()
        .map(|call| call.method_name.clone())
        .collect();
    assert_eq!(
        methods,
        HashSet::from([
            "insert".to_string(),
            "contains".to_string(),
            "remove".to_string(),
            "len".to_string(),
        ])
    );

    for call in output.resolved_calls.values() {
        assert_eq!(call.impl_id, ImplId(1));
        assert_eq!(
            call.type_args,
            vec![TyPattern::Primitive("Point".into())],
            "ResolvedCall type args should mirror HashSet<T> receiver binding"
        );
        assert_eq!(
            call.target.call_hint,
            hew_types::check::dispatch::CallAbiHint::RuntimeShim
        );
        assert!(
            !call.target.consumes_receiver,
            "legacy HashSet runtime methods do not consume the receiver"
        );
    }
}

#[test]
fn unsupported_float_hashset_stays_checker_rejected_and_unresolved() {
    let output = typecheck(
        r"
        fn main() {
            let s: HashSet<f64> = HashSet::new();
            s.insert(1.0);
        }
        ",
    );

    assert!(
        !output.errors.is_empty(),
        "HashSet<f64> must remain rejected by the legacy Stage-B allowlist"
    );
    assert!(
        output.resolved_calls.is_empty(),
        "rejected HashSet<f64> sites must not receive ResolvedCall entries"
    );
}
