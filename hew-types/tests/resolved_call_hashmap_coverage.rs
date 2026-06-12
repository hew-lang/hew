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
fn layout_key_hashmap_methods_dual_emit_resolved_calls() {
    let output = typecheck(
        r"
        record Point { x: i64, y: i64 }

        fn main() {
            let m: HashMap<Point, i64> = HashMap::new();
            m.insert(Point { x: 1, y: 2 }, 10);
            let _v = m.get(Point { x: 1, y: 2 });
            let _has = m.contains_key(Point { x: 1, y: 2 });
            let _removed = m.remove(Point { x: 1, y: 2 });
            let _len = m.len();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "layout-key HashMap canary should typecheck; got: {:#?}",
        output.errors
    );

    // Stage C3 cutover: dual-emit is retired. The legacy `method_call_rewrites` channel no longer
    // receives HashMap runtime symbols; authority lives entirely in `resolved_calls`.
    let symbols = rewrite_symbols(&output);
    for legacy in [
        "hew_hashmap_insert_layout",
        "hew_hashmap_get_layout",
        "hew_hashmap_contains_key_layout",
        "hew_hashmap_remove_layout",
        "hew_hashmap_len_layout",
    ] {
        assert!(
            !symbols.contains(legacy),
            "Stage C3: legacy MethodCallRewrite symbol {legacy} must no longer be emitted; got {symbols:#?}",
        );
    }

    assert_eq!(
        output.resolved_calls.len(),
        5,
        "every HashMap method call site should produce a ResolvedCall: {:#?}",
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
            "get".to_string(),
            "contains_key".to_string(),
            "remove".to_string(),
            "len".to_string(),
        ])
    );

    for call in output.resolved_calls.values() {
        assert_eq!(call.impl_id, ImplId(0));
        assert_eq!(
            call.type_args,
            vec![
                TyPattern::Primitive("Point".into()),
                TyPattern::Primitive("i64".into())
            ],
            "ResolvedCall type args should mirror HashMap<K, V> receiver binding"
        );
        assert_eq!(
            call.target.call_hint,
            hew_types::check::dispatch::CallAbiHint::RuntimeShim
        );
        assert!(
            !call.target.consumes_receiver,
            "HashMap runtime methods do not consume the receiver"
        );
    }
}
