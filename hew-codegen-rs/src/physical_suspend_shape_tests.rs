//! Suspending operations share one shape: every ask converges on a single
//! release, and result materialization and handle release are module-level
//! thunks emitted once per type rather than expanded at each site.

use super::*;

const TWO_ASKS: &str = r"
actor Counter {
    var seen: i64 = 0;
    receive fn next() -> i64 {
        seen = seen + 1;
        seen
    }
}

fn main() {
    let c = spawn Counter;
    let first = match c.next() {
        .Ok(value) => value,
        .Err(_) => 0,
    };
    let second = match c.next() {
        .Ok(value) => value,
        .Err(_) => 0,
    };
    println(first + second);
    sleep(1ms);
    stop(c);
    stopped(c);
}
";

fn ir_of(source: &str) -> String {
    let physical = physical(source);
    let ctx = Context::create();
    let module = llvm(&ctx, &physical);
    module.verify().unwrap();
    module.print_to_string().to_string()
}

/// The text of the one function whose definition names `symbol`.
fn function<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let start = ir
        .lines()
        .scan(0, |offset, line| {
            let at = *offset;
            *offset += line.len() + 1;
            Some((at, line))
        })
        .find(|(_, line)| line.starts_with("define ") && line.contains(symbol))
        .map(|(at, _)| at)
        .unwrap_or_else(|| panic!("no definition of {symbol}"));
    let end = ir[start..].find("\n}\n").unwrap();
    &ir[start..start + end]
}

/// Two asks of one handler share one result thunk and one handle-release
/// thunk, and each ask site calls them instead of carrying the drain loop
/// and the `ActorError` switch.
#[test]
fn asks_of_one_handler_share_result_and_release_thunks() {
    let ir = ir_of(TWO_ASKS);
    let ramp = function(&ir, "@\"__hew_main_body$resume$body\"(");
    assert_eq!(
        ramp.matches("call void @__hew_ask_result_").count(),
        2,
        "each ask calls the result thunk once"
    );
    assert_eq!(
        ramp.matches("call ptr @__hew_release_handle_actor_call")
            .count(),
        2,
        "each ask drains its call through the shared thunk once"
    );
    for inline in ["ask.error.case", "release.operation.poll"] {
        assert!(!ramp.contains(inline), "{inline} is expanded at a site");
    }
    // The thunk bodies are emitted once, and only they poll the cleanup.
    assert_eq!(
        ir.matches("define internal void @__hew_ask_result_")
            .count(),
        1
    );
    assert_eq!(
        ir.matches("define internal ptr @__hew_release_handle_actor_call(")
            .count(),
        1
    );
    assert_eq!(
        ir.lines()
            .filter(|line| line.starts_with("@__hew_ask_error_tags_"))
            .count(),
        1,
        "the ActorError tags live in one constant table"
    );
    assert!(
        function(&ir, "@\"__hew_release_handle_actor_call$body\"(")
            .contains("@hew_actor_call_cleanup_poll"),
        "the drain lives in the thunk"
    );
}

/// Every ask has exactly one release block: its completed, cancelled, cycle
/// and failed exits converge there, so one suspension drains the call.
#[test]
fn every_ask_converges_its_exits_on_one_release() {
    let ir = ir_of(TWO_ASKS);
    let ramp = function(&ir, "@\"__hew_main_body$resume$body\"(");
    let releases = ramp
        .lines()
        .filter(|line| line.starts_with("ask.release") && !line.contains(".from."))
        .count();
    assert_eq!(releases, 2, "one release per ask site");
}
