use crate::support;

#[test]
fn dotted_type_member_shapes_lower_from_checker_facts() {
    let output = support::checker_pipeline::lower_through_checker(
        r"
machine Lifecycle {
    events { Reset, }
    state Start,
    state Running { value: i64, },
    on Reset: Running => Start { Start }
    default { state }
}

fn main() -> i64 {
    let start: Lifecycle = Lifecycle.Start;
    let running: Lifecycle = Lifecycle.Running { value: 42 };
    let some: Option<i64> = Option.Some(5);
    let ok: Result<i64, string> = Result.Ok(6);
    let set: HashSet<i64> = HashSet<i64>.new();
    let explicit: Option<i64> = Option<i64>.Some(7);
    set.insert(8);
    some.unwrap() + ok.unwrap() + explicit.unwrap() + set.len()
}
",
    );

    assert!(
        output.diagnostics.is_empty(),
        "dotted type members must lower without re-resolving their heads: {:#?}",
        output.diagnostics
    );
}
