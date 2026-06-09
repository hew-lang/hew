mod common;

use common::typecheck_isolated as typecheck;

#[test]
fn trait_object_different_order_unifies() {
    let output = typecheck(
        r"
        trait A { fn a(val: Self) -> i64; }
        trait B { fn b(val: Self) -> i64; }
        fn takes_ab(x: dyn (A + B)) -> i64 { x.a() }
        fn gives_ba(x: dyn (B + A)) -> i64 { takes_ab(x) }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}
