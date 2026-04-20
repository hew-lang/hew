mod common;

use common::typecheck_isolated as typecheck;

#[test]
fn trait_object_different_order_unifies() {
    let output = typecheck(
        r"
        trait A { fn a(val: Self) -> int; }
        trait B { fn b(val: Self) -> int; }
        fn takes_ab(x: dyn (A + B)) -> int { x.a() }
        fn gives_ba(x: dyn (B + A)) -> int { takes_ab(x) }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}
