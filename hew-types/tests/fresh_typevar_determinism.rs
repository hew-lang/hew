//! Inference-variable allocation must not depend on hash-table order.
//!
//! This binary holds one test so the process-wide `TypeVar` counter is not
//! shared with concurrently running tests.

mod common;

use hew_types::ty::TypeVar;

/// Several impls can supply `Base` for `Crate<_>`; only the last declared one
/// matches `Crate<i64>`. Each failed candidate allocates its own inference
/// variables, so a hash-ordered candidate walk allocates a different number
/// of variables on every check.
const SOURCE: &str = "
trait Base {}
trait Ext0: Base {}
trait Ext1: Base {}
trait Ext2: Base {}
trait Ext3: Base {}
trait Ext4: Base {}
trait Ext5: Base {}
trait Ext6: Base {}
trait Ext7: Base {}
type Crate<X> { item: X; }
impl<X, Y, Z> Ext0 for Crate<Vec<(X, Y, Z)>> {}
impl<X, Y, Z> Ext1 for Crate<Vec<(X, Y, Z)>> {}
impl<X, Y, Z> Ext2 for Crate<Vec<(X, Y, Z)>> {}
impl<X, Y, Z> Ext3 for Crate<Vec<(X, Y, Z)>> {}
impl<X, Y, Z> Ext4 for Crate<Vec<(X, Y, Z)>> {}
impl<X, Y, Z> Ext5 for Crate<Vec<(X, Y, Z)>> {}
impl<X, Y, Z> Ext6 for Crate<Vec<(X, Y, Z)>> {}
impl<X, Y, Z> Ext7 for Crate<Vec<(X, Y, Z)>> {}
impl<X> Base for Crate<X> {}
fn need<T: Base>(value: T) -> T { value }
fn main() {
    let crate_value = need(Crate { item: 1 });
    println(crate_value.item);
}
";

fn allocated_variables() -> u32 {
    let before = TypeVar::fresh();
    let output = common::typecheck(SOURCE);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    TypeVar::fresh().0 - before.0
}

#[test]
fn checking_allocates_the_same_inference_variables_every_time() {
    let first = allocated_variables();
    for _ in 0..5 {
        assert_eq!(allocated_variables(), first);
    }
}
