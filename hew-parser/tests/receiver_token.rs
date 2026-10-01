use hew_parser::ast::{Item, TraitItem};

/// The parser publishes the receiver from the `self` token alone: a first
/// parameter typed `Self` or the impl target without the token belongs to an
/// associated function.
#[test]
fn receiver_is_published_from_the_self_token() {
    let source = r"type Point {
    x: i64;
}

impl Point {
    fn by_value(self) -> i64 { self.x }
    fn by_var(var self) { self.x = 1; }
    fn by_consume(consume self) -> i64 { self.x }
    fn named(p: Point) -> i64 { p.x }
}

trait Zero {
    fn zero(value: Self) -> Self;
    fn is_zero(self) -> bool;
}
";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parser errors: {:?}",
        parsed.errors
    );
    let Item::Impl(impl_decl) = &parsed.program.items[1].0 else {
        panic!("expected impl item");
    };
    let receivers: Vec<bool> = impl_decl
        .methods
        .iter()
        .map(|method| method.params.first().is_some_and(|p| p.is_receiver))
        .collect();
    assert_eq!(receivers, [true, true, true, false]);
    assert!(impl_decl.methods[1].params[0].is_mutable);

    let Item::Trait(trait_decl) = &parsed.program.items[2].0 else {
        panic!("expected trait item");
    };
    let receivers: Vec<bool> = trait_decl
        .items
        .iter()
        .filter_map(|item| match item {
            TraitItem::Method(method) => Some(method.params.first().is_some_and(|p| p.is_receiver)),
            _ => None,
        })
        .collect();
    assert_eq!(receivers, [false, true]);
}
