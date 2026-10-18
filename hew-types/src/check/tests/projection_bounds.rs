use super::{check_source, TypeErrorKind};

#[test]
fn inherited_associated_bound_preserves_trait_arguments_and_item_equality() {
    let output = check_source(
        r#"
        trait Cursor<Tag> {
            type Item;
            fn next(var self, tag: Tag) -> Option<Self.Item>;
        }
        trait Collection<Tag> {
            type Item;
            type Cursor: Cursor<Tag, Item = Self.Item>;
            fn cursor(self) -> Self.Cursor;
        }
        fn quantity<C>(input: C) -> i64
        where C: Collection<string, Item = (string, i64)> {
            var cursor = input.cursor();
            match cursor.next("inventory") {
                .Some(item) => { let (name, quantity) = item; quantity }
                .None => 0,
            }
        }
        "#,
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn associated_bound_rejects_a_cursor_with_different_item_equality() {
    let output = check_source(
        r"
        trait Cursor {
            type Item;
            fn next(var self) -> Option<Self.Item>;
        }
        trait Collection {
            type Item;
            type Cursor: Cursor<Item = Self.Item>;
            fn cursor(self) -> Self.Cursor;
        }
        type TextCursor {}
        impl Cursor for TextCursor {
            type Item = string;
            fn next(var self) -> Option<string> { .None }
        }
        type Numbers {}
        impl Collection for Numbers {
            type Item = i64;
            type Cursor = TextCursor;
            fn cursor(self) -> TextCursor { TextCursor {} }
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
        "{:?}",
        output.errors
    );
}

#[test]
fn inherited_associated_bound_rejects_a_different_trait_argument() {
    let output = check_source(
        r"
        trait Cursor<Tag> {
            fn next(var self, tag: Tag) -> i64;
        }
        trait Collection<Tag> {
            type Cursor: Cursor<Tag>;
            fn cursor(self) -> Self.Cursor;
        }
        fn quantity<C>(input: C) -> i64 where C: Collection<string> {
            var cursor = input.cursor();
            cursor.next(3)
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "{:?}",
        output.errors
    );
}

#[test]
fn self_referential_associated_equality_remains_a_finite_template() {
    let output = check_source(
        r"
        fn advance<I>(input: I) where I: Iterator<Item = I.Item> {
            var cursor = input;
            cursor.next();
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn inherited_item_equality_normalizes_before_any_cursor_method_call() {
    let output = check_source(
        r"
        fn same_item<I>(value: I.Item) -> <I.IntoIter as Iterator>.Item
        where I: IntoIterator { value }
    ",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn associated_equality_does_not_weaken_callable_capabilities() {
    let output = check_source(
        r"
        trait CallableSource { type Item; }
        type AffineSource {}
        impl CallableSource for AffineSource {
            type Item = fn() -> i64;
        }
        fn requires_clone<I>(input: I)
        where I: CallableSource<Item = fn[clone]() -> i64> {}
        fn main() { requires_clone(AffineSource {}); }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
        "{:?}",
        output.errors
    );
}

#[test]
fn conditional_impl_proofs_require_finite_evidence() {
    for (next, accepted) in [("Vec<Leaf>", true), ("Vec<LinkNode>", false)] {
        let source = format!(
            r"
            trait Link {{ type Next; }}
            trait Ready {{ fn ready(self) -> i64; }}
            type Terminal {{}}
            impl Ready for Terminal {{ fn ready(self) -> i64 {{ 1 }} }}
            type Leaf {{}}
            impl Link for Leaf {{ type Next = Terminal; }}
            type LinkNode {{}}
            impl Link for LinkNode {{ type Next = {next}; }}
            impl<T: Link> Ready for Vec<T> where T.Next: Ready {{
                fn ready(self) -> i64 {{ 1 }}
            }}
            fn demand<R: Ready>(rows: R) {{}}
            fn main() {{ let rows: Vec<LinkNode> = []; demand(rows); }}
            "
        );
        let output = check_source(&source);
        if accepted {
            assert!(output.errors.is_empty(), "{:?}", output.errors);
        } else {
            assert!(
                output
                    .errors
                    .iter()
                    .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
                "{:?}",
                output.errors
            );
        }
    }
}

#[test]
fn structural_witnesses_require_the_inherent_receivers_conditions() {
    for (item, accepted) in [("string", true), ("i64", false)] {
        let source = format!(
            r"
            trait Cursor {{ type Item; }}
            trait Label {{ fn label(self) -> i64; }}
            type Current {{}}
            impl Cursor for Current {{ type Item = {item}; }}
            type Inventory<T> {{ cursor: T; }}
            impl<T: Cursor<Item = string>> Inventory<T> {{
                fn label(self) -> i64 {{ 1 }}
            }}
            fn demand<R: Label>(rows: R) {{}}
            fn main() {{ demand(Inventory {{ cursor: Current {{}} }}); }}
            "
        );
        let output = check_source(&source);
        if accepted {
            assert!(output.errors.is_empty(), "{:?}", output.errors);
        } else {
            assert!(
                output
                    .errors
                    .iter()
                    .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
                "{:?}",
                output.errors
            );
        }
    }
}

#[test]
fn conditional_inherent_witness_requires_finite_evidence() {
    let output = check_source(
        r"
        trait Link { type Next; }
        trait Ready { fn ready(self) -> i64; }
        type LinkNode {}
        impl Link for LinkNode { type Next = Inventory<LinkNode>; }
        type Inventory<T> { cursor: T; }
        impl<T: Link> Inventory<T> where T.Next: Ready {
            fn ready(self) -> i64 { 1 }
        }
        fn demand<R: Ready>(rows: R) {}
        fn main() { demand(Inventory { cursor: LinkNode {} }); }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
        "{:?}",
        output.errors
    );
}

#[test]
fn strictly_growing_trait_obligations_report_a_finite_limit() {
    let output = check_source(
        r"
        trait Link { type Next; }
        trait Ready { fn ready(self) -> i64; }
        impl<U> Link for Vec<U> { type Next = Vec<Vec<Vec<U>>>; }
        impl<T: Link> Ready for Vec<T> where T.Next: Ready {
            fn ready(self) -> i64 { 1 }
        }
        fn demand<R: Ready>(rows: R) {}
        fn main() { let rows: Vec<Vec<i64>> = []; demand(rows); }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::InvalidOperation
                && error.message.contains("trait proof")
                && error.message.contains("nested queries")),
        "{:?}",
        output.errors
    );
}

#[test]
fn unsupported_where_subjects_are_diagnosed_at_checker_boundary() {
    for subject in ["Inventory<T>", "i64", "(T, i64)", "Missing"] {
        let source = format!(
            r"
            trait Label {{ fn label(self) -> i64; }}
            type Inventory<T> {{ value: T; }}
            impl<T> Inventory<T> where {subject}: Label {{
                fn first(self) -> Inventory<T> {{ self }}
            }}
            "
        );
        let output = check_source(&source);
        if subject == "Missing" {
            assert!(
                output
                    .errors
                    .iter()
                    .any(|error| error.kind == TypeErrorKind::UndefinedType),
                "{:?}",
                output.errors
            );
            assert!(
                !output
                    .errors
                    .iter()
                    .any(|error| error.message.contains("where-clause subject")),
                "{:?}",
                output.errors
            );
        } else {
            assert!(
                output
                    .errors
                    .iter()
                    .any(|error| error.kind == TypeErrorKind::InvalidOperation
                        && error.message.contains("where-clause subject")),
                "{:?}",
                output.errors
            );
        }
    }
}
