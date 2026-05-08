use hew_parser::ast::{Item, Visibility};

/// `pub fn` inside a non-generic impl body: visibility must flow into the `FnDecl`.
#[test]
fn pub_method_nongeneric_impl_visibility() {
    let source = r"
        type Foo {
            x: int;
        }

        impl Foo {
            pub fn make(v: int) -> Foo {
                Foo { x: v }
            }

            fn private_helper(f: Foo) -> int {
                f.x
            }
        }
    ";

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parser errors: {:?}",
        parsed.errors
    );

    let impl_decl = match &parsed.program.items[1].0 {
        Item::Impl(id) => id,
        other => panic!("expected impl item, got {other:?}"),
    };

    assert_eq!(impl_decl.methods.len(), 2);

    let make = &impl_decl.methods[0];
    assert_eq!(make.name, "make");
    assert_eq!(
        make.visibility,
        Visibility::Pub,
        "pub fn make should have Visibility::Pub"
    );

    let helper = &impl_decl.methods[1];
    assert_eq!(helper.name, "private_helper");
    assert_eq!(
        helper.visibility,
        Visibility::Private,
        "fn private_helper should have Visibility::Private"
    );
}

/// `pub fn` inside a generic impl body: visibility must flow into the `FnDecl`.
#[test]
fn pub_method_generic_impl_visibility() {
    let source = r"
        type Wrapper<T> {
            value: T;
        }

        impl<T> Wrapper<T> {
            pub fn new(v: T) -> Wrapper<T> {
                Wrapper { value: v }
            }

            fn unwrap(w: Wrapper<T>) -> T {
                w.value
            }
        }
    ";

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parser errors: {:?}",
        parsed.errors
    );

    let impl_decl = match &parsed.program.items[1].0 {
        Item::Impl(id) => id,
        other => panic!("expected impl item, got {other:?}"),
    };

    assert_eq!(impl_decl.methods.len(), 2);

    let new_fn = &impl_decl.methods[0];
    assert_eq!(new_fn.name, "new");
    assert_eq!(
        new_fn.visibility,
        Visibility::Pub,
        "pub fn new should have Visibility::Pub"
    );

    let unwrap_fn = &impl_decl.methods[1];
    assert_eq!(unwrap_fn.name, "unwrap");
    assert_eq!(
        unwrap_fn.visibility,
        Visibility::Private,
        "fn unwrap should have Visibility::Private"
    );
}
