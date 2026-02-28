use hew_parser::ast::{Item, TraitItem};

#[test]
fn parses_trait_defaults_and_impl_type_aliases() {
    let source = r#"
        trait Iterator {
            type Item = int;
            fn next(self) -> Self::Item;
        }

        type Counter {
            value: int;
        }

        impl Iterator for Counter {
            type Item = int;
            fn next(self) -> Self::Item {
                self.value
            }
        }
    "#;

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parser errors: {:?}",
        parsed.errors
    );

    let trait_decl = match &parsed.program.items[0].0 {
        Item::Trait(tr) => tr,
        other => panic!("expected trait item, got {other:?}"),
    };
    let assoc = trait_decl
        .items
        .iter()
        .find_map(|item| {
            if let TraitItem::AssociatedType { default, .. } = item {
                Some(default)
            } else {
                None
            }
        })
        .expect("trait should have associated type");
    assert!(assoc.is_some(), "expected associated type default");

    let impl_decl = match &parsed.program.items[2].0 {
        Item::Impl(id) => id,
        other => panic!("expected impl item, got {other:?}"),
    };
    assert_eq!(impl_decl.type_aliases.len(), 1);
    assert_eq!(impl_decl.type_aliases[0].name, "Item");
}
