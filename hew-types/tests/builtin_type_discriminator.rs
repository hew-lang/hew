mod common;

use hew_types::{BuiltinType, Ty};

#[test]
fn user_defined_option_does_not_get_builtin_discriminator() {
    let output = common::typecheck_isolated(
        r"
        pub type Option { value: i64 }
        pub type Holder { value: Option }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "user-defined Option should typecheck without builtin collision: {:?}",
        output.errors
    );
    let holder = output.type_defs.get("Holder").expect("Holder type exists");
    assert!(matches!(
        holder.fields.get("value"),
        Some(Ty::Named {
            name,
            args,
            builtin: None,
        }) if name == "Option" && args.is_empty()
    ));
}

#[test]
fn internal_option_constructor_sets_builtin_discriminator() {
    assert!(matches!(
        Ty::option(Ty::I64),
        Ty::Named {
            builtin: Some(BuiltinType::Option),
            ..
        }
    ));
}
