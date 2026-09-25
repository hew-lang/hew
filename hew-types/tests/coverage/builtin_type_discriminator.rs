use crate::common;

use hew_types::{BuiltinType, Ty};

#[test]
fn user_defined_option_box_does_not_get_builtin_discriminator() {
    let output = common::typecheck_isolated(
        r"
        pub type OptionBox { value: i64 }
        pub type Holder { value: OptionBox }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "user-defined nominal should typecheck without builtin identity: {:?}",
        output.errors
    );
    let holder = output
        .type_def_at_path("Holder")
        .expect("Holder type exists");
    assert!(matches!(
        holder.fields.get("value"),
        Some(Ty::Named { args, head: name_head @ (hew_types::TypeHead::Nominal(_) | hew_types::TypeHead::Param(_) | hew_types::TypeHead::Unresolved(_)) }) if name_head.spelling() == "OptionBox" && args.is_empty()
    ));
}

#[test]
fn internal_option_constructor_sets_builtin_discriminator() {
    assert!(matches!(
        Ty::option(Ty::I64),
        Ty::Named {
            head: hew_types::TypeHead::Builtin(BuiltinType::Option),
            ..
        }
    ));
}
