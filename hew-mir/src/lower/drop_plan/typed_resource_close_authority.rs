use super::*;
use hew_hir::OpaqueResourceLifecycle;
use hew_types::ffi_contracts::ReleaseDischargeDepth;
use hew_types::runtime_call::RuntimeDropDescriptor;
use std::collections::BTreeSet;

fn admit_lifecycle(classes: &mut hew_hir::TypeClassTable, ty: &str, close: &str) {
    classes
        .admit_opaque_resource_lifecycle(OpaqueResourceLifecycle {
            resource_declaration: hew_types::DefId::new(ty),
            close_declaration: hew_types::DefId::new(format!("{ty}::close")),
            release_declaration: hew_types::DefId::new(format!(
                "hew_release_{}",
                ty.replace('.', "_")
            )),
            close_symbol: close.to_string(),
            release_symbol: format!("hew_release_{}", ty.replace('.', "_")),
            discharge_depth: ReleaseDischargeDepth::Shallow,
            producer_declarations: BTreeSet::default(),
            producer_symbols: BTreeSet::default(),
            producer_modules: BTreeSet::default(),
        })
        .expect("test lifecycle is unique");
}

#[test]
fn builtin_identity_selects_runtime_close_without_type_class_spelling() {
    let classes = hew_hir::TypeClassTable::new();
    for (builtin, expected) in [
        (BuiltinType::Duplex, RuntimeDropDescriptor::DuplexClose),
        (BuiltinType::Stream, RuntimeDropDescriptor::StreamClose),
        (BuiltinType::Sink, RuntimeDropDescriptor::SinkClose),
        (BuiltinType::Sender, RuntimeDropDescriptor::SenderClose),
        (BuiltinType::Receiver, RuntimeDropDescriptor::ReceiverClose),
        (
            BuiltinType::LambdaActorHandle,
            RuntimeDropDescriptor::LambdaActorHandleClose,
        ),
        (
            BuiltinType::LambdaPid,
            RuntimeDropDescriptor::LambdaActorHandleClose,
        ),
        (BuiltinType::SendHalf, RuntimeDropDescriptor::SendHalfClose),
        (BuiltinType::RecvHalf, RuntimeDropDescriptor::RecvHalfClose),
        (
            BuiltinType::CancellationToken,
            RuntimeDropDescriptor::CancellationTokenRelease,
        ),
        (
            BuiltinType::MonitorRef,
            RuntimeDropDescriptor::MonitorRefClose,
        ),
    ] {
        let ty = ResolvedTy::named_builtin("Handle", builtin, vec![ResolvedTy::String]);
        assert!(matches!(
            resource_drop_fn(&ty, &classes),
            Some(crate::model::DropFnSpec::Runtime(actual)) if actual == expected
        ));
    }
}

#[test]
fn legacy_user_resources_remain_explicit_closes_but_not_opaque_registry_entries() {
    let collisions = [
        ("Duplex", "close"),
        ("Stream", "close"),
        ("Sink", "close"),
        ("Sender", "close"),
        ("Receiver", "close"),
        ("LambdaActorHandle", "close"),
        ("SendHalf", "close"),
        ("RecvHalf", "close"),
        ("CancellationToken", "release"),
        ("MonitorRef", "close"),
    ];
    let mut classes = hew_hir::TypeClassTable::new();
    for (name, method) in collisions {
        classes.insert(
            name.to_string(),
            (ResourceMarker::Resource, Some(method.to_string())),
        );
    }
    let registry = resource_opaque_close_registry(&classes);
    assert!(
        registry.is_empty(),
        "the opaque registry must contain only checker-admitted lifecycle facts"
    );
    for (name, method) in collisions {
        let symbol = format!("{name}::{method}");
        assert_eq!(
            resource_drop_fn(&ResolvedTy::named_user(name, vec![]), &classes),
            Some(crate::model::DropFnSpec::UserClose(symbol)),
            "user resource `{name}` must retain generated-function close authority"
        );
    }
}

#[test]
fn same_leaf_opaque_resources_keep_distinct_qualified_close_authority() {
    let mut classes = hew_hir::TypeClassTable::new();
    classes.insert(
        "Receiver".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );
    classes.insert(
        "foo.Receiver".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );
    classes.insert(
        "bar.Receiver".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );
    admit_lifecycle(&mut classes, "foo.Receiver", "foo.Receiver::close");
    admit_lifecycle(&mut classes, "bar.Receiver", "bar.Receiver::dispose");
    let registry = resource_opaque_close_registry(&classes);
    assert_eq!(
        registry,
        vec![
            (
                "bar.Receiver".to_string(),
                "bar.Receiver::dispose".to_string()
            ),
            (
                "foo.Receiver".to_string(),
                "foo.Receiver::close".to_string()
            ),
        ]
    );
    assert_eq!(
        resource_drop_fn(
            &ResolvedTy::named_opaque("bar.Receiver".to_string(), Vec::new()),
            &classes,
        ),
        Some(crate::model::DropFnSpec::UserClose(
            "bar.Receiver::dispose".to_string()
        ))
    );
    assert_eq!(
        resource_drop_fn(
            &ResolvedTy::named_opaque("Receiver".to_string(), Vec::new()),
            &classes,
        ),
        None,
        "an opaque leaf spelling must not inherit either qualified lifecycle"
    );
    assert_eq!(
        resource_drop_fn(
            &ResolvedTy::named_opaque("foo.Receiver".to_string(), Vec::new()),
            &classes,
        ),
        Some(crate::model::DropFnSpec::UserClose(
            "foo.Receiver::close".to_string()
        ))
    );
}

#[test]
fn nonopaque_resource_close_never_crosses_same_leaf_owner() {
    let mut classes = hew_hir::TypeClassTable::new();
    classes.insert(
        "left.Socket".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );
    // Models a legacy bare spelling in the table. The foreign qualified
    // type must not inherit it through a short-name fallback.
    classes.insert(
        "Socket".to_string(),
        (ResourceMarker::Resource, Some("legacy_close".to_string())),
    );
    assert_eq!(
        resource_drop_fn(&ResolvedTy::named_user("left.Socket", Vec::new()), &classes),
        Some(crate::model::DropFnSpec::UserClose(
            "left.Socket::close".to_string()
        )),
        "the declaration's exact owner retains its close"
    );
    assert_eq!(
        resource_drop_fn(
            &ResolvedTy::named_user("right.Socket", Vec::new()),
            &classes
        ),
        None,
        "a same-leaf foreign type must not acquire left.Socket::close"
    );
}
