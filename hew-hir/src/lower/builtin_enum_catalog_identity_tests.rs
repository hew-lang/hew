use super::BUILTIN_ENUM_SPECS;

#[test]
fn monomorphic_builtin_specs_retain_exact_owner_identity() {
    let identities: Vec<_> = BUILTIN_ENUM_SPECS
        .iter()
        .filter(|spec| spec.type_params.is_empty())
        .map(|spec| spec.canonical_type_name)
        .collect();
    for expected in [
        "std.builtins.LookupError",
        "std.builtins.LinkError",
        "std.link_monitor.MonitorError",
        "std.failure.CrashAction",
        "std.failure.CrashKind",
    ] {
        assert!(
            identities.contains(&expected),
            "missing HIR spec {expected}"
        );
    }
    assert!(identities.iter().all(|identity| identity.contains('.')));
}
