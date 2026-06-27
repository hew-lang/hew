#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn intrinsic_in_floor_module_is_accepted() {
    // `std.math` is the canonical floor module for math intrinsics; the
    // bodyless `#[intrinsic("math.sqrt")]` declaration must register cleanly.
    let output = check_source_in_module(
        r#"#[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;"#,
        vec!["std".to_string(), "math".to_string()],
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::IntrinsicOutsideFloor { .. })),
        "intrinsic declared in floor module `std.math` must be accepted, got: {:?}",
        output.errors
    );
}

#[test]
fn intrinsic_in_user_root_module_is_rejected() {
    // A user program declaring `#[intrinsic]` at the root module must be
    // rejected: the root/user module is never a floor module (fail-closed).
    let output = check_source(r#"#[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;"#);
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOutsideFloor {
            intrinsic_key,
            module,
        } => Some((intrinsic_key.clone(), module.clone())),
        _ => None,
    });
    let (key, module) =
        hit.expect("root-module `#[intrinsic]` declaration must be rejected as outside the floor");
    assert_eq!(key, "math.sqrt", "diagnostic must name the intrinsic key");
    assert_eq!(
        module, "(root)",
        "diagnostic must label the offending module as the root/user module"
    );
}

#[test]
fn intrinsic_in_non_floor_module_is_rejected() {
    // A non-floor module (here a user `app` module) is likewise rejected,
    // proving the gate is an explicit allowlist, not "any module with a path".
    let output = check_source_in_module(
        r#"#[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;"#,
        vec!["app".to_string()],
    );
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOutsideFloor { module, .. } => Some(module.clone()),
        _ => None,
    });
    assert_eq!(
        hit.as_deref(),
        Some("app"),
        "non-floor module `app` must be rejected with its path in the diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn normal_function_does_not_trip_intrinsic_gate() {
    // Functions without `#[intrinsic]` are never touched by the floor gate,
    // even in a user module.
    let output = check_source("pub fn add(a: i64, b: i64) -> i64 { a + b }");
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::IntrinsicOutsideFloor { .. })),
        "non-intrinsic function must not trip the floor gate, got: {:?}",
        output.errors
    );
}

#[test]
fn intrinsic_on_impl_method_in_floor_module_is_rejected() {
    // `#[intrinsic]` on an impl method inside std.math must be REJECTED even
    // though std.math is an allowlisted floor module.  Only top-level free
    // functions in floor modules are valid intrinsic declarations; method
    // dispatch slots are never wired to compiler intrinsics (A605).
    //
    // The test asserts both legs of the fail-closed guarantee:
    //   1. The method-keyed entry does NOT appear in `intrinsic_declarations`.
    //   2. An `IntrinsicOnMethod` diagnostic IS emitted naming the key and
    //      the intrinsic catalog key.
    let source = r#"
type MathHelper {}
impl MathHelper {
    #[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;
}
"#;
    let output = check_source_in_module(source, vec!["std".to_string(), "math".to_string()]);

    // Leg 1: must NOT be in intrinsic_declarations.
    let method_key_present = output
        .intrinsic_declarations
        .keys()
        .any(|k| k.contains("::"));
    assert!(
        !method_key_present,
        "impl-method intrinsic must not be inserted into intrinsic_declarations; \
         got: {:?}",
        output.intrinsic_declarations
    );

    // Leg 2: must emit IntrinsicOnMethod naming both the catalog key and the
    // method key.
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOnMethod {
            intrinsic_key,
            method_key,
        } => Some((intrinsic_key.clone(), method_key.clone())),
        _ => None,
    });
    let (catalog_key, method_key) = hit.expect(
        "impl-method `#[intrinsic]` in a floor module must emit IntrinsicOnMethod diagnostic",
    );
    assert_eq!(
        catalog_key, "math.sqrt",
        "IntrinsicOnMethod diagnostic must name the intrinsic catalog key"
    );
    assert!(
        method_key.contains("::"),
        "IntrinsicOnMethod diagnostic must carry the method-shaped key (contains `::`);\
         got: {method_key:?}"
    );
}

// ── W5.005 (F1b): memory-intrinsic floor (`mem.*`) placement gate (A605) ───
//
// `std.mem` joins `std.math` on the `INTRINSIC_FLOOR_MODULES` allowlist. The
// same A605 gate that governs math intrinsics must accept `mem.*` declarations
// inside `std.mem` and reject them everywhere else, fail-closed.

#[test]
fn mem_intrinsic_in_floor_module_is_accepted() {
    // All five `mem.*` floor intrinsics must register cleanly inside the
    // canonical `std.mem` floor module (no IntrinsicOutsideFloor), and each
    // must land in `intrinsic_declarations` keyed by its qualified name.
    let source = r#"
#[intrinsic("mem.alloc")] pub fn alloc(size: u64, align: u64) -> *mut u8;
#[intrinsic("mem.realloc")] pub fn realloc(ptr: *mut u8, old_size: u64, new_size: u64, align: u64) -> *mut u8;
#[intrinsic("mem.dealloc")] pub fn dealloc(ptr: *mut u8, size: u64, align: u64);
#[intrinsic("mem.ptr_offset")] pub fn ptr_offset(ptr: *mut u8, byte_offset: u64) -> *mut u8;
#[intrinsic("mem.ptr_copy")] pub fn ptr_copy(dst: *mut u8, src: *mut u8, byte_count: u64);
"#;
    let output = check_source_in_module(source, vec!["std".to_string(), "mem".to_string()]);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::IntrinsicOutsideFloor { .. })),
        "mem intrinsics declared in floor module `std.mem` must be accepted, got: {:?}",
        output.errors
    );
    for key in [
        "std.mem.alloc",
        "std.mem.realloc",
        "std.mem.dealloc",
        "std.mem.ptr_offset",
        "std.mem.ptr_copy",
    ] {
        assert!(
            output.intrinsic_declarations.contains_key(key),
            "intrinsic_declarations must record `{key}`; got: {:?}",
            output.intrinsic_declarations
        );
    }
}

#[test]
fn mem_intrinsic_in_user_root_module_is_rejected() {
    // A user program declaring a `mem.*` intrinsic at the root module must be
    // rejected: the memory floor is compiler-internal-only (A605), so no
    // user-reachable module can wire itself to the allocator primitives. This
    // is validation candidate 3 (the surface-immutability acceptance gate) as
    // an executable assertion.
    let output = check_source(
        r#"#[intrinsic("mem.alloc")] pub fn alloc(size: u64, align: u64) -> *mut u8;"#,
    );
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOutsideFloor {
            intrinsic_key,
            module,
        } => Some((intrinsic_key.clone(), module.clone())),
        _ => None,
    });
    let (key, module) = hit.expect(
        "root-module `#[intrinsic(\"mem.alloc\")]` declaration must be rejected outside the floor",
    );
    assert_eq!(key, "mem.alloc", "diagnostic must name the intrinsic key");
    assert_eq!(
        module, "(root)",
        "diagnostic must label the offending module as the root/user module"
    );
    // Fail-closed: a rejected declaration must NOT leak into the live
    // intrinsic dispatch table.
    assert!(
        output.intrinsic_declarations.is_empty(),
        "rejected mem intrinsic must not be recorded as a dispatch target; got: {:?}",
        output.intrinsic_declarations
    );
}

#[test]
fn mem_intrinsic_in_non_floor_module_is_rejected() {
    // A non-floor `app` module is likewise rejected — the allowlist is an
    // explicit enumeration (`std.math`, `std.mem`), not a prefix/path match.
    // Pins the gate-regression behaviour (validation candidate 7) for `std.mem`.
    let output = check_source_in_module(
        r#"#[intrinsic("mem.dealloc")] pub fn dealloc(ptr: *mut u8, size: u64, align: u64);"#,
        vec!["app".to_string()],
    );
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOutsideFloor { module, .. } => Some(module.clone()),
        _ => None,
    });
    assert_eq!(
        hit.as_deref(),
        Some("app"),
        "non-floor module `app` must be rejected with its path in the diagnostic, got: {:?}",
        output.errors
    );
}
