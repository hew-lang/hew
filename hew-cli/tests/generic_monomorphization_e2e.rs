//! End-to-end tests for generic record/enum monomorphisation through the full
//! HIR -> MIR -> LLVM -> native pipeline.
//!
//! These exercise the post-function-mono layout-discovery slice: a generic
//! record/enum constructed only inside a generic function body has its concrete
//! layout discovered and lowered when the surrounding function is
//! monomorphised. They also cover direct-dot impl-method dispatch on a concrete
//! generic receiver (`p.first()` where `impl<T> Pair<T> { fn first(self) -> T }`
//! and `p: Pair<i64>`): the impl-method monomorphisation is registered so MIR
//! has a concrete body. Each positive test instantiates at two distinct
//! concrete types to prove genuine polymorphism (not a single-type special
//! case); a further test pins the generic owned-record drop spine (a
//! clone/drop-supported owned field such as `string`) admitting and running.

mod support;

use support::{hew_binary, repo_root, require_codegen};

fn fixture_path(name: &str) -> std::path::PathBuf {
    repo_root()
        .join("hew-cli")
        .join("tests")
        .join("fixtures")
        .join(name)
}

fn run_fixture(name: &str) -> std::process::Output {
    std::process::Command::new(hew_binary())
        .arg("run")
        .arg(fixture_path(name))
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|e| panic!("failed to run hew binary: {e}"))
}

/// A generic record constructed inside a generic fn body monomorphises and runs
/// at two distinct `BitCopy` types (i64 and bool) in one program. The `Box<T>`
/// layout is registered only after `make` is substituted as `make$$i64` /
/// `make$$bool`, so passing at *both* types proves the discovery is genuinely
/// polymorphic rather than a single-instantiation special case.
#[test]
fn generic_record_monomorphises_at_two_bitcopy_types() {
    require_codegen();

    let output = run_fixture("generic_record_mono.hew");
    assert!(
        output.status.success(),
        "expected success; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["7", "true"],
        "generic record mono output mismatch; stdout: {stdout}"
    );
}

/// A generic fn that uses `T` in an operation (`Vec<T>` element access) and
/// returns it lowers polymorphically at i64 AND string. The string arm proves
/// an owned-type instantiation flows through the generic-fn body end-to-end.
#[test]
fn generic_vec_element_access_at_i64_and_string() {
    require_codegen();

    let output = run_fixture("generic_vec_element.hew");
    assert!(
        output.status.success(),
        "expected success; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["10", "alpha"],
        "generic Vec element output mismatch; stdout: {stdout}"
    );
}

/// A generic enum constructed inside one generic fn body and matched inside
/// another monomorphises and runs. The `Holder<i64>` tagged-union layout is
/// discovered only after `wrap` / `unwrap_or` are substituted at i64; both the
/// payload-carrying (`Present`) and unit (`Absent`) variants must route.
#[test]
fn generic_enum_monomorphises_and_matches() {
    require_codegen();

    let output = run_fixture("generic_enum_mono.hew");
    assert!(
        output.status.success(),
        "expected success; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["99", "42"],
        "generic enum mono output mismatch; stdout: {stdout}"
    );
}

/// A generic inherent impl method dispatched by dot-syntax on a concrete
/// generic receiver monomorphises and runs at two distinct `BitCopy` types
/// (i64 and bool) in one program. `nums.first()` / `nums.pick(..)` on a
/// `Pair<i64>` resolve to the qualified `Pair::first` / `Pair::pick` symbols
/// and register the per-instantiation monomorphisations so MIR has a concrete
/// body to dispatch to; before this slice the by-value `RewriteToFunction` arm
/// never registered them, so the mangled callee (`Pair::first$$i64`) had no MIR
/// body and the call failed closed at the MIR boundary. Passing at *both* the
/// i64 and bool receiver types proves the impl-method monomorphisation is
/// genuinely polymorphic, and the multi-arg `pick` proves explicit method args
/// flow through alongside the injected receiver.
#[test]
fn generic_impl_method_dispatches_at_two_bitcopy_types() {
    require_codegen();

    let output = run_fixture("generic_impl_method_mono.hew");
    assert!(
        output.status.success(),
        "expected success; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["7", "8", "true"],
        "generic impl-method mono output mismatch; stdout: {stdout}"
    );
}

/// A generic free function called with EXPLICIT turbofish type arguments
/// (`id<i64>(5)`, `id<bool>(true)`) monomorphises and runs at two distinct
/// `BitCopy` types in one program, and a turbofish whose argument is the
/// enclosing fn's own type parameter (`id<U>(x)` inside `wrap<U>`) routes
/// through the per-monomorphisation substitution. Before the checker recorded
/// turbofish call sites in `call_type_args`, the explicit-`<T>` surface
/// produced no monomorphisation entry, so MIR failed closed with
/// "MIR lowering for function call is not implemented yet"; the inferred path
/// (`let y: i64 = id(5)`) already worked, proving the gap was the recording
/// gate, not the MIR call arm. Passing at *both* i64 and bool proves genuine
/// polymorphism through the turbofish surface.
#[test]
fn turbofish_generic_call_lowers_and_runs() {
    require_codegen();

    let output = run_fixture("generic_turbofish_call_mono.hew");
    assert!(
        output.status.success(),
        "expected the explicit-turbofish generic call to lower and run; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["5", "true", "7"],
        "turbofish generic call output mismatch; stdout: {stdout}"
    );
}

/// A generic record monomorphised with an OWNED (clone/drop-SUPPORTED) field
/// type (`string`) is admitted as the generic owned-record drop spine: the
/// `Box$$string` layout is discovered, its `string` field gets an in-place
/// record drop thunk, and the value drops exactly once at scope exit. This pins
/// the intended deliverable — the shape that previously failed closed with
/// `UnsupportedUserRecordValueClass` now compiles and runs. The complementary
/// fail-closed direction (clone-UNSUPPORTED opaque/IO-handle leaves) is pinned
/// by the `hew-mir` admission unit tests
/// (`generic_instantiation_with_opaque_field_rejected_at_value_class_gate`).
#[test]
fn generic_record_with_owned_field_admits_and_runs() {
    require_codegen();

    let output = run_fixture("generic_record_owned_field_admit.hew");
    assert!(
        output.status.success(),
        "expected the generic owned-record drop spine to admit and run; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout.trim(),
        "hello",
        "generic owned-record (Box<string>) output mismatch; stdout: {stdout}"
    );
    // Admit cleanly, not crash: no Rust panic / backtrace leaked to the user.
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("panicked") && !stderr.contains("RUST_BACKTRACE"),
        "the admit path must not surface a Rust panic; stderr: {stderr}"
    );
}
