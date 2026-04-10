use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

fn run_wire_check(current: &str, baseline: &str) -> Output {
    let dir = tempfile::tempdir().unwrap();
    let current_path = dir.path().join("current.hew");
    let baseline_path = dir.path().join("baseline.hew");
    std::fs::write(&current_path, current).unwrap();
    std::fs::write(&baseline_path, baseline).unwrap();

    Command::new(hew_binary())
        .args([
            "wire",
            "check",
            current_path.to_str().unwrap(),
            "--against",
            baseline_path.to_str().unwrap(),
        ])
        .current_dir(repo_root())
        .output()
        .unwrap()
}

/// Asserts the check passes (exit 0) and returns stderr for optional warning inspection.
fn assert_wire_check_ok(current: &str, baseline: &str) -> String {
    let output = run_wire_check(current, baseline);
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        output.status.success(),
        "expected wire check to pass but it failed:\n{stderr}"
    );
    stderr
}

// ── (1) Struct field-number reuse / duplicate tags ──────────────────────────

/// Duplicate @N within the same wire type in current schema is an error.
#[test]
fn wire_check_rejects_duplicate_field_tag_in_current() {
    let output = run_wire_check(
        "wire type Msg { id: String @1; also_id: i32 @1; }\n",
        "wire type Msg { id: String @1; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("current schema: wire `Msg` reuses field number @1"),
        "{stderr}",
    );
}

/// Duplicate @N in the baseline schema is also an error.
#[test]
fn wire_check_rejects_duplicate_field_tag_in_baseline() {
    let output = run_wire_check(
        "wire type Msg { id: String @1; }\n",
        "wire type Msg { id: String @1; also_id: i32 @1; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("baseline schema: wire `Msg` reuses field number @1"),
        "{stderr}",
    );
}

/// Reusing a field number for a *different* field name across schema versions
/// is a breaking change (field-number reuse, not just renaming).
#[test]
fn wire_check_rejects_field_tag_reassigned_to_different_name() {
    let output = run_wire_check(
        "wire type Msg { label: String @1; }\n",
        "wire type Msg { id: String @1; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("reused field number @1 in `Msg`: `id` became `label`"),
        "{stderr}",
    );
}

// ── (2) Field type / repeatedness changes ────────────────────────────────────

/// Changing the wire type of a field is a breaking change.
#[test]
fn wire_check_rejects_field_type_change() {
    let output = run_wire_check(
        "wire type Msg { count: i32 @1; }\n",
        "wire type Msg { count: String @1; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed field type for `Msg.count @1`: `String` -> `i32`"),
        "{stderr}",
    );
}

/// Changing a scalar field to `repeated` is a breaking change.
#[test]
fn wire_check_rejects_field_repeatedness_change() {
    let output = run_wire_check(
        "wire type Msg { tags: String @1 repeated; }\n",
        "wire type Msg { tags: String @1; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed field type for `Msg.tags @1`"),
        "{stderr}",
    );
}

// ── (3) Optional → required change ──────────────────────────────────────────

/// Promoting a previously optional field to required is a warning because old
/// writers will not supply it, leaving new required readers without a value.
#[test]
fn wire_check_warns_optional_to_required() {
    let output = run_wire_check(
        "wire type Msg { name: String @1; }\n",
        "wire type Msg { name: String @1 optional; }\n",
    );

    assert!(
        output.status.success(),
        "optional→required should be a warning, not an error: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("new required field `Msg.name @1` has no default"),
        "{stderr}",
    );
}

// ── (4) New required field warning and `since` suppression ──────────────────

/// Adding a required field without a `since` annotation warns that old writers
/// will not supply the field.
#[test]
fn wire_check_warns_new_required_field_without_since() {
    let output = run_wire_check(
        "wire type Msg { id: String @1; extra: i32 @2; }\n",
        "wire type Msg { id: String @1; }\n",
    );

    assert!(
        output.status.success(),
        "new required field without since should be a warning: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("new required field `Msg.extra @2` has no default"),
        "{stderr}",
    );
}

/// A new required field marked `since N` where N > baseline version suppresses
/// the "has no default" warning — the schema author is asserting that all
/// existing producers have been updated.
#[test]
fn wire_check_suppresses_new_required_field_warning_with_since() {
    // current: version 2 with a required field introduced at version 2
    // baseline: version 1 without that field
    let stderr = assert_wire_check_ok(
        "#[wire(version = 2)]\nstruct Msg { id: String, extra: i32 since 2, }\n",
        "#[wire(version = 1)]\nstruct Msg { id: String, }\n",
    );
    assert!(
        !stderr.contains("new required field"),
        "expected `since` to suppress the new-required-field warning:\n{stderr}",
    );
}

// ── (5) Deprecated field warning ─────────────────────────────────────────────

/// A deprecated field in the current schema always produces a warning.
#[test]
fn wire_check_warns_deprecated_field() {
    let output = run_wire_check(
        "wire type Msg { id: String @1; legacy: i32 @2 deprecated; }\n",
        "wire type Msg { id: String @1; legacy: i32 @2; }\n",
    );

    assert!(
        output.status.success(),
        "deprecated field should be a warning, not an error: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("deprecated field usage: `Msg.legacy @2`"),
        "{stderr}",
    );
}

// ── (6) Removed required field rejection ─────────────────────────────────────

/// Dropping a required field from a wire struct is a breaking change.
#[test]
fn wire_check_rejects_removed_required_field() {
    let output = run_wire_check(
        "wire type Msg { id: String @1; }\n",
        "wire type Msg { id: String @1; count: i32 @2; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("removed required field `Msg.count @2`"),
        "{stderr}",
    );
}

/// Dropping an *optional* field from a wire struct is safe; no error or warning.
#[test]
fn wire_check_allows_removed_optional_field() {
    let stderr = assert_wire_check_ok(
        "wire type Msg { id: String @1; }\n",
        "wire type Msg { id: String @1; tag: String @2 optional; }\n",
    );
    assert!(
        !stderr.contains("removed"),
        "removing an optional field should produce no diagnostic:\n{stderr}",
    );
}

// ── (7) Version advance warning ───────────────────────────────────────────────

/// When the current schema version is higher than the baseline version,
/// a warning is emitted but the check still passes.
#[test]
fn wire_check_warns_version_advance() {
    let output = run_wire_check(
        "#[wire(version = 3)]\nstruct Config { host: String, }\n",
        "#[wire(version = 1)]\nstruct Config { host: String, }\n",
    );

    assert!(
        output.status.success(),
        "version advance should be a warning, not an error: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("wire `Config` version advanced from 1 to 3"),
        "{stderr}",
    );
}

// ── (8) Invalid `min_version` rejection ──────────────────────────────────────

/// When `min_version` exceeds the baseline's recorded version, old clients
/// that only understand up to the baseline version cannot decode the schema.
#[test]
fn wire_check_rejects_min_version_higher_than_baseline() {
    let output = run_wire_check(
        "#[wire(version = 2, min_version = 3)]\nstruct Config { host: String, }\n",
        "#[wire(version = 1)]\nstruct Config { host: String, }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(
            "wire `Config`: min_version 3 is higher than baseline version 1 \
             — old clients cannot decode"
        ),
        "{stderr}",
    );
}

// ── (9) Declaration kind changes and duplicate declarations ───────────────────

/// Changing a wire struct to a wire enum (or vice versa) is a breaking change.
#[test]
fn wire_check_rejects_struct_to_enum_kind_change() {
    let output = run_wire_check(
        "wire enum Msg { Ok; Err; }\n",
        "wire type Msg { id: String @1; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("wire `Msg` changed declaration kind"),
        "{stderr}",
    );
}

/// Duplicate wire declarations in the current schema are an error.
#[test]
fn wire_check_rejects_duplicate_declaration_in_current() {
    let output = run_wire_check(
        "wire type Msg { id: String @1; }\nwire type Msg { id: String @1; }\n",
        "wire type Msg { id: String @1; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("current schema: duplicate wire declaration `Msg`"),
        "{stderr}",
    );
}

// ── (10) Wire type / enum removal rejection ───────────────────────────────────

/// Removing a wire struct that has required fields is a breaking change.
#[test]
fn wire_check_rejects_removed_wire_struct_with_required_fields() {
    let output = run_wire_check(
        "",
        "wire type Request { id: String @1; payload: String @2; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("removed required field `Request.id @1` (wire type removed)"),
        "{stderr}",
    );
}

/// Removing a wire enum is always a breaking change.
#[test]
fn wire_check_rejects_removed_wire_enum() {
    let output = run_wire_check("", "wire enum Status { Active; Inactive; }\n");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("removed wire enum `Status` (wire type removed)"),
        "{stderr}",
    );
}

// ── (11) Enum variant addition / removal and payload changes ──────────────────

/// Adding a new variant to a wire enum is a breaking change because decoders
/// that do not know the new variant cannot handle it.
#[test]
fn wire_check_rejects_wire_enum_variant_addition() {
    let output = run_wire_check(
        "wire enum Status { Active; Inactive; Pending; }\n",
        "wire enum Status { Active; Inactive; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("added variant `Status::Pending`"),
        "{stderr}",
    );
}

/// Removing a variant from a wire enum is a breaking change.
#[test]
fn wire_check_rejects_wire_enum_variant_removal() {
    let output = run_wire_check(
        "wire enum Status { Active; }\n",
        "wire enum Status { Active; Inactive; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("removed variant `Status::Inactive`"),
        "{stderr}",
    );
}

/// Changing a unit variant to a tuple variant (payload shape change) is breaking.
#[test]
fn wire_check_rejects_wire_enum_variant_payload_shape_change() {
    let output = run_wire_check(
        "wire enum Cmd { Start(String); }\n",
        "wire enum Cmd { Start; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed payload shape for `Cmd::Start`"),
        "{stderr}",
    );
}

/// Changing the type of a tuple variant payload item is a breaking change.
#[test]
fn wire_check_rejects_wire_enum_tuple_payload_type_change() {
    let output = run_wire_check(
        "wire enum Cmd { Data(i32); }\n",
        "wire enum Cmd { Data(String); }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed payload type for `Cmd::Data` item 1: `String` -> `i32`"),
        "{stderr}",
    );
}

/// Changing a field type inside a struct variant is a breaking change.
#[test]
fn wire_check_rejects_wire_enum_struct_variant_field_type_change() {
    let output = run_wire_check(
        "wire enum Cmd { Data { value: i32 }; }\n",
        "wire enum Cmd { Data { value: String }; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed payload type for `Cmd::Data` item 1: `String` -> `i32`"),
        "{stderr}",
    );
}

// ── (12) New wire type: required / deprecated field warnings ──────────────────

/// When a brand-new wire struct appears in the current schema (not present in
/// baseline), required fields generate a "has no default" warning.
#[test]
fn wire_check_warns_new_wire_struct_with_required_fields() {
    let output = run_wire_check(
        "wire type NewMessage { id: String @1; payload: String @2; }\n",
        "",
    );

    assert!(
        output.status.success(),
        "new wire struct with required fields should warn, not error: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("new required field `NewMessage.id @1` has no default"),
        "{stderr}",
    );
}

/// When a brand-new wire struct has a deprecated field, a warning is emitted.
#[test]
fn wire_check_warns_new_wire_struct_with_deprecated_field() {
    let output = run_wire_check(
        "wire type NewMessage { id: String @1; legacy: i32 @2 deprecated; }\n",
        "",
    );

    assert!(
        output.status.success(),
        "new wire struct with deprecated field should warn, not error: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("deprecated field usage: `NewMessage.legacy @2`"),
        "{stderr}",
    );
}

// ── Baseline existing tests (kept in place) ───────────────────────────────────

#[test]
fn wire_check_rejects_reordered_wire_enum_variants() {
    let output = run_wire_check(
        "wire enum Command { Start; Pause; Stop; }\n",
        "wire enum Command { Start; Stop; Pause; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed variant order for `Command` at position 2"),
        "{stderr}",
    );
}

#[test]
fn wire_check_rejects_wire_enum_payload_changes() {
    let output = run_wire_check(
        "wire enum Command { Start; Data(String); }\n",
        "wire enum Command { Start; Data(String, u32); }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed payload arity for `Command::Data`: 2 -> 1"),
        "{stderr}",
    );
}

#[test]
fn wire_check_rejects_wire_enum_struct_variant_field_renames() {
    let output = run_wire_check(
        "wire enum Command { Data { new_value: String }; }\n",
        "wire enum Command { Data { value: String }; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(
            "changed payload field name for `Command::Data` item 1: `value` -> `new_value`"
        ),
        "{stderr}",
    );
}
