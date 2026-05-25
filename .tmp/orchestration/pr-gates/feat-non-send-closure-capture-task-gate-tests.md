# Gate report - feat/non-send-closure-capture-task-gate-tests

## Scope

W4.016 fixture implementation for `SpawnedClosureNonSendCapture` through real Checker-produced closure capture facts.

## Changes

- Added `hew-hir/tests/task_gates_type_dependent.rs::checker_pipeline_rc_capture_emits_non_send_diagnostic`.
  - Source fixture:
    ```hew
    fn main() {
        let r = Rc::new(1);
        scope { (move || { let _ = r; })(); };
    }
    ```
  - Asserts typecheck is clean.
  - Asserts `tco.closure_capture_facts` contains capture `r` with `is_send == false`.
  - Asserts HIR lowering emits exactly `SpawnedClosureNonSendCapture` for capture name `r`.
  - Asserts `LowerOutput::into_result()` returns `Err`.
- Updated the `task_gates_type_dependent.rs` module header so non-Send positive coverage is no longer described as deferred.
- Added vertical-slice reject fixture `tests/vertical-slice/reject/spawned_closure_non_send_capture.hew`.
- Wired `tests/vertical-slice/run.sh` to compile that fixture and grep the targeted diagnostic text:
  `spawned closure captures non-Send value 'r'`.

## Validation

### Baseline

- `cargo test -p hew-hir --test task_gates_type_dependent`
  - Baseline before edits: pass.
  - Result: `5 passed; 0 failed`.

### After edits

- `cargo test -p hew-hir --test task_gates_type_dependent`
  - Pass.
  - Result: `6 passed; 0 failed`.
- `cargo test -p hew-hir --test task_gates --test task_gates_type_dependent --test closure_capture_lower`
  - Pass.
  - Results:
    - `closure_capture_lower`: `4 passed; 0 failed`.
    - `task_gates`: `16 passed; 0 failed`.
    - `task_gates_type_dependent`: `6 passed; 0 failed`.
- `cargo clippy -p hew-hir --all-targets -- -D warnings`
  - Pass.
- `target/debug/hew compile tests/vertical-slice/reject/spawned_closure_non_send_capture.hew`
  - Expected failure.
  - Output contains:
    - `SpawnedClosureNonSendCapture { site: SiteId(4), capture_name: "r" }`
    - `spawned closure captures non-Send value 'r'`
- `bash tests/vertical-slice/run.sh`
  - Not green due to unrelated existing failure after the new reject fixture passes.
  - Failure point: `tests/vertical-slice/accept/on_crash_info_code.hew`.
  - Manual reproduction:
    ```text
    E_NOT_YET_IMPLEMENTED: fail-closed: D10 violation: Named/user type `CrashKind` reached the LLVM emitter; the MIR D10 gate should have rejected this earlier
    ```

## Follow-ups

- Full vertical-slice remains blocked by the unrelated `on_crash_info_code` / `CrashKind` failure. The W4.016 reject fixture itself is target-asserted and passes its compile-fail diagnostic check.

## Preflight

not-ready-because-full-vertical-slice-has-unrelated-existing-on-crash-info-code-failure
