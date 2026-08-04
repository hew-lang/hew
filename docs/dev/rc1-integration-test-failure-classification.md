# RC1 integration test-failure classification

This report classifies the 17 previously unclassified non-`hew-cli` workspace
test failures at integration tip `7623c1fa4` against baseline `3fb4214ea`.
Both revisions were tested with the same Rust toolchain and this package set:

```sh
cargo nextest run -p hew-codegen-rs -p hew-mir -p hew-hir --no-fail-fast
```

The comparison follows the closeout rule: a failure is inherited only when the
fully qualified test name and decisive panic signature are the same at both
revisions. Process IDs, allocation addresses, and generated temporary paths
were ignored because they vary between invocations.

| Crate | Test | Tip and baseline evidence | Classification |
| --- | --- | --- | --- |
| `hew-codegen-rs` | `machine_dispatch_codegen::padded_payload_machine_uses_abi_correct_payload_size` | `emit_module must succeed: FailClosed("Move type mismatch: src=... i64 ... dest=... i8")` | Inherited |
| `hew-codegen-rs` | `stdlib_builtins_emission::free_math_builtins_emit_llvm_intrinsics` | HIR diagnostics contain `MethodCallNoRewrite` for `abs`, `min`, and `max` | Inherited |
| `hew-codegen-rs` | `for_in_vec_string_exec::vec_iter_cursor_transfers_are_path_sensitive_and_overwrite_safe` | `ordinary helper crash snapshot for Local(7) has conflicting typed drop descriptors across exit plans` | Inherited |
| `hew-codegen-rs` | `for_in_vec_string_exec::vec_iter_non_owning_values_and_call_carriers_release_exactly_once` | `ordinary helper crash snapshot for Local(11) has conflicting typed drop descriptors across exit plans` | Inherited |
| `hew-codegen-rs` | `machine_exec::run_machine_fixtures_compile_to_step_dispatch_and_state_table` | `missing step helper TrafficLight__step in IR` | Inherited |
| `hew-codegen-rs` | `machine_exec::run_emit_two_machines_fixture_executes` | stdout mismatch: actual `0\n0\n`, expected `1\n0\n` | Inherited |
| `hew-codegen-rs` | `machine_exec::run_emit_signal_fixture_executes` | stdout mismatch: actual `Idle\nActive\n0\n0\n0\nIdle\n`, expected `Idle\nActive\n1\n0\n0\nIdle\n` | Inherited |
| `hew-codegen-rs` | `pipeline_smoke::pipeline_accepts_user_fn_call_via_call_terminator` | `CheckerBoundaryViolation`: `add` has a `missing direct_call_targets entry` | Inherited |
| `hew-codegen-rs` | `d65_vec_release_truth_table::d65_actor_state_borrow_proof_rejects_a_live_release_flag` | `state_flat_full`: Vec release is not a resolvable drop-flag-gated cursor release | Inherited |
| `hew-codegen-rs` | `fork_spawn_env_drop::fork_env_rc_callback_drops_only_moved_string_argument` | `ObligationUnderReleased`: `greeting` reaches `return[bb1]` with a mint and no discharge | Inherited |
| `hew-codegen-rs` | `d65_vec_release_truth_table::d65_cursor_recursion_truth_table_has_one_owner_release_per_shape` | `state_flat_full`: Vec release is not a resolvable drop-flag-gated cursor release | Inherited |
| `hew-codegen-rs` | `coro_emission_exec::coro_substrate_round_trips_value_native` | runtime reports a mismatched active-frame stack; test receives `None` instead of `Some(0)` | Inherited |
| `hew-codegen-rs` | `coro_emission_exec::coro_substrate_guarded_heap_run_native` | runtime reports a mismatched active-frame stack; test receives `None` instead of `Some(0)` | Inherited |
| `hew-codegen-rs` | `coro_emission_exec::coro_substrate_round_trips_value_wasm32` | Wasm instantiation fails because `env::__hew_wasi_main` is undefined | Inherited |
| `hew-mir` | `cancellation_scope::fork_string_arg_spawns_via_fork_entry_shim` | `ObligationUnderReleased`: `greeting` reaches `suspend-abandon[bb0]` with a mint and no discharge | Inherited |
| `hew-mir` | `elaborate::user_record_string_field_drops_record_once` | `ObligationUnderReleased`: `full` reaches `return[bb1]` with a mint and no discharge | Inherited |
| `hew-hir` | `vertical::timeout_await_operation_preserves_ordered_subsumption_spine` | assertion cannot find the expected `CheckerBoundaryViolation` for `produced value identity` | Inherited |

Result: 17 inherited, 0 introduced. These are Cargo test failures, not
`tests/hew` suite fixtures, so this classification does not change
`scripts/hew-suite-expected-failures.txt`.
