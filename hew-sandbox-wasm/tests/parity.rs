use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Output;
use std::sync::OnceLock;

use assert_cmd::Command;
use hew_sandbox_wasm::{compile_to_sandbox_bytecode, Diagnostic, REQUIRED_PARITY_TEST_NAMES};

const SANDBOX_PROFILE: &str = "sandbox-vm-export";
const HEW_SEED: &str = "42";

const PARITY_CASES: &[ParityCase] = &[
    ParityCase {
        test_name: "hello_world",
        source_rel: "examples/playground/basics/hello_world.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "fibonacci",
        source_rel: "examples/playground/basics/fibonacci.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Float add/sub/mul/neg through the type-directed f64.* opcode family.
        test_name: "float_arithmetic",
        source_rel: "examples/playground/basics/float_arithmetic.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Float division and remainder (IEEE-754, never trap-on-zero).
        test_name: "float_division",
        source_rel: "examples/playground/basics/float_division.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Non-finite f64 equality: NaN never equal, ±Infinity compare by sign,
        // -0.0 == 0.0. `==` mirrors native `fcmp OEQ` and `!=` mirrors `fcmp UNE`
        // (true whenever a NaN operand is present), proving the sandbox no longer
        // collapses NaN/±Infinity through the canonical-JSON path.
        test_name: "float_nonfinite_compare",
        source_rel: "examples/playground/basics/float_nonfinite_compare.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Integer (checked i64.*) and float (f64.*) arithmetic in one program,
        // proving the emitter dispatches the opcode family per operand type.
        test_name: "mixed_numeric",
        source_rel: "examples/playground/basics/mixed_numeric.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "function_composition",
        source_rel: "examples/playground/basics/higher_order_functions.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "pattern_matching",
        source_rel: "examples/playground/types/pattern_matching.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "collections",
        source_rel: "examples/playground/types/collections.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "record_types",
        source_rel: "examples/playground/types/record_types.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "structural_records",
        source_rel: "examples/playground/types/structural_bounds.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "counter_actor",
        source_rel: "examples/playground/concurrency/counter_actor.hew",
        // Actor/supervisor/machine support now emits bytecode; no profile divergence.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "actor_pipeline",
        source_rel: "examples/playground/concurrency/actor_pipeline.hew",
        // Actor support now emits bytecode; no profile divergence.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "supervisor",
        source_rel: "examples/playground/concurrency/supervisor.hew",
        // Supervisor source rewritten to not use supervisor_stop (native-only);
        // the supervisor decl, spawn, and child method calls are all sandbox-admitted.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "traffic_light",
        source_rel: "examples/playground/machines/traffic_light.hew",
        // Machine support now emits bytecode; no profile divergence.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "stmt_if",
        source_rel: "examples/playground/basics/stmt_if.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "stmt_match",
        source_rel: "examples/playground/basics/stmt_match.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "stmt_if_let",
        source_rel: "examples/playground/basics/stmt_if_let.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Value-position if-let (`let v = if let Value(n) = w { n } else { d }`):
        // the matched arm value is joined on a result local, so the expression
        // yields the matched value, not unit. Proves the value-position lowering.
        test_name: "if_let_value",
        source_rel: "examples/playground/basics/if_let_value.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "arithmetic_operators",
        source_rel: "examples/playground/language/arithmetic_operators.hew",
        // Integer +,-,*,/,%, unary negate, and all six comparisons.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "array_indexing",
        source_rel: "examples/playground/language/array_indexing.hew",
        // Array literal, index read, `.len()`, and range-for over the length.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "string_slicing",
        source_rel: "examples/playground/language/string_slicing.hew",
        // String `.len()` and `.slice(start, end)`.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "while_loop",
        source_rel: "examples/playground/language/while_loop.hew",
        // `while` loop with a mutable accumulator.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "wildcard_match",
        source_rel: "examples/playground/language/wildcard_match.hew",
        // Enum dispatch with a catch-all `_` arm.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "match_guard_parity",
        source_rel: "examples/playground/language/match_guard.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "match_guard_catch_all_fallthrough",
        source_rel: "examples/enums/match_guard_catch_all_fallthrough.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Fieldless-enum `==`/`!=`: `BinaryOp::Equal`/`NotEqual` on a fieldless
        // enum emits `cmp.eq`/`cmp.ne`; the VM's `compare` handler uses
        // `canonicalComparable` which serialises `{ type, tag, payload: [] }` to
        // JSON, making same-tag variants equal and different-tag variants unequal.
        // Admitted by the checker in #1987. Source lives outside the curated
        // playground set so the playground manifest is not affected.
        test_name: "fieldless_enum_eq",
        source_rel: "examples/enums/run_colour_eq.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Structural `==`/`!=` on records (user-defined struct types) and
        // payload enums (enums with data-carrying variants). The profile now
        // admits these; `lower_binary` already emits `cmp.eq`/`cmp.ne` for all
        // types; the VM's `canonicalComparable` serialises record fields and
        // enum payloads recursively to a canonical JSON string, giving
        // structural field-by-field equality that mirrors native Hew semantics.
        test_name: "record_equality",
        source_rel: "examples/playground/types/record_equality.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // `clone expr` produces an independent deep copy. The emitter lowers
        // `Expr::Clone` by evaluating the operand then emitting `local.set`
        // into a fresh temp, which calls `cloneValue` in the VM — a deep
        // recursive copy. Proves vector aliasing safety: push to the original
        // does not affect the clone.
        test_name: "clone_value",
        source_rel: "examples/playground/basics/clone_value.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`) for both i64 and
        // f64. The emitter reads the current binding, applies the type-directed
        // opcode family (i64.checked_* or f64.*), and writes the result back.
        test_name: "compound_assign",
        source_rel: "examples/playground/language/compound_assign.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Non-finite f64 values (inf, -inf, nan) render identically to native.
        // Native uses printf("%g") which produces lowercase `inf`, `-inf`, `nan`;
        // the sandbox VM's renderF64 must match exactly — not JavaScript's
        // `String(Infinity)` which produces `Infinity` / `NaN` (capitalised).
        test_name: "f64_nonfinite_render",
        source_rel: "examples/playground/language/f64_nonfinite_render.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Finite f64 values render identically to native printf("%g"): negative
        // zero prints as `-0`, large/small values use scientific notation at the
        // %g thresholds (exp < -4 or >= 6), and 6 significant digits are used
        // with trailing zeros removed. JavaScript's `String()` uses Ryu
        // shortest-round-trip digits and different notation thresholds, so the
        // sandbox VM's renderF64 must implement %g-equivalent logic rather than
        // delegating to `String()` for finite values.
        test_name: "f64_finite_render",
        source_rel: "examples/playground/language/f64_finite_render.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Tuples lowered as anonymous records with positional fields _0, _1, …
        // so record.new / record.get handle construction and let-destructure.
        test_name: "tuple_values",
        source_rel: "examples/playground/types/tuple_values.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // `==` on generic records and aggregates containing Vec<T>: the checker
        // now resolves equality eligibility after substituting the generic
        // arguments, so Pair<i64, string> compares structurally. The emitter
        // already routes `==` through `cmp.eq`; the VM's `canonicalComparable`
        // handles records, enums, and vectors recursively.
        test_name: "generic_aggregate_eq",
        source_rel: "examples/playground/types/generic_aggregate_eq.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Option/Result marker methods: `is_some`, `is_none`, `is_ok`, `is_err`,
        // `unwrap`, `unwrap_or`. The emitter now lowers these via enum.tag /
        // enum.payload bytecode sequences rather than falling through to
        // emit_unsupported. The profile explicitly admits these methods on
        // Option/Result receiver types.
        test_name: "option_result_methods",
        source_rel: "examples/playground/types/option_result_methods.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // f-string interpolation (`{x}`) for integer and char types that gained
        // Display impls: i8/i16/i32, u8/u16/u32, u64, isize, usize, char. The
        // VM stores all integers as i64/u64 BigInt and renders them via
        // renderStdout, so the interpolated output matches native printf output.
        test_name: "display_scalars",
        source_rel: "examples/playground/basics/display_scalars.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // `#[wire]` is now the sole canonical declaration surface for wire types
        // (bare `wire`/`wire type`/`wire enum` keyword forms removed in 60c50dae;
        // the `struct` keyword itself later removed in favour of `#[wire] type`).
        // Verifies the sandbox profile and emitter treat a `#[wire] type` with
        // tagged fields as a plain record without rejecting the attribute or the
        // optional-field tag annotation.
        test_name: "wire_types_declaration",
        source_rel: "examples/playground/types/wire_types.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Vec<T>::contains (linear equality scan via canonical comparison) and
        // v[start..end] (exclusive range slice) map to new sandbox VM opcodes
        // `vector.contains` / `vector.range_slice` added in this parity sweep.
        test_name: "vec_operations",
        source_rel: "examples/playground/types/vec_operations.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // v[start..=end] inclusive range slice. The emitter computes the
        // exclusive end by adding 1 and delegates to the existing
        // `vector.range_slice` opcode. Pins end-inclusive vs end-exclusive
        // semantics against native `hew run`.
        test_name: "vec_inclusive_slice",
        source_rel: "examples/playground/types/vec_inclusive_slice.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // `rec.clone()` method on a user-defined record type. The emitter lowers
        // it via `local.set` which calls `cloneValue` in the VM — a deep
        // recursive copy — so the clone and the original are independent
        // objects with no aliased fields, matching native structural copy semantics.
        test_name: "record_clone",
        source_rel: "examples/playground/types/record_clone.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // `(rec.f)(args)` fn-field call. The emitter materialises the function
        // value stored in the record field via `const.function` / `record.get`
        // and invokes it via `call.indirect`. Pins the indirect-call routing
        // against direct `call.direct` results for the same function.
        test_name: "fn_field_call",
        source_rel: "examples/playground/types/fn_field_call.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Vec<f64>::contains with NaN and +-Infinity: the sandbox VM now routes
        // element equality through valuesEqual (which uses JS === for f64 pairs),
        // matching native fcmp-OEQ semantics.  Pre-fix the VM collapsed NaN and
        // +-Infinity to JSON null so [NaN].contains(NaN) returned true instead
        // of false -- a silent wrong-result divergence.
        test_name: "vec_f64_nonfinite_contains",
        source_rel: "examples/playground/types/vec_f64_nonfinite_contains.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "bool_not",
        source_rel: "examples/sandbox-graduation/bool_not.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "scalar_match_int",
        source_rel: "examples/sandbox-graduation/scalar_match_int.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "scalar_match_string",
        source_rel: "examples/sandbox-graduation/scalar_match_string.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "bool_match",
        source_rel: "examples/sandbox-graduation/bool_match.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "struct_functional_update",
        source_rel: "examples/sandbox-graduation/struct_functional_update.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "struct_pattern_match",
        source_rel: "examples/sandbox-graduation/struct_pattern_match.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "option_some_none",
        source_rel: "examples/sandbox-graduation/option_some_none.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "const_reference",
        source_rel: "examples/sandbox-graduation/const_reference.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "logical_binary_operators",
        source_rel: "examples/sandbox-graduation/logical_binary_operators.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "bitwise_binary_operators",
        source_rel: "examples/sandbox-graduation/bitwise_binary_operators.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "compound_bitwise_assign",
        source_rel: "examples/sandbox-graduation/compound_bitwise_assign.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "shift_out_of_range",
        source_rel: "examples/sandbox-graduation/shift_out_of_range.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "struct_destructure_let",
        source_rel: "examples/sandbox-graduation/struct_destructure_let.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "record_shorthand_destructure_let",
        source_rel: "examples/sandbox-graduation/record_shorthand_destructure_let.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "nested_tuple_destructure_let",
        source_rel: "examples/sandbox-graduation/nested_tuple_destructure_let.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // Wrapping ops at the overflow boundary: the VM truncates `&+`/`&-`/`&*`
        // to two's-complement 64 bits to match native (#2341).
        test_name: "wrapping_binary_operators",
        source_rel: "examples/sandbox-graduation/wrapping_binary_operators.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // `.clone()` method-call syntax on Vec/String/Array/Slice. The
        // emitter's generic clone arm (`local.set` → `cloneValue`) already
        // handled these receiver types; the profile allowlist was the gap.
        // Proves independence: mutating the original after clone leaves the
        // copy unaffected, for every newly-admitted receiver type.
        test_name: "method_clone",
        source_rel: "examples/playground/types/method_clone.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        // `regex.Pattern::clone()` — same profile-allowlist gap as
        // `method_clone`, covered by a source file outside the curated
        // playground manifest scope: `std::text::regex` FFI is unresolvable
        // by the hew-wasm browser analyzer (dev-tooling world, distinct from
        // this sandbox VM), so a regex-importing fixture cannot live under
        // `examples/playground/` without failing the analyzer smoke test.
        test_name: "regex_clone",
        source_rel: "examples/sandbox-graduation/regex_clone.hew",
        accepted_divergences: &[],
    },
];

#[derive(Debug, Clone, Copy)]
struct ParityCase {
    test_name: &'static str,
    source_rel: &'static str,
    accepted_divergences: &'static [AcceptedDivergence],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[expect(
    dead_code,
    reason = "catalog variant reserved for future sandbox-vm divergences; \
              all current playground examples have reached full parity"
)]
enum AcceptedDivergence {
    #[doc = "Catalog: docs/sandbox-vm-divergences.md#unknown-profile-symbol"]
    UnknownProfileSymbol,
}

impl AcceptedDivergence {
    fn diagnostic_kind(self) -> &'static str {
        match self {
            Self::UnknownProfileSymbol => "unknown_symbol",
        }
    }

    fn reason(self) -> &'static str {
        match self {
            Self::UnknownProfileSymbol => {
                "playground source calls helpers or builtins that are not in the current sandbox profile allowlist"
            }
        }
    }
}

#[test]
fn minimum_parity_set_is_enforced_by_test_name() {
    let required: BTreeSet<_> = REQUIRED_PARITY_TEST_NAMES.iter().copied().collect();
    let actual: BTreeSet<_> = PARITY_CASES.iter().map(|case| case.test_name).collect();

    assert_eq!(
        actual, required,
        "native↔sandbox parity cases must exactly cover the required playground set"
    );
}

#[test]
fn sandbox_graduation_corpus_is_fully_covered() {
    let graduation_dir = repo_root().join("examples").join("sandbox-graduation");
    let expected: BTreeSet<PathBuf> = std::fs::read_dir(&graduation_dir)
        .unwrap_or_else(|err| {
            panic!(
                "failed to read sandbox graduation directory {}: {err}",
                graduation_dir.display()
            )
        })
        .map(|entry| {
            entry
                .unwrap_or_else(|err| panic!("failed to read sandbox graduation entry: {err}"))
                .path()
        })
        .filter(|path| path.extension().is_some_and(|extension| extension == "hew"))
        .collect();
    let actual: BTreeSet<PathBuf> = PARITY_CASES
        .iter()
        .filter_map(|case| {
            case.source_rel
                .strip_prefix("examples/sandbox-graduation/")
                .map(|source_rel| graduation_dir.join(source_rel))
        })
        .collect();

    assert_eq!(
        actual, expected,
        "every sandbox-graduation Hew source must have a native↔sandbox parity case"
    );
}

// Windows parity enforcement is tracked in #1823; Windows runners do not yet
// provision the hew-sandbox-vm npm toolchain for this harness.
#[cfg_attr(windows, ignore)]
#[test]
fn playground_sources_match_native_or_catalogued_divergence() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();

    for case in PARITY_CASES {
        assert_case(case);
    }
}

fn assert_case(case: &ParityCase) {
    let repo_root = repo_root();
    let source_path = repo_root.join(case.source_rel);
    let native = run_native(&source_path);
    let source = std::fs::read_to_string(&source_path).unwrap_or_else(|err| {
        panic!(
            "failed to read parity source {} for {}: {err}",
            source_path.display(),
            case.test_name
        )
    });
    let sandbox_compile = compile_to_sandbox_bytecode(&source, Some(SANDBOX_PROFILE))
        .unwrap_or_else(|err| panic!("sandbox compile threw for {}: {err}", case.test_name));

    if !case.accepted_divergences.is_empty() {
        assert_accepted_divergences(case, &sandbox_compile.diagnostics);
        assert!(
            sandbox_compile.bytecode.is_none(),
            "{} declares accepted profile divergences, but sandbox bytecode was emitted; remove the divergence catalog entry and enable parity comparison",
            case.test_name
        );
        return;
    }

    assert_no_error_diagnostics(case, &sandbox_compile.diagnostics);
    let bytecode = sandbox_compile.bytecode.unwrap_or_else(|| {
        panic!(
            "sandbox compile emitted no bytecode for {}; diagnostics:\n{}",
            case.test_name,
            diagnostics_dump(&sandbox_compile.diagnostics)
        )
    });
    let bytecode_json = serde_json::to_string_pretty(&bytecode)
        .unwrap_or_else(|err| panic!("failed to serialize bytecode for {}: {err}", case.test_name));
    let tempdir = tempfile::tempdir()
        .unwrap_or_else(|err| panic!("failed to create tempdir for {}: {err}", case.test_name));
    let bytecode_path = tempdir.path().join("bytecode.json");
    std::fs::write(&bytecode_path, bytecode_json)
        .unwrap_or_else(|err| panic!("failed to write bytecode for {}: {err}", case.test_name));

    let sandbox = run_sandbox(&bytecode_path);
    assert_exit_code_parity(case, &native, &sandbox);
    assert_stdout_parity(case, &native, &sandbox);
}

fn assert_accepted_divergences(case: &ParityCase, diagnostics: &[Diagnostic]) {
    let actual_error_kinds: BTreeSet<&str> = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == "error")
        .map(|diagnostic| diagnostic.kind.as_str())
        .collect();
    let accepted_kinds: BTreeSet<&str> = case
        .accepted_divergences
        .iter()
        .map(|divergence| divergence.diagnostic_kind())
        .collect();

    for divergence in case.accepted_divergences {
        let kind = divergence.diagnostic_kind();
        assert!(
            actual_error_kinds.contains(kind),
            "{} declared accepted divergence {:?} ({kind}: {}) but that diagnostic did not trigger; remove or update the catalog entry.\nActual diagnostics:\n{}",
            case.test_name,
            divergence,
            divergence.reason(),
            diagnostics_dump(diagnostics)
        );
    }

    for kind in &actual_error_kinds {
        assert!(
            accepted_kinds.contains(kind),
            "{} produced uncatalogued sandbox diagnostic kind {kind:?}; add an AcceptedDivergence with reason and diagnostic linkage or fix the parity gap.\nDiagnostics:\n{}",
            case.test_name,
            diagnostics_dump(diagnostics)
        );
    }
}

fn assert_no_error_diagnostics(case: &ParityCase, diagnostics: &[Diagnostic]) {
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.severity != "error"),
        "{} produced sandbox compile errors:\n{}",
        case.test_name,
        diagnostics_dump(diagnostics)
    );
}

fn assert_exit_code_parity(case: &ParityCase, native: &Output, sandbox: &Output) {
    assert_eq!(
        sandbox.status.code(),
        native.status.code(),
        "{} exit-code mismatch\nnative:\n{}\nsandbox:\n{}",
        case.test_name,
        describe_output(native),
        describe_output(sandbox)
    );
}

fn assert_stdout_parity(case: &ParityCase, native: &Output, sandbox: &Output) {
    assert_eq!(
        String::from_utf8_lossy(&sandbox.stdout),
        String::from_utf8_lossy(&native.stdout),
        "{} stdout mismatch\nnative:\n{}\nsandbox:\n{}",
        case.test_name,
        describe_output(native),
        describe_output(sandbox)
    );
}

fn run_native(source_path: &Path) -> Output {
    Command::new(hew_binary())
        .arg("run")
        .arg(source_path)
        .current_dir(repo_root())
        .env("HEWPATH", repo_root())
        .env("HEW_SEED", HEW_SEED)
        .env("NO_COLOR", "1")
        .output()
        .unwrap_or_else(|err| panic!("failed to spawn native `hew run`: {err}"))
}

fn run_sandbox(bytecode_path: &Path) -> Output {
    Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .arg("run")
        .arg("-s")
        .arg("parity:run")
        .arg("--")
        .arg(bytecode_path)
        .arg("--seed")
        .arg(HEW_SEED)
        .current_dir(repo_root())
        .env("NO_COLOR", "1")
        .output()
        .unwrap_or_else(|err| panic!("failed to spawn sandbox parity runner: {err}"))
}

fn ensure_native_toolchain() {
    static NATIVE_TOOLCHAIN: OnceLock<()> = OnceLock::new();
    NATIVE_TOOLCHAIN.get_or_init(|| {
        run_bootstrap_command(
            "cargo build -q -p hew-cli",
            std::process::Command::new("cargo")
                .args(["build", "-q", "-p", "hew-cli"])
                .current_dir(repo_root()),
        );
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

fn ensure_parity_runner_built() {
    static PARITY_RUNNER: OnceLock<()> = OnceLock::new();
    PARITY_RUNNER.get_or_init(|| {
        let vm_dir = repo_root().join("hew-sandbox-vm");
        assert!(
            vm_dir.join("node_modules").is_dir(),
            "hew-sandbox-vm dependencies are not installed; run `npm --prefix hew-sandbox-vm ci` or `make sandbox-parity`"
        );
        run_bootstrap_command(
            "npm --prefix hew-sandbox-vm run -s build",
            std::process::Command::new("npm")
                .arg("--prefix")
                .arg(&vm_dir)
                .arg("run")
                .arg("-s")
                .arg("build")
                .current_dir(repo_root()),
        );
        assert!(
            vm_dir
                .join("dist")
                .join("interpreter")
                .join("parity-runner.js")
                .is_file(),
            "sandbox parity runner was not built at hew-sandbox-vm/dist/interpreter/parity-runner.js"
        );
    });
}

fn run_bootstrap_command(label: &str, command: &mut std::process::Command) {
    let output = command
        .output()
        .unwrap_or_else(|err| panic!("failed to invoke `{label}`: {err}"));
    assert!(
        output.status.success(),
        "`{label}` failed\n{}",
        describe_output(&output)
    );
}

fn hew_binary() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_hew") {
        return PathBuf::from(path);
    }

    target_debug_dir().join(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

fn target_debug_dir() -> PathBuf {
    if let Ok(target_dir) = std::env::var("CARGO_TARGET_DIR") {
        return PathBuf::from(target_dir).join("debug");
    }
    repo_root().join("target").join("debug")
}

fn repo_root() -> &'static Path {
    static REPO_ROOT: OnceLock<PathBuf> = OnceLock::new();
    REPO_ROOT
        .get_or_init(|| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .expect("hew-sandbox-wasm crate should have a workspace parent")
                .to_path_buf()
        })
        .as_path()
}

fn set_test_hewpath() {
    std::env::set_var("HEWPATH", repo_root());
}

fn diagnostics_dump(diagnostics: &[Diagnostic]) -> String {
    serde_json::to_string_pretty(diagnostics).expect("diagnostics should serialize")
}

fn describe_output(output: &Output) -> String {
    format!(
        "status: {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}
