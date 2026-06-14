//! Allowlist parity-coverage ratchet (SP-3 / closes G7).
//!
//! The sandbox profile (`profile.rs`) admits a set of language constructs:
//! anything it does not fail-closed-reject compiles to bytecode and is handed to
//! the TS VM. Historically, parity was enforced over exactly 11 hand-picked
//! examples — so a construct that the profile admits but that the emitter or
//! interpreter mis-handles could silently diverge from native while the suite
//! stayed green (the float-as-i64 bug, G1, is exactly this class: admitted,
//! unparited, wrong).
//!
//! This file makes the parity set a **ratchet over the admitted surface** rather
//! than a sample. It encodes a manifest of every admitted construct, each tagged
//! with its coverage:
//!
//! - [`Coverage::Parity`] — the construct runs at native↔sandbox parity and is
//!   pinned to a required parity case in `parity.rs`. The cross-check test
//!   asserts that case name is in `REQUIRED_PARITY_TEST_NAMES`, so admitting a
//!   construct and claiming it runs **without** a parity case is a build error.
//! - [`Coverage::NotYetRunnable`] — the profile admits the construct, but the
//!   emitter/interpreter cannot yet run it: it traps (`unsupported_instruction`
//!   / `invalid_enum_tag`) or fails to lower. These are catalogued, not hidden.
//!   A self-verifying test compiles+runs each one and asserts it really does NOT
//!   run at parity. The moment a graduation lane makes one runnable, that test
//!   fails — forcing the lane to promote the entry to `Parity` and add its case.
//!
//! Together these close G7 in both directions: you cannot mark a construct
//! runnable without a green parity case, and you cannot make a catalogued hole
//! runnable without updating the manifest + adding a case.

use std::path::{Path, PathBuf};
use std::process::Output;
use std::sync::OnceLock;

use hew_sandbox_wasm::{compile_to_sandbox_bytecode, REQUIRED_PARITY_TEST_NAMES};

const SANDBOX_PROFILE: &str = "sandbox-vm-export";
const HEW_SEED: &str = "42";

/// How a profile-admitted construct is covered by the parity ratchet.
#[derive(Debug, Clone, Copy)]
enum Coverage {
    /// Runs at native↔sandbox parity; pinned to this required parity-case name.
    Parity(&'static str),
    /// Admitted by the profile but not yet runnable: the emitter/interpreter
    /// traps or fails to lower. Catalogued with the observed failure so it can
    /// never silently masquerade as runnable. The `probe` source is compiled +
    /// run by `not_yet_runnable_constructs_do_not_run_at_parity` to keep the
    /// catalogue honest.
    NotYetRunnable {
        /// Observed failure when run in the sandbox today.
        failure: Failure,
        /// A minimal program that exercises (only) this construct.
        probe: &'static str,
    },
}

/// The way an admitted-but-not-yet-runnable construct fails in the sandbox.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Failure {
    /// Compiles to bytecode, then the VM traps at run time.
    Trap,
    /// The emitter fails closed with a typed diagnostic (no bytecode). This is
    /// the desired shape for a partially-admitted construct, but still "not
    /// runnable" for ratchet purposes.
    NoBytecode,
}

/// One profile-admitted construct and its parity coverage.
#[derive(Debug, Clone, Copy)]
struct AdmittedConstruct {
    /// Stable identifier for the construct (for assertion messages).
    id: &'static str,
    coverage: Coverage,
}

/// The manifest of admitted constructs. Grown by `profile.rs` audit (SP-3).
///
/// INVARIANT: every construct the profile admits appears here exactly once,
/// classified `Parity` (with a required case) or `NotYetRunnable` (with a probe
/// that demonstrably does not run). Adding admission in `profile.rs` without a
/// row here leaves a coverage blind spot — review must add the row.
const ADMITTED_CONSTRUCTS: &[AdmittedConstruct] = &[
    // ---- Runs at parity (pinned to a required case) ----
    AdmittedConstruct {
        id: "literal+println (hello world)",
        coverage: Coverage::Parity("hello_world"),
    },
    AdmittedConstruct {
        id: "recursive fn call + expr-if + range-for + interpolation",
        coverage: Coverage::Parity("fibonacci"),
    },
    AdmittedConstruct {
        id: "enum unit-variant construction + dispatch (compose)",
        coverage: Coverage::Parity("function_composition"),
    },
    AdmittedConstruct {
        id: "match with constructor-payload patterns",
        coverage: Coverage::Parity("pattern_matching"),
    },
    AdmittedConstruct {
        id: "Vec::new + push/get/len",
        coverage: Coverage::Parity("collections"),
    },
    AdmittedConstruct {
        id: "record type StructInit + field access",
        coverage: Coverage::Parity("record_types"),
    },
    AdmittedConstruct {
        id: "nested expr-if returning string",
        coverage: Coverage::Parity("structural_records"),
    },
    AdmittedConstruct {
        id: "actor spawn + receive + mutable state",
        coverage: Coverage::Parity("counter_actor"),
    },
    AdmittedConstruct {
        id: "actor ask via await + Ok/Err reply match",
        coverage: Coverage::Parity("actor_pipeline"),
    },
    AdmittedConstruct {
        id: "supervisor decl + child access",
        coverage: Coverage::Parity("supervisor"),
    },
    AdmittedConstruct {
        id: "machine new/step/state_name",
        coverage: Coverage::Parity("traffic_light"),
    },
    AdmittedConstruct {
        id: "integer arithmetic + comparison + unary negate",
        coverage: Coverage::Parity("arithmetic_operators"),
    },
    AdmittedConstruct {
        id: "array literal + index + len",
        coverage: Coverage::Parity("array_indexing"),
    },
    AdmittedConstruct {
        id: "string len + slice",
        coverage: Coverage::Parity("string_slicing"),
    },
    AdmittedConstruct {
        id: "while loop",
        coverage: Coverage::Parity("while_loop"),
    },
    AdmittedConstruct {
        id: "match with wildcard arm",
        coverage: Coverage::Parity("wildcard_match"),
    },
    // ---- Admitted by the profile but NOT yet runnable (G7 holes) ----
    // Each traps today; catalogued so it cannot silently regress to "looks
    // green". See docs/sandbox-vm-divergences.md for the narrative.
    AdmittedConstruct {
        id: "scalar-literal match (i64 scrutinee)",
        coverage: Coverage::NotYetRunnable {
            // lower_match lowers EVERY match as enum.tag dispatch; a non-enum
            // scrutinee traps invalid_enum_tag.
            failure: Failure::Trap,
            probe: "fn pick(n: i64) -> string {\n    match n {\n        1 => \"one\",\n        _ => \"many\",\n    }\n}\nfn main() {\n    println(pick(1));\n}\n",
        },
    },
    AdmittedConstruct {
        id: "scalar-literal match (string scrutinee)",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            probe: "fn code(s: string) -> i64 {\n    match s {\n        \"a\" => 1,\n        _ => 0,\n    }\n}\nfn main() {\n    println(code(\"a\"));\n}\n",
        },
    },
    AdmittedConstruct {
        id: "bool match",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            probe: "fn f(b: bool) -> i64 {\n    match b {\n        true => 1,\n        false => 0,\n    }\n}\nfn main() {\n    println(f(true));\n}\n",
        },
    },
    AdmittedConstruct {
        id: "tuple value + tuple-let destructure",
        coverage: Coverage::NotYetRunnable {
            // No Expr::Tuple emit arm -> emit_unsupported -> trap.
            failure: Failure::Trap,
            probe: "fn main() {\n    let t = (1, 2, 3);\n    let (a, b, c) = t;\n    println(a + b + c);\n}\n",
        },
    },
    AdmittedConstruct {
        id: "expression-position if-let",
        coverage: Coverage::NotYetRunnable {
            // No Expr::IfLet emit arm -> emit_unsupported -> trap.
            failure: Failure::Trap,
            probe: "enum Box { Has(i64); Empty; }\nfn main() {\n    let b = Has(9);\n    let v = if let Has(x) = b { x } else { 0 };\n    println(v);\n}\n",
        },
    },
    AdmittedConstruct {
        id: "numeric cast (`as`)",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            probe: "fn main() {\n    let x: i64 = 65;\n    let c = x as i32;\n    println(c);\n}\n",
        },
    },
    AdmittedConstruct {
        id: "postfix-try (`?`)",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            probe: "fn ok() -> Result<i64, string> {\n    Ok(1)\n}\nfn run() -> Result<i64, string> {\n    let v = ok()?;\n    Ok(v + 1)\n}\nfn main() {\n    let r = match run() {\n        Ok(v) => v,\n        Err(_) => -1,\n    };\n    println(r);\n}\n",
        },
    },
    AdmittedConstruct {
        id: "Option Some/None construction",
        coverage: Coverage::NotYetRunnable {
            // Some/None are not pre-registered in enum_variant_tags -> the
            // constructor call hits the catch-all -> emit_unsupported -> trap.
            failure: Failure::Trap,
            probe: "fn main() {\n    let o = Some(5);\n    let v = match o {\n        Some(x) => x,\n        None => 0,\n    };\n    println(v);\n}\n",
        },
    },
    AdmittedConstruct {
        id: "struct pattern in match arm",
        coverage: Coverage::NotYetRunnable {
            // lower_match early-rejects Struct/Tuple arm patterns.
            failure: Failure::Trap,
            probe: "type Point { x: i64; y: i64; }\nfn sum(p: Point) -> i64 {\n    match p {\n        Point { x: a, y: b } => a + b,\n    }\n}\nfn main() {\n    println(sum(Point { x: 3, y: 4 }));\n}\n",
        },
    },
    AdmittedConstruct {
        id: "const item reference",
        coverage: Coverage::NotYetRunnable {
            // Const value is not bound; the identifier reference hits the
            // identifier catch-all -> emit_unsupported -> trap (prints unit).
            failure: Failure::Trap,
            probe: "const LIMIT: i64 = 100;\nfn main() {\n    println(LIMIT);\n}\n",
        },
    },
];

/// Cross-check (the G7 teeth): every construct claimed to run at parity is
/// pinned to a name in `REQUIRED_PARITY_TEST_NAMES`. A profile admission that
/// claims `Parity(name)` for a name with no required case fails the build.
#[test]
fn every_runnable_admitted_construct_has_a_required_parity_case() {
    let required: std::collections::BTreeSet<&str> =
        REQUIRED_PARITY_TEST_NAMES.iter().copied().collect();
    for construct in ADMITTED_CONSTRUCTS {
        if let Coverage::Parity(case) = construct.coverage {
            assert!(
                required.contains(case),
                "admitted construct `{}` claims parity case `{case}`, but no such name is in \
                 REQUIRED_PARITY_TEST_NAMES; add the parity case or fix the manifest",
                construct.id
            );
        }
    }
}

/// Inverse teeth: no required parity name is orphaned. Every name in the
/// required set is claimed by at least one admitted-construct row, so a parity
/// case cannot exist without a construct it proves.
#[test]
fn every_required_parity_case_backs_an_admitted_construct() {
    let claimed: std::collections::BTreeSet<&str> = ADMITTED_CONSTRUCTS
        .iter()
        .filter_map(|c| match c.coverage {
            Coverage::Parity(name) => Some(name),
            Coverage::NotYetRunnable { .. } => None,
        })
        .collect();
    for name in REQUIRED_PARITY_TEST_NAMES {
        assert!(
            claimed.contains(name),
            "required parity case `{name}` is not claimed by any admitted-construct row in \
             ADMITTED_CONSTRUCTS; every parity case must prove a construct"
        );
    }
}

/// The manifest's runnable count only grows. A stored baseline guards against
/// silently dropping coverage (e.g. deleting a `Parity` row to dodge a failing
/// case). Bumping this is a deliberate, reviewed act — never lower it.
#[test]
fn runnable_construct_coverage_does_not_shrink() {
    const RUNNABLE_BASELINE: usize = 16;
    let runnable = ADMITTED_CONSTRUCTS
        .iter()
        .filter(|c| matches!(c.coverage, Coverage::Parity(_)))
        .count();
    assert!(
        runnable >= RUNNABLE_BASELINE,
        "runnable admitted-construct coverage dropped to {runnable}; the ratchet only grows. \
         If you intentionally removed a construct's admission in profile.rs, lower the baseline \
         in the same commit with justification."
    );
}

/// Keeps the catalogue honest (the inverse ratchet). Every `NotYetRunnable`
/// construct, compiled and run in the sandbox today, must genuinely NOT run at
/// parity — it must trap or fail to lower. If a graduation lane makes one of
/// these runnable, this test fails, forcing the lane to promote the entry to
/// `Parity` and add a real parity case (so the construct cannot become runnable
/// without joining the ratchet).
///
/// This is the structural guarantee against a future G1: a construct cannot be
/// "admitted + runnable + unparited" because making it runnable trips this test.
#[cfg_attr(windows, ignore)]
#[test]
fn not_yet_runnable_constructs_do_not_run_at_parity() {
    ensure_parity_runner_built();
    for construct in ADMITTED_CONSTRUCTS {
        let Coverage::NotYetRunnable { failure, probe } = construct.coverage else {
            continue;
        };
        let compiled = compile_to_sandbox_bytecode(probe, Some(SANDBOX_PROFILE))
            .unwrap_or_else(|err| panic!("sandbox compile threw for `{}`: {err}", construct.id));

        let has_errors = compiled.diagnostics.iter().any(|d| d.severity == "error");
        let Some(bytecode) = compiled.bytecode.as_ref().filter(|_| !has_errors) else {
            // No bytecode (fail-closed). Honest for NoBytecode; an unexpected
            // reject for a Trap row also means "not runnable", but flag the
            // mismatch so the catalogue stays accurate.
            assert_eq!(
                failure,
                Failure::NoBytecode,
                "construct `{}` is catalogued as Trap but the profile/emitter rejected it \
                 (no bytecode); update the row to Failure::NoBytecode",
                construct.id
            );
            continue;
        };
        assert_eq!(
            failure,
            Failure::Trap,
            "construct `{}` is catalogued as NoBytecode but it compiled to bytecode; \
             update the row to Failure::Trap or promote it to Parity with a case",
            construct.id
        );

        let sandbox = run_sandbox_inline(&serde_json::to_string(bytecode).expect("serialize"));
        assert_ne!(
            sandbox.status.code(),
            Some(0),
            "construct `{}` is catalogued NotYetRunnable, but the sandbox VM ran it cleanly \
             (exit 0). It is now runnable — promote it to Coverage::Parity and add a required \
             parity case so it joins the ratchet.\nsandbox stdout:\n{}\nsandbox stderr:\n{}",
            construct.id,
            String::from_utf8_lossy(&sandbox.stdout),
            String::from_utf8_lossy(&sandbox.stderr)
        );
    }
}

fn run_sandbox_inline(bytecode_json: &str) -> Output {
    let tempdir = tempfile::tempdir().expect("create tempdir");
    let bytecode_path = tempdir.path().join("bytecode.json");
    std::fs::write(&bytecode_path, bytecode_json).expect("write bytecode");
    assert_cmd::Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .arg("run")
        .arg("-s")
        .arg("parity:run")
        .arg("--")
        .arg(&bytecode_path)
        .arg("--seed")
        .arg(HEW_SEED)
        .current_dir(repo_root())
        .env("NO_COLOR", "1")
        .output()
        .expect("spawn sandbox parity runner")
}

fn ensure_parity_runner_built() {
    static PARITY_RUNNER: OnceLock<()> = OnceLock::new();
    PARITY_RUNNER.get_or_init(|| {
        let vm_dir = repo_root().join("hew-sandbox-vm");
        assert!(
            vm_dir.join("node_modules").is_dir(),
            "hew-sandbox-vm dependencies are not installed; run `make sandbox-parity`"
        );
        let output = std::process::Command::new("npm")
            .arg("--prefix")
            .arg(&vm_dir)
            .arg("run")
            .arg("-s")
            .arg("build")
            .current_dir(repo_root())
            .output()
            .expect("invoke npm run build");
        assert!(
            output.status.success(),
            "npm run build failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            vm_dir
                .join("dist")
                .join("interpreter")
                .join("parity-runner.js")
                .is_file(),
            "sandbox parity runner was not built"
        );
    });
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
