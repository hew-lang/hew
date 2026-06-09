//! Regression tests: zero-arg module-qualified enum types (`fs.IoError`) must
//! resolve through the LLVM type table without tripping D10.
//!
//! Root cause: `resolve_ty` only fell back to the short (unqualified) name
//! when `args.is_empty()` was false.  A zero-arg module-qualified type like
//! `fs.IoError` therefore missed the fallback and fell through to the D10
//! fail-closed arm.  `is_indirect_enum` already did the unconditional fallback;
//! `resolve_ty` now mirrors that pattern.
//!
//! Tests here invoke `hew run` end-to-end (parse → module-graph → typecheck
//! → HIR → MIR → LLVM emit → link → exec) because `hew_parser::parse` alone
//! returns `module_graph: None`, so the stdlib-function MIR (which drags
//! `fs.IoError` into codegen) is only lowered when the full pipeline runs.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs must have a workspace parent")
        .to_path_buf()
}

fn target_dir(repo: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || repo.join("target"),
        |dir| {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                path
            } else {
                repo.join(path)
            }
        },
    )
}

fn ensure_hew_runtime_lib(repo: &Path) {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let lib = target_dir(repo).join("debug").join("libhew.a");
        if lib.exists() {
            return;
        }
        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .status()
            .expect("spawn cargo build -p hew-lib");
        assert!(
            status.success(),
            "cargo build -p hew-lib failed: {status:?}"
        );
    });
}

fn hew_command(repo: &Path) -> Command {
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut cmd = Command::new(cargo);
    cmd.current_dir(repo)
        .args(["run", "--quiet", "-p", "hew-cli", "--bin", "hew", "--"]);
    cmd
}

/// Write `source` to a temp file and run `hew run <file>`.
/// Returns `(exit_success, stdout, stderr)`.
fn run_hew_source_raw(repo: &Path, stem: &str, source: &str) -> (bool, String, String) {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!("hew-zeroarg-enum-{stem}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let output = hew_command(repo)
        .arg("run")
        .arg(&path)
        .output()
        .expect("spawn hew run");

    (
        output.status.success(),
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

/// `import std::net; fn main() -> i64 { 0 }` compiles and runs: `net` drags
/// `into_stream_sink -> (Stream<bytes>, Sink<bytes>)` — a tuple carrying two
/// opaque handles — into MIR, and the W5.021 tuple-of-owned-handles drop spine
/// gives that return a safe ABI (the composite-return gate admits it via
/// `is_heap_owning_tuple_composite_return`, and the per-element
/// `DropKind::TupleInPlace` drop closes each handle exactly once).
///
/// This test originally asserted success once the D10 zero-arg short-name
/// fallback was fixed (the `fs.IoError` bug), then was flipped to fail-closed
/// when the handle-pair ABI was still missing. With the drop spine landed it
/// returns to asserting success — and continues to prove D10 is NOT the error.
#[test]
fn import_std_net_empty_main_admits_handle_tuple_with_drop_spine() {
    let repo = repo_root();
    let (ok, _stdout, stderr) = run_hew_source_raw(
        &repo,
        "net_empty_main",
        "import std::net;\nfn main() -> i64 { 0 }\n",
    );
    assert!(
        ok,
        "`import std::net; fn main() -> i64 {{ 0 }}` must compile and run now \
         that the tuple-of-handles return has a drop spine;\nstderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("D10 violation"),
        "must NOT produce a D10 violation (D10 short-name fix is intact);\nstderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("requires tag-aware"),
        "the heap-owning-tuple gate must no longer reject the handle-pair return; \
         got:\nstderr:\n{stderr}"
    );
}

/// `import std::fs; fn main() -> i64 { 0 }` must compile and run, exit 0.
///
/// Same root cause: fs's own `try_*` functions return `Result<_, IoError>`
/// and drag `fs.IoError` into MIR even when unused.
#[test]
fn import_std_fs_empty_main_runs_without_d10() {
    let repo = repo_root();
    let (ok, _stdout, stderr) = run_hew_source_raw(
        &repo,
        "fs_empty_main",
        "import std::fs;\nfn main() -> i64 { 0 }\n",
    );
    assert!(
        ok,
        "`import std::fs; fn main() -> i64 {{ 0 }}` must exit 0;\nstderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("D10 violation"),
        "must not produce a D10 violation;\nstderr:\n{stderr}"
    );
}

/// A non-trivial `std::fs` program using `fs.exists` must run correctly.
///
/// `fs.exists` returns `bool` and is one of the simplest fs operations.
/// This proves not just that the enum resolves, but that a real stdlib
/// function call compiles and links.
#[test]
fn import_std_fs_exists_runs_and_returns_bool() {
    let repo = repo_root();
    let (ok, _stdout, stderr) = run_hew_source_raw(
        &repo,
        "fs_exists",
        r#"
        import std::fs;
        fn main() -> i64 {
            // Calling fs.exists proves the function compiles and links.
            // The return value is discarded; main returns 0 so hew run exits 0.
            let _found = fs.exists("/");
            0
        }
        "#,
    );
    assert!(
        ok,
        "`fs.exists(\"/\")` must compile and run;\nstderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("D10 violation"),
        "must not produce a D10 violation;\nstderr:\n{stderr}"
    );
}

/// Constructing AND matching an imported enum-with-data variant at the importer
/// must round-trip end to end.
///
/// This is the layer below the D10 type-resolution tests above: even once
/// `fs.IoError` resolves through `resolve_ty`, the `Place::MachineTag` /
/// `Place::MachineVariant` codegen looks the layout up in
/// `IrPipeline.machine_layouts`. Monomorphic enums register under their bare
/// declaration name (`IoError`), but a Place at an importer carries the
/// module-qualified name (`fs.IoError`); without the short-name fallback in
/// `machine_layout_for_local` this fails closed with
/// `E_NOT_YET_IMPLEMENTED: machine fs.IoError ... not in
/// IrPipeline.machine_layouts`. Here we both construct (`Err(IoError::...)`)
/// and match (binding the i64 payload) the imported variant.
#[test]
fn construct_and_match_imported_ioerror_variant_round_trips() {
    let repo = repo_root();
    let (ok, stdout, stderr) = run_hew_source_raw(
        &repo,
        "ioerror_round_trip",
        r#"
        import std::fs;
        fn make_err() -> Result<i64, fs.IoError> {
            Err(IoError::ConnectionRefused(111))
        }
        fn main() {
            match make_err() {
                Ok(v) => println(f"ok: {v}"),
                Err(e) => match e {
                    IoError::ConnectionRefused(code) => println(f"refused {code}"),
                    _ => println("other"),
                },
            }
        }
        "#,
    );
    assert!(
        ok,
        "constructing+matching imported `fs.IoError` must compile and run;\nstderr:\n{stderr}"
    );
    assert!(
        stdout.contains("refused 111"),
        "matched arm must bind the payload and run; stdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("not in IrPipeline.machine_layouts"),
        "must not fail closed on the machine-layout lookup;\nstderr:\n{stderr}"
    );
}

/// The same round-trip across the `std::net` surface that motivated the fix:
/// `net.try_connect` returns `Result<_, fs.IoError>`; connecting to a closed
/// port on localhost yields `ConnectionRefused`, matched at the importer.
#[test]
fn net_try_connect_error_match_round_trips() {
    let repo = repo_root();
    let (ok, stdout, stderr) = run_hew_source_raw(
        &repo,
        "net_try_connect_match",
        r#"
        import std::net;
        fn main() {
            match net.try_connect("127.0.0.1:1") {
                Ok(_) => println("connected"),
                Err(e) => match e {
                    IoError::ConnectionRefused(_) => println("refused"),
                    _ => println("other"),
                },
            }
        }
        "#,
    );
    assert!(
        ok,
        "`net.try_connect` + match on IoError must compile and run;\nstderr:\n{stderr}"
    );
    assert!(
        stdout.contains("refused") || stdout.contains("other"),
        "an error arm must be taken (port 1 is not listening); stdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("not in IrPipeline.machine_layouts"),
        "must not fail closed on the machine-layout lookup;\nstderr:\n{stderr}"
    );
}

/// An imported enum carrying an OWNED (string) payload must round-trip too,
/// proving the drop spine for the bound payload is correct across the import
/// boundary. `json.ParseError::Invalid(string)` is constructed in the json
/// module by `json.try_parse` on bad input and matched at the importer.
#[test]
fn imported_enum_owned_string_payload_round_trips() {
    let repo = repo_root();
    let (ok, stdout, stderr) = run_hew_source_raw(
        &repo,
        "json_parseerror_match",
        r#"
        import std::encoding::json;
        fn main() {
            match json.try_parse("{ not valid") {
                Ok(_) => println("parsed"),
                Err(e) => match e {
                    ParseError::Invalid(msg) => println(f"invalid: {msg}"),
                },
            }
        }
        "#,
    );
    assert!(
        ok,
        "matching imported `json.ParseError::Invalid(string)` must run;\nstderr:\n{stderr}"
    );
    assert!(
        stdout.contains("invalid:"),
        "the owned-string payload must bind and print; stdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("not in IrPipeline.machine_layouts"),
        "must not fail closed on the machine-layout lookup;\nstderr:\n{stderr}"
    );
}

/// A genuinely unknown named type still fails closed with a D10 diagnostic.
///
/// This pins the negative: the short-name fallback must not weaken D10 for
/// types that are truly unregistered.  We inject a synthetic local of type
/// `NeverDeclared` directly into the MIR pipeline, bypassing the checker
/// (which would catch it earlier).
#[test]
fn unknown_named_type_still_fails_closed_with_d10() {
    use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
    use hew_mir::{
        BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
        ExitPath, IrPipeline, RawMirFunction, Terminator,
    };
    use hew_types::ResolvedTy;

    let never_ty = ResolvedTy::Named {
        name: "NeverDeclared".into(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };

    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    }];

    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "bad_fn".into(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![never_ty.clone()],
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "bad_fn".into(),
            return_ty: ResolvedTy::I64,
            blocks: raw_blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "bad_fn".into(),
            return_ty: ResolvedTy::I64,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: vec![],
    };

    let tmp = std::env::temp_dir().join(format!("hew-d10-failclosed-{}", std::process::id()));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "d10_fail_closed",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };

    let result = emit_module(&pipeline, &options);
    assert!(
        matches!(
            result,
            Err(CodegenError::FailClosed(ref msg))
                if msg.contains("D10") && msg.contains("NeverDeclared")
        ),
        "codegen must fail closed with a D10 message for NeverDeclared; got: {result:?}"
    );
}
