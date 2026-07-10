use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use std::time::Duration;

#[test]
fn actor_e2e_counter_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_counter", 42);
}

#[test]
fn actor_init_on_start_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_counter_init", 42);
}

#[test]
fn actor_on_stop_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_on_stop", 42);
}

#[test]
fn closure_pid_send_compile_and_exit_code() {
    // fn-closure capturing an actor pid and sending through it — the
    // closure-shim builder must carry the module's actor layout tables so
    // the body's send resolves `actor_method_info`. Runs under
    // MallocScribble so a freed-read on the aliased handle is
    // deterministic, not flaky.
    compile_and_run_actor_fixture("actor_closure_pid_send", 42);
}

#[test]
fn network_deadline_arbiters_emit_typed_cancelled_branch() {
    for (fixture, prefix) in [
        ("await_accept_deadline_timeout", "suspending_accept"),
        ("await_read_string_deadline_timeout", "suspending_read"),
    ] {
        let ll = compile_vertical_slice_ll(fixture);
        let compare = ll
            .lines()
            .find(|line| line.contains(&format!("%{prefix}_cancelled = icmp eq i32")))
            .unwrap_or_else(|| panic!("{fixture}: missing Cancelled status compare in IR"));
        assert!(
            compare.ends_with(", 2"),
            "{fixture}: Cancelled compare must test AwaitCancelStatus::Cancelled (2): {compare}"
        );

        let block_start = ll
            .find(&format!("\n{prefix}_cancelled"))
            .map(|index| index + 1)
            .unwrap_or_else(|| panic!("{fixture}: missing Cancelled basic block in IR"));
        let block = ll[block_start..].split("\n\n").next().unwrap_or_default();
        assert!(
            block.contains("store i8 4"),
            "{fixture}: Cancelled block must store NetError::Cancelled tag 4:\n{block}"
        );
    }
}

/// Drop-plan oracle (the truth standard): capturing a pid into a closure
/// must add ZERO drops to the parent's emitted drop plan. The pid has no
/// drop glue, so the closure fixture's `main` plan must be
/// signature-identical (drop count + every `drop_fn`/`kind` line) to the
/// no-closure baseline `actor_counter`, whose body is the same program
/// without the closure.
#[test]
fn closure_pid_capture_adds_no_drops_to_parent_plan() {
    let repo = repo_root();
    ensure_codegen_artifacts(&repo);

    let closure_dump = dump_elab_mir(&repo, "tests/vertical-slice/accept/closure_pid_send.hew");
    let baseline_dump = dump_elab_mir(&repo, "tests/vertical-slice/accept/actor_counter.hew");

    let closure_sig = main_drop_signature(&closure_dump);
    let baseline_sig = main_drop_signature(&baseline_dump);
    assert_eq!(
        closure_sig, baseline_sig,
        "closure pid capture changed the parent's drop plan; the capture is \
         a BitCopy alias and must add zero drops"
    );
    // Every drop in the plan is the pid's no-op resource drop — the plan
    // never grows a real release for an aliased handle.
    // Accept both the structured text format (`kind=resource`) and the
    // legacy Debug format (`kind: Resource` / `drop_fn: None` / `ElabDrop`).
    assert!(
        closure_sig.iter().all(|line| line.contains("drop_fn: None")
            || line.contains("kind: Resource")
            || line.contains("kind=resource")
            || line.starts_with("ElabDrop")),
        "unexpected drop signature lines: {closure_sig:?}"
    );
}

/// Codegen oracle for the lambda env: the synthesized state dropper for a
/// pid-capturing lambda actor frees the env bytes ONLY — no per-field
/// release call for the pid field (LambdaEnvFieldDrop::None).
#[test]
fn lambda_pid_capture_env_drop_frees_bytes_only() {
    let repo = repo_root();
    ensure_codegen_artifacts(&repo);

    let emit_dir = tempfile::Builder::new()
        .prefix("hew-lambda-pid-env-drop-")
        .tempdir()
        .expect("create emit dir");
    let compile = Command::new(hew_bin(&repo))
        .current_dir(&repo)
        .args([
            "compile",
            "--emit-dir",
            emit_dir.path().to_str().expect("emit dir UTF-8"),
            "tests/vertical-slice/accept/lambda_capture_pid_forward.hew",
        ])
        .output()
        .expect("run built hew compile");
    assert!(
        compile.status.success(),
        "hew compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let ll = std::fs::read_to_string(emit_dir.path().join("lambda_capture_pid_forward.ll"))
        .expect("read emitted LLVM IR");
    let body_start = ll
        .find("define internal void @__hew_lambda_env_drop_main_0")
        .expect("synthesized lambda env drop fn present in IR");
    let body = &ll[body_start..];
    let body_end = body.find("\n}").expect("env drop fn body terminated");
    let body = &body[..body_end];
    let calls: Vec<&str> = body.lines().filter(|line| line.contains("call ")).collect();
    assert!(
        calls.len() == 1 && calls[0].contains("@free("),
        "pid-capturing lambda env drop must free bytes only (no per-field \
         release for the no-drop pid field); got calls: {calls:?}\nbody:\n{body}"
    );
}

/// Run `hew compile --dump-mir elab` on a repo-relative source and return
/// the dump text.
fn dump_elab_mir(repo: &Path, rel_source: &str) -> String {
    let output = Command::new(hew_bin(repo))
        .current_dir(repo)
        .args(["compile", "--dump-mir", "elab", rel_source])
        .output()
        .expect("run built hew compile --dump-mir elab");
    assert!(
        output.status.success(),
        "hew compile --dump-mir elab {rel_source} failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// Extract `main`'s drop signature from an elaborated-MIR dump: one entry
/// per non-`(none)` drop-plan line inside main's function chunk.
/// Local indices and block indices are stripped so the signature compares
/// across programs whose numbering differs.
///
/// Works with both the legacy Debug format (`ElaboratedMirFunction {` /
/// `name: "main"`) and the structured text format (`fn main -> <ty>`).
fn main_drop_signature(dump: &str) -> Vec<String> {
    // Try the structured text format first: functions are separated by
    // lines beginning with "fn " at column 0.
    let structured_chunk: Option<&str> = {
        let mut found: Option<&str> = None;
        let mut start: Option<usize> = None;
        for (byte_pos, line) in line_byte_positions(dump) {
            if line.starts_with("fn ") {
                if let Some(s) = start {
                    if dump[s..].trim_start().starts_with("fn main ") {
                        found = Some(&dump[s..byte_pos]);
                        break;
                    }
                }
                start = Some(byte_pos);
            }
        }
        // Handle main being the last function in the dump.
        if found.is_none() {
            if let Some(s) = start {
                if dump[s..].trim_start().starts_with("fn main ") {
                    found = Some(&dump[s..]);
                }
            }
        }
        found
    };

    if let Some(chunk) = structured_chunk {
        return chunk
            .lines()
            .map(str::trim)
            .filter(|line| {
                // Collect every actual drop entry in the drop_plans section;
                // skip `(none)` entries and block/terminator header lines.
                line.starts_with("drop ") && line.contains("kind=")
            })
            .map(|line| {
                // Strip the local name token (position-specific) so that the
                // signature can be compared across programs with different
                // local naming. Keep ty=… and kind=… which are the
                // structural facts we care about.
                normalise_drop_line(line)
            })
            .collect();
    }

    // Fallback: legacy Debug format.
    let main_chunk = dump
        .split("ElaboratedMirFunction {")
        .find(|chunk| chunk.trim_start().starts_with("name: \"main\""))
        .expect("dump contains an ElaboratedMirFunction named main (structured or Debug format)");
    main_chunk
        .lines()
        .map(str::trim)
        .filter(|line| {
            line.starts_with("ElabDrop")
                || line.starts_with("drop_fn:")
                || (line.starts_with("kind:") && line.contains("Resource"))
        })
        .map(ToString::to_string)
        .collect()
}

/// Iterate over the (byte_offset, line_str) pairs for a string without
/// allocating a line buffer.
fn line_byte_positions(s: &str) -> impl Iterator<Item = (usize, &str)> {
    let mut offset = 0usize;
    s.split('\n').map(move |line| {
        let pos = offset;
        offset += line.len() + 1; // +1 for the '\n'
        (pos, line)
    })
}

/// Remove the local-specific name token from a structured drop line so that
/// two programs with the same *types* but different *names* compare equal.
///
/// Input:  `drop actor2 ty=LocalPid<Counter> kind=resource`
/// Output: `drop ty=LocalPid<Counter> kind=resource`
fn normalise_drop_line(line: &str) -> String {
    // The format is: `drop <name> ty=… kind=…`
    // We want: `drop ty=… kind=…`
    let after_drop = line.strip_prefix("drop ").unwrap_or(line);
    // Skip the name token (first whitespace-delimited word).
    let rest = after_drop
        .find(' ')
        .map(|i| after_drop[i..].trim_start())
        .unwrap_or(after_drop);
    format!("drop {rest}")
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
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

fn hew_bin(repo: &Path) -> PathBuf {
    target_dir(repo)
        .join("debug")
        .join(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

fn hew_lib(repo: &Path) -> PathBuf {
    let lib_name = if cfg!(windows) { "hew.lib" } else { "libhew.a" };
    target_dir(repo).join("debug").join(lib_name)
}

fn ensure_codegen_artifacts(repo: &Path) {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let hew = hew_bin(repo);
        let lib = hew_lib(repo);
        if hew.is_file() && lib.is_file() {
            return;
        }

        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let output = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-cli", "-p", "hew-lib"])
            .output()
            .expect("spawn cargo build -p hew-cli -p hew-lib");
        assert!(
            output.status.success(),
            "cargo build -p hew-cli -p hew-lib failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            hew.is_file(),
            "codegen bootstrap succeeded but hew binary was not created at {}",
            hew.display()
        );
        assert!(
            lib.is_file(),
            "codegen bootstrap succeeded but Hew library was not created at {}",
            lib.display()
        );
    });
}

fn compile_vertical_slice_ll(fixture_name: &str) -> String {
    let repo = repo_root();
    ensure_codegen_artifacts(&repo);
    let emit_dir = tempfile::Builder::new()
        .prefix(&format!("hew-cancelled-arbiter-{fixture_name}-"))
        .tempdir()
        .expect("create cancelled-arbiter emit dir");
    let source = format!("tests/vertical-slice/accept/{fixture_name}.hew");
    let compile = Command::new(hew_bin(&repo))
        .current_dir(&repo)
        .args([
            "compile",
            "--emit-dir",
            emit_dir
                .path()
                .to_str()
                .expect("emit dir path is valid UTF-8"),
            &source,
        ])
        .output()
        .expect("run built hew compile");
    assert!(
        compile.status.success(),
        "hew compile {source} failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    std::fs::read_to_string(emit_dir.path().join(format!("{fixture_name}.ll")))
        .expect("read emitted cancelled-arbiter LLVM IR")
}

/// Run a compiled fixture, killing it if it does not exit within `secs`.
///
/// The actor fixtures can deadlock at runtime; a bare `Command::output()`
/// would hang the test harness forever and orphan the child process. This
/// bounds the wait and reaps the child so a hang surfaces as a test failure
/// instead of a leaked process.
fn run_bounded(binary: &Path, secs: u64) -> std::process::ExitStatus {
    let mut command = Command::new(binary);
    // Scribble freed allocations (macOS libmalloc; inert elsewhere) so a
    // read-after-free in actor teardown is a deterministic crash here, not
    // a flaky pass on stale-but-intact heap bytes.
    command.env("MallocScribble", "1");
    hew_testutil::run_command_bounded(
        &mut command,
        format!("actor fixture {}", binary.display()),
        Duration::from_secs(secs),
    )
    .unwrap_or_else(|error| panic!("{error}"))
    .status
}

fn compile_and_run_actor_fixture(fixture_name: &str, expected_exit_code: i32) {
    let repo = repo_root();
    ensure_codegen_artifacts(&repo);

    let emit_dir = tempfile::Builder::new()
        .prefix(&format!("hew-actor-e2e-{fixture_name}-"))
        .tempdir()
        .expect("create actor fixture emit dir");

    let compile = Command::new(hew_bin(&repo))
        .current_dir(&repo)
        .args([
            "compile",
            "--emit-dir",
            emit_dir
                .path()
                .to_str()
                .expect("emit dir path is valid UTF-8"),
            &format!("examples/v05/{fixture_name}.hew"),
        ])
        .output()
        .expect("run built hew compile");
    assert!(
        compile.status.success(),
        "hew compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let stdout = String::from_utf8_lossy(&compile.stdout);
    let binary = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            panic!("hew compile stdout did not contain a `native:` line:\n{stdout}")
        });
    assert!(
        binary.is_file(),
        "hew compile reported native binary {}, but it does not exist",
        binary.display()
    );

    let status = run_bounded(&binary, 30);
    assert_eq!(status.code(), Some(expected_exit_code));
}
