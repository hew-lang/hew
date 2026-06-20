mod support;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use serde::Deserialize;
use support::{hew_binary, repo_root, require_wasi_runner, run_hew_in};

#[derive(Debug, Deserialize)]
struct Capabilities {
    wasi: String,
}

#[derive(Debug, Deserialize)]
struct PlaygroundEntry {
    id: String,
    source_path: PathBuf,
    expected_path: PathBuf,
    capabilities: Capabilities,
}

fn playground_root() -> PathBuf {
    repo_root().join("examples").join("playground")
}

fn load_playground_manifest() -> Vec<PlaygroundEntry> {
    let manifest_path = playground_root().join("manifest.json");
    let manifest = fs::read_to_string(&manifest_path).expect("read playground manifest");
    serde_json::from_str(&manifest).expect("parse playground manifest")
}

fn run_wasi_example(source: &Path) -> Output {
    let source = source
        .to_str()
        .expect("wasi example path must be valid UTF-8");
    run_hew_in(repo_root(), &["run", source, "--target", "wasm32-wasi"])
}

// Exact set of curated playground entries that declare wasi: "unsupported".
// Update this constant AND scripts/gen-playground-manifest.py :: WASI_CAPABILITY
// together whenever the unsupported set changes intentionally.  The coverage
// guard in curated_playground_examples_run_under_wasi relies on this to catch
// misclassified manifest entries before they silently drop out of the runnable loop.
const EXPECTED_WASI_UNSUPPORTED: &[&str] = &[
    "concurrency/actor_pipeline",
    "concurrency/async_await",
    "concurrency/counter_actor",
    "concurrency/supervisor",
    "language/string_slicing",
    "machines/traffic_light",
];

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn curated_playground_examples_run_under_wasi() {
    require_wasi_runner();

    let manifest = load_playground_manifest();
    assert_eq!(
        manifest.len(),
        35,
        "expected the curated 35-snippet manifest"
    );

    let mut actual_unsupported: Vec<&str> = manifest
        .iter()
        .filter(|entry| entry.capabilities.wasi == "unsupported")
        .map(|entry| entry.id.as_str())
        .collect();
    actual_unsupported.sort_unstable();

    assert_eq!(
        actual_unsupported, EXPECTED_WASI_UNSUPPORTED,
        "wasi 'unsupported' set mismatch: if a new example is intentionally \
         unsupported update EXPECTED_WASI_UNSUPPORTED in wasi_run_e2e.rs and \
         WASI_CAPABILITY in scripts/gen-playground-manifest.py together; if \
         an example was misclassified, fix its manifest capability instead"
    );

    let runnable: Vec<_> = manifest
        .iter()
        .filter(|entry| entry.capabilities.wasi == "runnable")
        .collect();

    for entry in runnable {
        let source = playground_root().join(&entry.source_path);
        let expected = fs::read_to_string(playground_root().join(&entry.expected_path))
            .expect("read expected output");
        let output = run_wasi_example(&source);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        assert!(
            output.status.success(),
            "hew run --target wasm32-wasi failed for {}\nstdout:\n{}\nstderr:\n{}",
            entry.id,
            stdout,
            stderr,
        );
        assert_eq!(
            stdout.as_ref(),
            expected.as_str(),
            "stdout mismatch for {}\nstderr:\n{}",
            entry.id,
            stderr,
        );
    }
}

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn supervisor_stays_on_the_unsupported_diagnostic_path_under_wasi() {
    require_wasi_runner();

    let manifest = load_playground_manifest();
    let supervisor_entry = manifest
        .iter()
        .find(|entry| entry.id == "concurrency/supervisor")
        .expect("concurrency/supervisor entry in manifest");
    assert_eq!(
        supervisor_entry.capabilities.wasi, "unsupported",
        "concurrency/supervisor must declare wasi capability 'unsupported' in manifest"
    );

    let source = playground_root().join(&supervisor_entry.source_path);
    let output = run_wasi_example(&source);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(125),
        "supervisor should fail in compile phase\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert!(
        stderr.contains("not supported on WASM32"),
        "expected unsupported WASM diagnostic\nstderr:\n{stderr}",
    );
    assert!(
        !stderr.contains("hew.supervisor.new"),
        "supervisor should be rejected before lowering emits runtime symbols\nstderr:\n{stderr}",
    );
}

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn toml_encoding_round_trips_under_wasi() {
    require_wasi_runner();

    let dir = support::tempdir();
    let source = dir.path().join("toml_wasi.hew");
    fs::write(
        &source,
        "import std::encoding::toml;\n\
         \n\
         fn main() {\n\
         \x20   let doc = toml.parse(\"[package]\\nname = \\\"hew\\\"\");\n\
         \x20   let pkg = doc.get_field(\"package\");\n\
         \x20   let name = pkg.get_field(\"name\");\n\
         \x20   println(name.get_string());\n\
         \x20   name.free();\n\
         \x20   pkg.free();\n\
         \x20   doc.free();\n\
         }\n",
    )
    .expect("write TOML WASI parity source");

    let output = run_wasi_example(&source);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "hew run --target wasm32-wasi failed for TOML parity source\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert_eq!(stdout.as_ref(), "hew\n", "stderr:\n{stderr}");
}

// The layout-keyed `HashMap<Record, V>` / `HashSet<string>` runtime ABI
// (`hew_hashmap_*_layout` / `hew_hashset_*_layout`) is target-agnostic: the
// modules are not `cfg`-gated against wasm32 and codegen lowers their callees
// directly (no `WasmUnsupportedSubstrate` arm in `uses_wasm_excluded_symbol`).
// This pins the non-actor map/set surface end-to-end under the real WASI
// runtime with exact output, so a regression that re-gated the family or
// produced wrong-width descriptors would be caught here rather than slipping
// through as a documented-but-unproven claim.
//
// The same program is run natively below; the two stdouts must match
// byte-for-byte (native↔wasm parity).
const HASHMAP_HASHSET_LAYOUT_SOURCE: &str = r#"record Point {
    x: i64,
    y: i64
}

fn main() {
    let m: HashMap<Point, i64> = HashMap::new();
    m.insert(Point { x: 1, y: 2 }, 10);
    m.insert(Point { x: 3, y: 4 }, 20);
    let n = m.len();
    println(f"len={n}");
    let hit = m.get(Point { x: 1, y: 2 });
    match hit {
        Some(got) => println(f"get_present={got}"),
        None => println("get_present=None"),
    }
    let miss = m.get(Point { x: 9, y: 9 });
    match miss {
        Some(got) => println(f"get_absent={got}"),
        None => println("get_absent=None"),
    }
    let has = m.contains_key(Point { x: 3, y: 4 });
    println(f"contains={has}");
    let rm = m.remove(Point { x: 1, y: 2 });
    println(f"remove={rm}");
    let rm_again = m.remove(Point { x: 1, y: 2 });
    println(f"remove_again={rm_again}");
    let n2 = m.len();
    println(f"len_after_remove={n2}");

    let s: HashSet<string> = HashSet::new();
    s.insert("alpha");
    s.insert("beta");
    s.insert("alpha");
    let sn = s.len();
    println(f"set_len={sn}");
    let shas = s.contains("alpha");
    println(f"set_contains={shas}");
    let srm = s.remove("beta");
    println(f"set_remove={srm}");
    let shas_missing = s.contains("beta");
    println(f"set_contains_removed={shas_missing}");
    let sn2 = s.len();
    println(f"set_len_after_remove={sn2}");
}
"#;

const HASHMAP_HASHSET_LAYOUT_EXPECTED: &str = "len=2\n\
     get_present=10\n\
     get_absent=None\n\
     contains=true\n\
     remove=true\n\
     remove_again=false\n\
     len_after_remove=1\n\
     set_len=2\n\
     set_contains=true\n\
     set_remove=true\n\
     set_contains_removed=false\n\
     set_len_after_remove=1\n";

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn wasm_hashmap_hashset_layout_run_pass() {
    require_wasi_runner();

    let dir = support::tempdir();
    let source = dir.path().join("hashmap_hashset_layout_wasi.hew");
    fs::write(&source, HASHMAP_HASHSET_LAYOUT_SOURCE).expect("write map/set layout WASI source");

    let output = run_wasi_example(&source);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "hew run --target wasm32-wasi failed for HashMap/HashSet layout source\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert_eq!(
        stdout.as_ref(),
        HASHMAP_HASHSET_LAYOUT_EXPECTED,
        "wasm32 HashMap/HashSet layout output mismatch\nstderr:\n{stderr}",
    );
}

// Native↔wasm parity: the same layout-keyed map/set program must produce
// byte-identical output on the native target.  This is the parity half of the
// run-pass above — it fails if the descriptor width or ABI diverges between
// targets rather than only when wasm regresses in isolation.
#[test]
fn native_hashmap_hashset_layout_matches_wasi_output() {
    support::require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("hashmap_hashset_layout_native.hew");
    fs::write(&source, HASHMAP_HASHSET_LAYOUT_SOURCE).expect("write map/set layout native source");

    let output = support::run_bounded_hew_run(&source, dir.path());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "hew run (native) failed for HashMap/HashSet layout source\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert_eq!(
        stdout.as_ref(),
        HASHMAP_HASHSET_LAYOUT_EXPECTED,
        "native HashMap/HashSet layout output mismatch\nstderr:\n{stderr}",
    );
}

// Ownership / drop-path parity for the string-keyed/valued layout surface.
//
// String elements carry header-aware reference counts, so the descriptor
// `drop_fn` (`hew_string_drop`) and clone thunk (`hew_string_clone`) MUST run
// correctly on each ownership transition:
//   * overwrite releases the OLD value buffer and stores the new one;
//   * remove releases the entry's key+value buffers;
//   * scope exit frees the remaining storage.
//
// The exact drop-count assertions for those transitions live at the FFI
// boundary (`hew-runtime/tests/hashmap_layout_string_refcount_ladder.rs`,
// which asserts the visible rc ladder `1→2→1→0` over the SAME ungated
// `hew_hashmap_clone_layout` / `hew_hashset_free_layout` symbols this WASI
// program links). Here the proof is behavioural and runs under wasmtime: the
// post-overwrite and post-remove reads must observe EXACTLY the surviving
// values. An inert drop thunk would leak (caught by the native rc ladder); a
// double-free / use-after-free of a released string buffer would trap under
// wasmtime and fail this run. Native and wasm stdout must match byte-for-byte.
const HASHMAP_HASHSET_OWNERSHIP_SOURCE: &str = r#"fn main() {
    let m: HashMap<string, string> = HashMap::new();
    m.insert("alpha", "first");
    m.insert("beta", "second");

    // Overwrite releases the old "first" buffer and stores "third".
    m.insert("alpha", "third");
    let a = m.get("alpha");
    match a {
        Some(val) => println(f"alpha={val}"),
        None => println("alpha=None"),
    }
    // The untouched entry must still read its original value.
    let bv = m.get("beta");
    match bv {
        Some(val) => println(f"beta={val}"),
        None => println("beta=None"),
    }

    // Remove releases beta's key+value buffers.
    let removed = m.remove("beta");
    println(f"removed={removed}");
    let b = m.get("beta");
    match b {
        Some(val) => println(f"beta_after={val}"),
        None => println("beta_after=None"),
    }
    let n = m.len();
    println(f"map_len={n}");

    // String set: insert/remove exercise hew_string_drop on the element.
    let s: HashSet<string> = HashSet::new();
    s.insert("x");
    s.insert("y");
    s.insert("z");
    let srm = s.remove("y");
    println(f"set_remove={srm}");
    let has_y = s.contains("y");
    println(f"set_has_removed={has_y}");
    let has_x = s.contains("x");
    println(f"set_has_kept={has_x}");
    let sn = s.len();
    println(f"set_len={sn}");
}
"#;

const HASHMAP_HASHSET_OWNERSHIP_EXPECTED: &str = "alpha=third\n\
     beta=second\n\
     removed=true\n\
     beta_after=None\n\
     map_len=1\n\
     set_remove=true\n\
     set_has_removed=false\n\
     set_has_kept=true\n\
     set_len=2\n";

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn wasm_hashmap_hashset_layout_ownership_run_pass() {
    require_wasi_runner();

    let dir = support::tempdir();
    let source = dir.path().join("hashmap_hashset_ownership_wasi.hew");
    fs::write(&source, HASHMAP_HASHSET_OWNERSHIP_SOURCE)
        .expect("write map/set ownership WASI source");

    let output = run_wasi_example(&source);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "hew run --target wasm32-wasi failed for HashMap/HashSet ownership source\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert_eq!(
        stdout.as_ref(),
        HASHMAP_HASHSET_OWNERSHIP_EXPECTED,
        "wasm32 HashMap/HashSet ownership output mismatch\nstderr:\n{stderr}",
    );
}

#[test]
fn native_hashmap_hashset_ownership_matches_wasi_output() {
    support::require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("hashmap_hashset_ownership_native.hew");
    fs::write(&source, HASHMAP_HASHSET_OWNERSHIP_SOURCE)
        .expect("write map/set ownership native source");

    let output = support::run_bounded_hew_run(&source, dir.path());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "hew run (native) failed for HashMap/HashSet ownership source\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert_eq!(
        stdout.as_ref(),
        HASHMAP_HASHSET_OWNERSHIP_EXPECTED,
        "native HashMap/HashSet ownership output mismatch\nstderr:\n{stderr}",
    );
}

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn wasi_run_timeout_terminates_a_non_terminating_program() {
    require_wasi_runner();

    let dir = support::tempdir();
    let source = dir.path().join("timeout_wasi.hew");
    fs::write(
        &source,
        "fn main() {\n    var i = 0;\n    loop {\n        i = i + 1;\n    }\n}\n",
    )
    .expect("write source");

    let output = Command::new(hew_binary())
        .args(["run", "--timeout", "1"])
        .arg(&source)
        .arg("--target")
        .arg("wasm32-wasi")
        .current_dir(dir.path())
        .output()
        .expect("run hew --target wasm32-wasi --timeout");
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "timeout should exit with code 1\nstderr:\n{stderr}",
    );
    assert!(
        stderr.contains("Error: program timed out after 1s"),
        "expected explicit timeout diagnostic\nstderr:\n{stderr}",
    );
}
