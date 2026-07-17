#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
        .to_path_buf()
}

fn hew_command(repo: &Path) -> Command {
    let _ = repo;
    // Cold `target/`: build `hew` once under the shared serialized build
    // lock, OUTSIDE any per-test deadline, so a concurrent build-lock holder
    // cannot make a `cargo run` fallback burn the bounded budget and produce
    // a false timeout (hew-lang/hew#1887).
    Command::new(hew_testutil::ensure_hew_bin_built().expect("build hew binary"))
}

fn ensure_hew_runtime_lib(repo: &Path) {
    let _ = repo;
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!("hew-float-return-{}-{stem}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let output = hew_command(repo)
        .arg("run")
        .arg(&path)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("stdout is utf-8")
}

#[test]
fn float_return_values_execute_and_feed_call_sites() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "float_returns_runtime",
        r#"
        fn c_to_f(c: f64) -> f64 {
            c * 9.0 / 5.0 + 32.0
        }

        fn square(x: f64) -> f64 {
            x * x
        }

        fn hyp(a: f64, b: f64) -> f64 {
            square(a) + square(b)
        }

        fn dbl(x: f32) -> f32 {
            x * 2.0
        }

        fn main() {
            let c0 = c_to_f(0.0) == 32.0;
            let c100 = c_to_f(100.0) == 212.0;
            let composed = hyp(3.0, 4.0) == 25.0;
            let f32_ok = dbl(2.5) == 5.0;
            let direct: f64 = c_to_f(0.0) + 10.0;
            let arithmetic = direct == 42.0;

            if c0 {
                if c100 {
                    if composed {
                        if f32_ok {
                            if arithmetic {
                                print(f"ret={c_to_f(0.0)};");
                                print("PASS");
                            }
                        }
                    }
                }
            }
        }
        "#,
    );

    assert!(
        stdout.contains("ret=") && stdout.contains("32") && stdout.contains("PASS"),
        "float-return runtime oracle failed; stdout={stdout:?}"
    );
}
