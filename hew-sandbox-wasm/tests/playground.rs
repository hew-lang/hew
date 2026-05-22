use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use hew_sandbox_wasm::{compile_to_sandbox_bytecode, Diagnostic};
use serde::Deserialize;

const RUNNABLE: &str = "runnable";
const UNSUPPORTED_NATIVE_ONLY: &str = "unsupported_native_only";
const SANDBOX_PROFILE: &str = "sandbox-vm-export";

const REQUIRED_SANDBOX_EXAMPLES: &[&str] = &[
    "basics/hello_world",
    "basics/fibonacci",
    "concurrency/counter_actor",
    "concurrency/actor_pipeline",
    "concurrency/supervisor",
    "machines/traffic_light",
];

#[derive(Debug, Deserialize)]
struct ManifestEntry {
    id: String,
    source_path: PathBuf,
    capabilities: Capabilities,
}

#[derive(Debug, Deserialize)]
struct Capabilities {
    sandbox: String,
}

#[test]
fn playground_manifest_sources_compile_to_sandbox_bytecode() {
    set_test_hewpath();
    let playground_dir = playground_dir();
    let entries = load_manifest(&playground_dir);
    let mut seen_ids = BTreeSet::new();

    for entry in &entries {
        assert!(
            seen_ids.insert(entry.id.as_str()),
            "duplicate playground manifest id {}",
            entry.id
        );

        let source_path = playground_dir.join(&entry.source_path);
        let source = std::fs::read_to_string(&source_path).unwrap_or_else(|err| {
            panic!(
                "failed to read playground source {} for {}: {err}",
                source_path.display(),
                entry.id
            )
        });
        let output =
            compile_to_sandbox_bytecode(&source, Some(SANDBOX_PROFILE)).unwrap_or_else(|err| {
                panic!("compile_to_sandbox_bytecode threw for {}: {err}", entry.id)
            });

        match entry.capabilities.sandbox.as_str() {
            RUNNABLE => {
                assert!(
                    output.diagnostics.iter().all(|diagnostic| diagnostic.severity != "error"),
                    "runnable playground entry {} produced error diagnostics:\n{}",
                    entry.id,
                    diagnostics_dump(&output.diagnostics)
                );
                let bytecode = output.bytecode.unwrap_or_else(|| {
                    panic!(
                        "runnable playground entry {} did not emit bytecode; diagnostics:\n{}",
                        entry.id,
                        diagnostics_dump(&output.diagnostics)
                    )
                });
                assert!(
                    !serde_json::to_vec(&bytecode)
                        .expect("sandbox bytecode should serialize")
                        .is_empty(),
                    "runnable playground entry {} emitted empty bytecode",
                    entry.id
                );
            }
            UNSUPPORTED_NATIVE_ONLY => {
                assert!(
                    output.bytecode.is_none(),
                    "unsupported_native_only playground entry {} unexpectedly emitted bytecode",
                    entry.id
                );
                assert!(
                    output
                        .diagnostics
                        .iter()
                        .any(is_typed_profile_rejection),
                    "unsupported_native_only playground entry {} must fail closed with a typed profile diagnostic; got:\n{}",
                    entry.id,
                    diagnostics_dump(&output.diagnostics)
                );
            }
            other => panic!(
                "manifest entry {} has unsupported capabilities.sandbox {other:?}; expected {RUNNABLE:?} or {UNSUPPORTED_NATIVE_ONLY:?}",
                entry.id
            ),
        }
    }

    for required_id in REQUIRED_SANDBOX_EXAMPLES {
        assert!(
            seen_ids.contains(required_id),
            "playground manifest missing required sandbox parity entry {required_id}"
        );
    }
}

fn load_manifest(playground_dir: &Path) -> Vec<ManifestEntry> {
    let manifest_path = playground_dir.join("manifest.json");
    let manifest = std::fs::read_to_string(&manifest_path).unwrap_or_else(|err| {
        panic!(
            "failed to read playground manifest {}: {err}",
            manifest_path.display()
        )
    });
    serde_json::from_str(&manifest).unwrap_or_else(|err| {
        panic!(
            "failed to parse playground manifest {}: {err}",
            manifest_path.display()
        )
    })
}

fn playground_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate should have a workspace parent")
        .join("examples")
        .join("playground")
}

fn set_test_hewpath() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate should have a workspace parent");
    std::env::set_var("HEWPATH", repo_root);
}

fn is_typed_profile_rejection(diagnostic: &Diagnostic) -> bool {
    diagnostic.phase == "profile"
        && !diagnostic.kind.is_empty()
        && (diagnostic.kind == "sandbox_profile_rejected"
            || diagnostic.kind.contains("NATIVE_ONLY")
            || diagnostic.kind.starts_with("reserved_")
            || diagnostic.kind.starts_with("unknown_")
            || diagnostic.kind.ends_with("_rejected"))
}

fn diagnostics_dump(diagnostics: &[Diagnostic]) -> String {
    serde_json::to_string_pretty(diagnostics).expect("diagnostics should serialize")
}
