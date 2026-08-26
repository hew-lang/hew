//! Ownership oracle for typed wire codecs over `HashMap`/`HashSet` values.
//!
//! This pins the collection-field leak reported against PR #3030: retained
//! `Registry` values and their decoded siblings reached normal scope exit, but
//! their layout-backed collection handles were not discharged. The probe uses
//! owned string keys/elements and owned record values, then exercises binary
//! CBOR, JSON, and YAML through both the `#[wire]` method surface and the bare
//! `std.encoding.wire` facade.
//!
//! LOW/HIGH runs print exactly one witness line per completed iteration. A
//! leaked collection therefore grows with the iteration count instead of being
//! hidden by a one-process baseline. `leaks --atExit` runs under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`; a separate poisoned
//! allocator run makes over-release, stale reads, and double-free fail directly.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn wire_collection_roundtrip_source(frames: usize) -> String {
    const SOURCE: &str = r#"
import std.encoding.wire;

#[wire]
type Feature {
    enabled: bool @1,
    note: string @2,
}

#[wire]
type Registry {
    features: HashMap<string, Feature> @1,
    labels: HashMap<i64, string> @2,
    names: HashSet<string> @3,
}

fn main() {
    var i: i64 = 0;
    while i < __FRAMES__ {
        let features: HashMap<string, Feature> = HashMap.new();
        features.insert("beta-" + "key", Feature { enabled: false, note: "later-" + "owned" });
        features.insert("alpha-" + "key", Feature { enabled: true, note: "now-" + "owned" });

        let labels: HashMap<i64, string> = HashMap.new();
        labels.insert(2, "two-" + "owned");
        labels.insert(1, "one-" + "owned");

        let names: HashSet<string> = HashSet.new();
        names.insert("gamma-" + "owned");
        names.insert("alpha-" + "owned");

        let registry = Registry { features: features, labels: labels, names: names };

        let binary_back = Registry.decode(registry.encode());
        if binary_back.features.len() != 2 { panic("binary features"); }
        if binary_back.labels.len() != 2 { panic("binary labels"); }
        if binary_back.names.len() != 2 { panic("binary names"); }

        let json = registry.to_json();
        match Registry.from_json(json) {
            .Ok(json_back) => {
                if json_back.features.len() != 2 { panic("json features"); }
                if json_back.names.len() != 2 { panic("json names"); }
            },
            .Err(_) => { panic("json parse"); }
        }

        let yaml = registry.to_yaml();
        match Registry.from_yaml(yaml) {
            .Ok(yaml_back) => {
                if yaml_back.labels.len() != 2 { panic("yaml labels"); }
                if yaml_back.names.len() != 2 { panic("yaml names"); }
            },
            .Err(_) => { panic("yaml parse"); }
        }

        let bare_json = wire.to_json(registry.features);
        match wire.from_json<HashMap<string, Feature>>(bare_json) {
            .Ok(bare_back) => {
                if bare_back.len() != 2 { panic("bare map"); }
            },
            .Err(_) => { panic("bare json parse"); }
        }

        if registry.features.len() != 2 { panic("serialization moved registry"); }
        println(i);
        i = i + 1;
    }
}
"#;
    SOURCE.replace("__FRAMES__", &frames.to_string())
}

fn expected_lines(frames: usize) -> usize {
    frames
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` and the Darwin poisoned allocator"
)]
#[test]
fn wire_hash_collections_have_flat_owned_value_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "wire_hash_collections",
        wire_collection_roundtrip_source,
        expected_lines,
    );
}

#[cfg_attr(not(target_os = "macos"), ignore = "poisoned allocator is macOS-only")]
#[test]
fn wire_hash_collections_release_exactly_once_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("wire-hash-collections-poison-")
        .tempdir()
        .expect("tempdir");
    let binary = compile_to_native(
        &wire_collection_roundtrip_source(8),
        dir.path(),
        "wire_hash_collections_poison",
    );
    let output = run_under_malloc_scribble(&binary);
    assert!(
        output.status.success(),
        "wire collection round-trips must exit normally under the poisoned allocator:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).lines().count(),
        8,
        "poisoned run must complete every round-trip iteration:\n{}",
        describe_output(&output)
    );
}
