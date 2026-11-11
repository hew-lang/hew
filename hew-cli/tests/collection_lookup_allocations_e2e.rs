//! Computed collection keys cost no more snapshots than immutable key bindings.

mod support;

use support::{hew_command, require_codegen, run_bounded_command};

const SOURCE: &str = r#"extern "C" {
    fn hew_memory_alloc_count() -> u64;
}

fn computed_reads(values: HashMap<i64, string>, count: i64) -> u64 {
    let before = unsafe { hew_memory_alloc_count() };
    var total = 0;
    for n in 0..count {
        match values.get((n % 64) * 7) {
            .Some(value) => { total += value.len(); }
            .None => panic("missing key"),
        }
    }
    let after = unsafe { hew_memory_alloc_count() };
    assert(total == count * 6);
    after - before
}

fn named_reads(values: HashMap<i64, string>, count: i64) -> u64 {
    let before = unsafe { hew_memory_alloc_count() };
    var total = 0;
    for n in 0..count {
        let key = (n % 64) * 7;
        match values.get(key) {
            .Some(value) => { total += value.len(); }
            .None => panic("missing key"),
        }
    }
    let after = unsafe { hew_memory_alloc_count() };
    assert(total == count * 6);
    after - before
}

fn main() {
    var values: HashMap<i64, string> = HashMap.new();
    for n in 0..64 {
        values.insert(n * 7, "colour");
    }
    // Warm one-time runtime state before comparing incremental costs.
    let _ = computed_reads(values, 1);
    let _ = named_reads(values, 1);
    println(computed_reads(values, 576) - computed_reads(values, 64));
    println(named_reads(values, 576) - named_reads(values, 64));
}
"#;

#[test]
fn computed_lookup_keys_do_not_allocate_receiver_snapshots() {
    require_codegen();
    let dir = support::tempdir();
    let path = dir.path().join("collection_lookup_allocations.hew");
    std::fs::write(&path, SOURCE).unwrap();
    let mut command = hew_command();
    command
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .env("HEW_WORKERS", "1");
    let output = run_bounded_command(command, "hew run collection_lookup_allocations.hew");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "fixture should run; stdout: {stdout}\nstderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let counts: Vec<u64> = stdout
        .lines()
        .map(|line| line.trim().parse().expect("allocation count"))
        .collect();
    assert_eq!(counts.len(), 2, "{stdout}");
    assert!(counts[1] > 0, "named lookups must produce owned values");
    assert_eq!(
        counts[0], counts[1],
        "computed keys must not snapshot a receiver that named keys borrow"
    );
}
