//! Joining strings allocates only the joined result.
//!
//! The program counts allocator events around two loops and prints the
//! difference between them, so start-up and one-time growth cancel out. `+` on two live
//! strings allocates exactly one block; an f-string allocates its rendered
//! segments, its literal and one block per join, with no scratch copies.

mod support;

use support::{hew_command, require_codegen, run_bounded_command};

const SOURCE: &str = r#"
extern "C" {
    fn hew_memory_alloc_count() -> u64;
}

fn joins(left: string, right: string, count: i64) -> u64 {
    let before = unsafe { hew_memory_alloc_count() };
    var total = 0;
    for _ in 0..count {
        let joined = left + right;
        total += joined.len();
    }
    let after = unsafe { hew_memory_alloc_count() };
    if total < 0 {
        panic("unreachable");
    }
    after - before
}

fn formats(count: i64) -> u64 {
    let before = unsafe { hew_memory_alloc_count() };
    var total = 0;
    for value in 0..count {
        let text = f"{value * 7}:{value}";
        total += text.len();
    }
    let after = unsafe { hew_memory_alloc_count() };
    if total < 0 {
        panic("unreachable");
    }
    after - before
}

fn main() {
    let left = "colour-" + "neighbour";
    let right = f"{left}-behaviour";
    println(f"{joins(left, right, 576) - joins(left, right, 64)}");
    println(f"{formats(576) - formats(64)}");
}
"#;

#[test]
fn string_joins_allocate_only_their_results() {
    require_codegen();
    let dir = support::tempdir();
    let path = dir.path().join("string_concat_allocations.hew");
    std::fs::write(&path, SOURCE).unwrap();
    let mut command = hew_command();
    command
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .env("HEW_WORKERS", "1");
    let output = run_bounded_command(command, "hew run string_concat_allocations.hew");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "fixture should run; stdout: {stdout}\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let counts: Vec<u64> = stdout
        .lines()
        .map(|line| line.trim().parse().expect("allocation count"))
        .collect();
    assert_eq!(counts.len(), 2, "{stdout}");
    assert!(counts[0] <= 512, "512 joins allocated {} blocks", counts[0]);
    // Two integer renderings, the `:` literal and two joins per format.
    assert!(
        counts[1] <= 5 * 512,
        "512 formats allocated {} blocks",
        counts[1]
    );
}
