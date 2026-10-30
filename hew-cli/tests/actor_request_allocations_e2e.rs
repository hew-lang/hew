//! A request to an actor allocates a bounded number of heap blocks.
//!
//! The program counts allocator events around two runs of sequential asks and
//! prints the per-request difference, so start-up and the first request's
//! one-time growth cancel out. The bound is an upper limit: lowering the
//! per-request cost never fails it.

mod support;

use support::{hew_command, require_codegen, run_bounded_command};

const MAX_ALLOCATIONS_PER_REQUEST: u64 = 21;

const SOURCE: &str = r#"
actor Counter {
    var count: i64;
    receive fn increment() -> i64 {
        count += 1;
        count
    }
}

extern "C" {
    fn hew_memory_alloc_count() -> u64;
}

fn asks(counter: Counter, count: i64) -> u64 {
    let before = unsafe { hew_memory_alloc_count() };
    var sum = 0;
    for _ in 0..count {
        match counter.increment() {
            .Ok(value) => { sum += value; }
            .Err(_) => panic("counter request failed"),
        }
    }
    let after = unsafe { hew_memory_alloc_count() };
    after - before
}

fn main() {
    let counter = spawn Counter { count: 0 };
    let short = asks(counter, 64);
    let long = asks(counter, 576);
    println(f"{(long - short) / 512}");
}
"#;

#[test]
fn actor_request_allocations_stay_bounded() {
    require_codegen();
    let dir = support::tempdir();
    let path = dir.path().join("actor_request_allocations.hew");
    std::fs::write(&path, SOURCE).unwrap();
    let mut command = hew_command();
    command
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .env("HEW_WORKERS", "1");
    let output = run_bounded_command(command, "hew run actor_request_allocations.hew");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "fixture should run; stdout: {stdout}\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let per_request: u64 = stdout.trim().parse().expect("per-request allocation count");
    assert!(
        per_request <= MAX_ALLOCATIONS_PER_REQUEST,
        "one actor request allocated {per_request} blocks; the bound is {MAX_ALLOCATIONS_PER_REQUEST}",
    );
}
