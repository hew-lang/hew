//! Creating and dropping an `Rc` allocates exactly its own block.
//!
//! The program counts allocator events around two loops of `Rc` construction
//! and prints the difference between them, so start-up and one-time growth
//! cancel out. The live
//! Rc census must not add allocations, whether or not a profile consumer is
//! configured to read it.

mod support;

use support::{hew_command, require_codegen, run_bounded_command};

const SOURCE: &str = r#"
extern "C" {
    fn hew_memory_alloc_count() -> u64;
}

fn cells(count: i64) -> u64 {
    let before = unsafe { hew_memory_alloc_count() };
    var sum = 0;
    for value in 0..count {
        let cell: Rc<i64> = Rc.new(value);
        sum += cell.get();
    }
    let after = unsafe { hew_memory_alloc_count() };
    if sum < 0 {
        panic("unreachable");
    }
    after - before
}

fn main() {
    let short = cells(64);
    let long = cells(576);
    println(f"{long - short}");
}
"#;

fn allocations_for_512_rcs(profile_output: Option<&str>) -> u64 {
    let dir = support::tempdir();
    let path = dir.path().join("rc_allocations.hew");
    std::fs::write(&path, SOURCE).unwrap();
    let mut command = hew_command();
    command
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .env("HEW_WORKERS", "1")
        .env_remove("HEW_PPROF")
        .env_remove("HEW_PROF_OUTPUT");
    if let Some(output) = profile_output {
        command.env("HEW_PROF_OUTPUT", output);
    }
    let output = run_bounded_command(command, "hew run rc_allocations.hew");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "fixture should run; stdout: {stdout}\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    stdout
        .lines()
        .next()
        .and_then(|line| line.trim().parse().ok())
        .expect("allocation count")
}

#[test]
fn rc_allocation_allocates_only_its_block() {
    require_codegen();
    let allocations = allocations_for_512_rcs(None);
    assert!(
        allocations <= 512,
        "512 Rc values allocated {allocations} blocks"
    );
}

#[test]
fn rc_census_adds_no_allocation_when_a_profile_reads_it() {
    require_codegen();
    let allocations = allocations_for_512_rcs(Some("flat"));
    assert!(
        allocations <= 512,
        "512 Rc values allocated {allocations} blocks"
    );
}
