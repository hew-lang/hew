//! Seed corpora for the runtime fuzz targets, built from the runtime's own
//! encoders. `HEW_FUZZ_WRITE_SEEDS=1` rewrites `seeds/`; otherwise the test
//! fails when a committed seed no longer matches its encoder.

use std::path::{Path, PathBuf};

use hew_runtime::fuzzing::{control_handler_seeds, DECODERS};

fn wire_frame_seeds() -> Vec<Vec<u8>> {
    let mut seeds = Vec::new();
    for (index, decoder) in DECODERS.iter().enumerate() {
        let selector = u8::try_from(index).expect("decoder table fits a byte");
        for seed in (decoder.seeds)() {
            let mut input = vec![selector];
            input.extend(seed);
            seeds.push(input);
        }
    }
    seeds
}

fn sync(target: &str, seeds: &[Vec<u8>]) {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("seeds")
        .join(target);
    let write = std::env::var_os("HEW_FUZZ_WRITE_SEEDS").is_some();
    if write {
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("seed directory");
    }
    for (index, seed) in seeds.iter().enumerate() {
        let path = dir.join(format!("seed-{index:03}"));
        if write {
            std::fs::write(&path, seed).expect("write seed");
        } else {
            assert_eq!(
                std::fs::read(&path).ok().as_deref(),
                Some(seed.as_slice()),
                "{} is stale; rerun with HEW_FUZZ_WRITE_SEEDS=1",
                display(&path)
            );
        }
    }
    let committed = std::fs::read_dir(&dir).map_or(0, Iterator::count);
    assert_eq!(committed, seeds.len(), "{target} seed count");
}

fn display(path: &Path) -> String {
    path.display().to_string()
}

#[test]
fn seeds_match_the_encoders() {
    sync("fuzz_wire_frames", &wire_frame_seeds());
    sync("fuzz_control_handler", &control_handler_seeds());
}
