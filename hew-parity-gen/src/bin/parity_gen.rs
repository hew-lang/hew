//! Render parity programs for `scripts/fuzz/run-parity.py`.
//!
//! `parity_gen --out DIR --seeds START..END` renders one program per seed from
//! a fixed byte stream, so a seed names the same program on every host.
//! `parity_gen --out DIR --corpus DIR` renders each libFuzzer input of the
//! `fuzz_parity_gen` target. `--only 0,2` keeps a subset of each program's
//! cases, which the oracle uses to minimise a mismatch. Each program is written
//! as `NAME.hew` beside `NAME.json`, the generator's model of its output.

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use arbitrary::Unstructured;
use hew_parity_gen as parity;

/// Bytes for a seed: a splitmix64 stream, stable across hosts and releases.
fn seed_bytes(seed: u64) -> Vec<u8> {
    let mut state = seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) ^ 0xD1B5_4A32_D192_ED03;
    let mut bytes = Vec::with_capacity(4096);
    while bytes.len() < 4096 {
        state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = state;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        bytes.extend_from_slice(&(z ^ (z >> 31)).to_le_bytes());
    }
    bytes
}

fn json_string(text: &str) -> String {
    let mut out = String::from("\"");
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            c if u32::from(c) < 0x20 => {
                let _ = write!(out, "\\u{:04x}", u32::from(c));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn write(out: &Path, name: &str, data: &[u8], only: Option<&[usize]>) {
    let program = parity::generate(&mut Unstructured::new(data), only).expect("generation");
    std::fs::write(out.join(format!("{name}.hew")), &program.source).expect("write program");
    let trap = program.trap.map_or("null".to_string(), json_string);
    let model = format!(
        "{{\"stdout\": {}, \"trap\": {trap}}}\n",
        json_string(&program.stdout)
    );
    std::fs::write(out.join(format!("{name}.json")), model).expect("write model");
}

fn main() {
    let mut args = std::env::args().skip(1);
    let mut out = None;
    let mut seeds = None;
    let mut corpus = None;
    let mut only: Option<Vec<usize>> = None;
    while let Some(arg) = args.next() {
        let value = args.next().unwrap_or_else(|| panic!("{arg} needs a value"));
        match arg.as_str() {
            "--out" => out = Some(PathBuf::from(value)),
            "--seeds" => seeds = Some(value),
            "--corpus" => corpus = Some(PathBuf::from(value)),
            "--only" => {
                only = Some(
                    value
                        .split(',')
                        .map(|n| n.parse().expect("case index"))
                        .collect(),
                );
            }
            _ => panic!("unknown argument {arg}"),
        }
    }
    let out = out.expect("--out DIR is required");
    std::fs::create_dir_all(&out).expect("create output directory");
    if let Some(range) = seeds {
        let (start, end) = range.split_once("..").expect("seed range START..END");
        let (start, end): (u64, u64) = (start.parse().expect("start"), end.parse().expect("end"));
        for seed in start..end {
            write(
                &out,
                &format!("seed-{seed:06}"),
                &seed_bytes(seed),
                only.as_deref(),
            );
        }
    }
    if let Some(corpus) = corpus {
        let mut inputs: Vec<_> = std::fs::read_dir(&corpus)
            .expect("read corpus")
            .filter_map(|entry| entry.ok().map(|entry| entry.path()))
            .filter(|path| path.is_file())
            .collect();
        inputs.sort();
        for input in inputs {
            let name = input
                .file_name()
                .and_then(|name| name.to_str())
                .expect("input name");
            let data = std::fs::read(&input).expect("read input");
            write(&out, &format!("input-{name}"), &data, only.as_deref());
        }
    }
}
