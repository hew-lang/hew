//! A release-count differential over the SHIPPED corpora — the observable that
//! can see a release that was silently WITHHELD.
//!
//! # Why the existing sweeps could not see this
//!
//! Round ten joined ACCEPTANCE to the binder-shape sweep's observable, because
//! the defect that round found was an over-restriction: an over-restriction
//! REFUSES a program rather than changing a count, so a count-only sweep was
//! blind to it. It also added `stdlib_corpus_compilation_root_sweep`, whose
//! observable is acceptance alone.
//!
//! Round eleven is that lesson in mirror image. `std/net/net.hew` kept
//! compiling clean — as a root unit and through an import, both — while
//! `connect_timeout` quietly lost all thirteen releases of its `host` binding,
//! including the one on the ordinary `return`. A sweep that watches acceptance
//! cannot see a withheld release, because withholding a release is not a
//! refusal: it is the same program, minus a `free`.
//!
//! The binder-shape sweep DOES count releases, but over synthetic fixtures on
//! axes it enumerates — and it has no "argument to a declared extern" axis, so
//! the cell this defect lived in did not exist. Six hundred and forty-eight
//! shipped `.hew` files, zero acceptance drift, forty-six releases gone.
//!
//! # The property
//!
//! > A release the compiler used to emit for a shipped program does not
//! > disappear without someone saying why.
//!
//! This is a differential, not an absolute claim: it cannot tell you the count
//! is RIGHT (the leak oracles do that, by running programs under an allocator).
//! It tells you the count did not silently fall — which is exactly the failure
//! mode acceptance is blind to and the one an ownership change produces.
//!
//! # What is counted, and against what
//!
//! One number per `(file, function)`: the drop entries the ELABORATOR planted,
//! read out of `--dump-mir elab`'s `drop_plans` section. That is the authority
//! the runtime `free` calls are generated from, so it needs no allocator, no
//! linker and no execution — the whole 648-file corpus is measured in seconds.
//!
//! The baseline in `fixtures/release-count-baseline.tsv` was captured from
//! `main` (`2459668ea`) — the merge base of the branch that introduced this
//! test — so the comparison really is a differential against the shipped
//! compiler rather than against a number this branch chose. Cells whose count
//! is zero are omitted: a cell that goes `0 -> n` is a fix, and a cell that
//! goes `n -> 0` is still caught, because the baseline row is there. A file
//! whose every cell is zero therefore carries no rows at all — it has nothing
//! that can fall.
//!
//! Regenerate with `HEW_RELEASE_COUNT_CAPTURE=1 cargo nextest run -p hew-cli
//! -E 'binary(stdlib_corpus_release_count_differential)' --no-capture`, and say
//! in the commit message which counts moved and why. A DROP is a finding; an
//! INCREASE passes and is reported, because restoring a release is the direction
//! this file exists to protect.

mod support;

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;
use support::{hew_binary, repo_root};

/// One `(file, function)` cell's release count, or the fact that the file was
/// not compiled at all.
///
/// A file that stops compiling loses EVERY release in it at once, so
/// `Rejected` has to be a value of the same observable rather than a skip —
/// otherwise the loudest possible drop would read as "no rows to compare".
#[derive(Debug, Clone, PartialEq, Eq)]
enum FileCounts {
    Rejected,
    Functions(BTreeMap<String, usize>),
}

const BASELINE: &str = "hew-cli/tests/fixtures/release-count-baseline.tsv";
const REJECTED: &str = "REJECTED";

/// Files whose source changed after the corpus-wide baseline was anchored and
/// whose complete positive-count block was therefore recaptured. The global
/// differential deliberately permits new release cells (the fix direction),
/// but a source refresh must not omit one of its new functions from future
/// regression coverage.
///
/// `examples/mqtt_broker.hew` remains in the ordinary baseline: its repaired
/// suspend-abandon cleanup compiles cleanly again, so the main differential
/// continues to protect every positive-count function in its `Functions` block.
const SOURCE_REFRESHED_BASELINE_FILES: &[&str] = &[];

/// Cells that stand below the captured baseline on purpose.
const INLINE_RELEASE_REASON: &str = "generation-aware ownership now places the release directly \
    after the final borrowing read in raw MIR instead of copying it into each reachable exit \
    plan; the raw instruction remains the runtime release authority, and the executable \
    ownership corpus plus checked-MIR execution pin the affected cleanup paths";
const ACCOUNTED_BELOW_BASELINE: &[(&str, &str, usize, &str)] = &[
    (
        "examples/benchmarks/http_server.hew",
        "std$net$http$find_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/benchmarks/http_server.hew",
        "std$net$http$find_response_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/benchmarks/http_server.hew",
        "std$net$http$header_count",
        31,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/benchmarks/http_server_expert.hew",
        "std$net$http$find_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/benchmarks/http_server_expert.hew",
        "std$net$http$find_response_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/benchmarks/http_server_expert.hew",
        "std$net$http$header_count",
        31,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/curl_client.hew",
        "main",
        220,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/http_server.hew",
        "std$net$http$find_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/http_server.hew",
        "std$net$http$find_response_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/http_server.hew",
        "std$net$http$header_count",
        31,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/log/logger_scoped.hew",
        "std$misc$log$field",
        0,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/log/structured_json.hew",
        "std$misc$log$field",
        0,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/log/structured_text.hew",
        "std$misc$log$field",
        0,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/await_http_codec_hardening.hew",
        "std$net$http$http_async_client$find_response_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/await_http_codec_hardening.hew",
        "std$net$http$http_async_server$find_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/await_http_codec_hardening.hew",
        "std$net$http$http_async_server$header_count",
        31,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/await_http_roundtrip.hew",
        "std$net$http$http_async_client$find_response_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/await_http_roundtrip.hew",
        "std$net$http$http_async_server$find_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/await_http_roundtrip.hew",
        "std$net$http$http_async_server$header_count",
        31,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/http_await_service.hew",
        "std$net$http$http_async_client$find_response_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/http_await_service.hew",
        "std$net$http$http_async_server$find_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/net/http_await_service.hew",
        "std$net$http$http_async_server$header_count",
        31,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/playground/types/method_clone.hew",
        "main",
        10,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/sandbox-graduation/struct_functional_update.hew",
        "main",
        1,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/selfhost_lexer_v2.hew",
        "test_string_builtins",
        0,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/static_server.hew",
        "std$net$http$find_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/static_server.hew",
        "std$net$http$find_response_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/static_server.hew",
        "std$net$http$header_count",
        31,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/v05/checked-mir/vec_owned_elems.hew",
        "main",
        59,
        INLINE_RELEASE_REASON,
    ),
    (
        "examples/v05/surfaces/template_render.hew",
        "std$text$template$validate",
        138,
        INLINE_RELEASE_REASON,
    ),
    (
        "std/misc/log/log.hew",
        "std$misc$log$field",
        0,
        INLINE_RELEASE_REASON,
    ),
    (
        "std/net/http/http_async_client.hew",
        "std$net$http$find_response_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "std/net/http/http_async_server.hew",
        "std$net$http$find_header",
        42,
        INLINE_RELEASE_REASON,
    ),
    (
        "std/net/http/http_async_server.hew",
        "std$net$http$header_count",
        31,
        INLINE_RELEASE_REASON,
    ),
    (
        "std/text/template/template.hew",
        "std$text$template$validate",
        138,
        INLINE_RELEASE_REASON,
    ),
];

fn accounted_shortfalls() -> impl Iterator<Item = (&'static str, &'static str, usize, &'static str)>
{
    ACCOUNTED_BELOW_BASELINE.iter().copied()
}

fn capture_mode() -> bool {
    std::env::var_os("HEW_RELEASE_COUNT_CAPTURE").is_some()
}

/// Every `.hew` file under `std/` and `examples/` — the shipped corpora, the
/// same sets `scripts/corpus-ratchet.sh stdlib` and the example gates walk.
fn corpus() -> Vec<PathBuf> {
    fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "hew") {
                out.push(path);
            }
        }
    }
    let mut out = Vec::new();
    for root in ["std", "examples"] {
        walk(&repo_root().join(root), &mut out);
    }
    out.sort();
    assert!(
        out.len() > 400,
        "the shipped corpus must be discovered, not silently empty — found {}",
        out.len()
    );
    out
}

fn rel(path: &Path) -> String {
    path.strip_prefix(repo_root())
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

/// Count the elaborated drop entries per function in one file.
///
/// `drop_plans` entries are indented six spaces and name a MIR local
/// (`drop _7 ty=string kind=cow_heap(hew_string_drop)`); the statement-level
/// `drop BindingId(..)` lines four spaces in are the pre-elaboration binding
/// list and are NOT releases, so only the former are counted.
fn counts_for(file: &Path) -> FileCounts {
    let output = Command::new(hew_binary())
        .arg("compile")
        .arg(file)
        .args(["--dump-mir", "elab"])
        .current_dir(repo_root())
        .output()
        .expect("run hew compile --dump-mir elab");
    if !output.status.success() {
        return FileCounts::Rejected;
    }
    let direct_prefix =
        hew_types::module_registry::is_canonical_stdlib_module_source(file, "std.stream")
            .then_some("stream$");
    FileCounts::Functions(parse_dump_for_direct_module(
        &String::from_utf8_lossy(&output.stdout),
        direct_prefix,
    ))
}

/// The parser the whole observable rests on, factored out so the self-test
/// below exercises THIS code rather than a copy of it.
fn parse_dump(stdout: &str) -> BTreeMap<String, usize> {
    parse_dump_for_direct_module(stdout, None)
}

/// Parse one dump, optionally normalising the canonical prefix of the module
/// compiled directly as the root unit.
///
/// Imported dependency symbols retain their prefixes. `std/stream.hew` is the
/// first non-intrinsic source module whose exact std provenance requires the
/// frontend to lower it under `std.stream`; its own `stream$pipe` symbol must
/// therefore compare with the historical root-source key `pipe`, while an
/// imported `fs$read` in the same dump remains `fs$read`.
fn parse_dump_for_direct_module(
    stdout: &str,
    direct_prefix: Option<&str>,
) -> BTreeMap<String, usize> {
    let mut per: BTreeMap<String, usize> = BTreeMap::new();
    let mut current: Option<String> = None;
    for line in stdout.lines() {
        if let Some(rest) = line.strip_prefix("fn ") {
            let raw_name = rest.split_once(" -> ").map_or(rest, |(head, _)| head);
            let name = direct_prefix
                .and_then(|prefix| raw_name.strip_prefix(prefix))
                .unwrap_or(raw_name);
            current = Some(name.to_string());
            per.entry(name.to_string()).or_insert(0);
        } else if line.starts_with("      drop _") {
            if let Some(name) = &current {
                *per.get_mut(name).expect("function seen before its drops") += 1;
            }
        }
    }
    per
}

/// Walk the corpus with a small thread pool — the whole sweep is a few seconds
/// wall-clock, which is what makes a corpus-wide observable affordable enough
/// to run on every change rather than in a nightly.
fn measure_corpus() -> BTreeMap<String, FileCounts> {
    let files = corpus();
    let threads = std::thread::available_parallelism().map_or(4, std::num::NonZeroUsize::get);
    let chunk = files.len().div_ceil(threads);
    let mut measured: BTreeMap<String, FileCounts> = BTreeMap::new();
    std::thread::scope(|scope| {
        let handles: Vec<_> = files
            .chunks(chunk)
            .map(|slice| {
                scope.spawn(move || {
                    slice
                        .iter()
                        .map(|f| (rel(f), counts_for(f)))
                        .collect::<Vec<_>>()
                })
            })
            .collect();
        for handle in handles {
            measured.extend(handle.join().expect("corpus worker"));
        }
    });
    measured
}

fn render(measured: &BTreeMap<String, FileCounts>) -> String {
    let mut out = String::from(
        "# Elaborated release counts per (file, function) over std/ and examples/.\n\
         # Anchored on main (2459668ea). Zero-count cells are omitted; a file the\n\
         # compiler refuses is recorded as REJECTED so losing a whole file is a\n\
         # visible drop rather than an absence. Regenerate with\n\
         # HEW_RELEASE_COUNT_CAPTURE=1 and state in the commit which counts moved.\n",
    );
    for (file, counts) in measured {
        match counts {
            FileCounts::Rejected => {
                let _ = writeln!(out, "{file}\t{REJECTED}\t0");
            }
            FileCounts::Functions(per) => {
                for (function, count) in per {
                    if *count > 0 {
                        let _ = writeln!(out, "{file}\t{function}\t{count}");
                    }
                }
            }
        }
    }
    out
}

fn load_baseline() -> BTreeMap<String, FileCounts> {
    let text = std::fs::read_to_string(repo_root().join(BASELINE))
        .unwrap_or_else(|e| panic!("read {BASELINE}: {e}"));
    let mut out: BTreeMap<String, FileCounts> = BTreeMap::new();
    for line in text.lines() {
        if line.starts_with('#') || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.split('\t');
        let (file, function, count) = (
            parts.next().expect("file column"),
            parts.next().expect("function column"),
            parts.next().expect("count column"),
        );
        if function == REJECTED {
            out.insert(file.to_string(), FileCounts::Rejected);
            continue;
        }
        let entry = out
            .entry(file.to_string())
            .or_insert_with(|| FileCounts::Functions(BTreeMap::new()));
        if let FileCounts::Functions(per) = entry {
            per.insert(
                function.to_string(),
                count.parse().expect("count is a number"),
            );
        }
    }
    assert!(
        out.len() > 300,
        "the baseline must cover the corpus — found {} files. Only files that \
         emit at least one release, plus the files the compiler refuses, carry \
         rows; a file whose every cell is zero has nothing that can fall.",
        out.len()
    );
    out
}

#[test]
fn no_shipped_program_silently_loses_a_release() {
    let measured = measure_corpus();

    if capture_mode() {
        std::fs::write(repo_root().join(BASELINE), render(&measured))
            .unwrap_or_else(|e| panic!("write {BASELINE}: {e}"));
        eprintln!("captured {BASELINE} over {} files", measured.len());
        return;
    }

    let baseline = load_baseline();
    let accounted: BTreeMap<(&str, &str), (usize, &str)> = accounted_shortfalls()
        .map(|(file, function, count, reason)| ((file, function), (count, reason)))
        .collect();

    let mut drops: Vec<String> = Vec::new();
    let mut increases: Vec<String> = Vec::new();
    let mut stale: Vec<String> = Vec::new();

    for (file, before) in &baseline {
        let Some(after) = measured.get(file) else {
            drops.push(format!(
                "  {file}: the file is in the baseline and was not measured — \
                 it was deleted or moved, which is a drop of every release in it"
            ));
            continue;
        };
        match (before, after) {
            (FileCounts::Rejected, FileCounts::Rejected) => {}
            (FileCounts::Rejected, FileCounts::Functions(_)) => {
                increases.push(format!("  {file}: REJECTED -> accepted"));
            }
            (FileCounts::Functions(_), FileCounts::Rejected) => {
                drops.push(format!(
                    "  {file}: accepted -> REJECTED; every release in the file is gone"
                ));
            }
            (FileCounts::Functions(was), FileCounts::Functions(now)) => {
                for (function, &before_count) in was {
                    let after_count = now.get(function).copied().unwrap_or(0);
                    match accounted.get(&(file.as_str(), function.as_str())) {
                        Some((expected, _)) if after_count == *expected => {}
                        Some((expected, _)) => stale.push(format!(
                            "  {file}::{function}: accounted for at {expected} releases, \
                             measured {after_count} (baseline {before_count}). The stated \
                             reason no longer describes the code — re-read it and either \
                             update or delete the entry."
                        )),
                        None if after_count < before_count => drops.push(format!(
                            "  {file}::{function}: {before_count} -> {after_count} releases"
                        )),
                        None if after_count > before_count => increases.push(format!(
                            "  {file}::{function}: {before_count} -> {after_count} releases"
                        )),
                        None => {}
                    }
                }
            }
        }
    }

    if !increases.is_empty() {
        eprintln!(
            "release-count differential: {} cell(s) gained releases against the \
             baseline (the fix direction; recapture when they are settled):\n{}",
            increases.len(),
            increases.join("\n")
        );
    }
    assert!(
        stale.is_empty(),
        "an accounted-for shortfall no longer matches what the compiler emits:\n{}",
        stale.join("\n")
    );
    assert!(
        drops.is_empty(),
        "a shipped program lost releases the compiler used to emit for it. A withheld \
         release is a leak, and acceptance cannot see it — `std/net/net.hew` kept \
         compiling clean while `connect_timeout` lost thirteen. Either restore the \
         releases or, if the withholding is correct, add the cell to \
         ACCOUNTED_BELOW_BASELINE with the reason:\n{}",
        drops.join("\n")
    );
}

/// The instrument has to be able to fail, and it has to fail for the reason it
/// claims. Both halves are checked against the parser rather than against the
/// compiler, so this stays a test of the OBSERVABLE.
#[test]
fn the_differential_reads_plan_drops_and_not_binding_statements() {
    // A dump shaped exactly like `--dump-mir elab`: the four-space
    // `drop BindingId(..)` lines are the binding list, the six-space
    // `drop _n` lines are the elaborated releases.
    let dump = [
        "fn pick -> i64",
        "  statements:",
        "    bind BindingId(1) host site=SiteId(0) ty=string",
        "    drop BindingId(1) host ty=string",
        "  drop_plans:",
        "    return[bb1] ->",
        "      drop _4 ty=string kind=cow_heap(hew_string_drop)",
        "      drop _9 ty=string kind=cow_heap(hew_string_drop)",
        "",
        "fn quiet -> ()",
        "  drop_plans:",
        "    return[bb0] ->",
        "      (none)",
    ]
    .join("\n");
    let per = parse_dump(&dump);
    assert_eq!(
        per.get("pick").copied(),
        Some(2),
        "two plan drops, not three"
    );
    assert_eq!(
        per.get("quiet").copied(),
        Some(0),
        "a function with no plan drops must still be a cell, so losing its \
         releases later is a drop rather than a missing row"
    );
}

#[test]
fn direct_module_prefix_normalization_preserves_imported_dependency_symbols() {
    let dump = [
        "fn stream$pipe -> ()",
        "  drop_plans:",
        "    return[bb0] ->",
        "      drop _1 ty=string kind=cow_heap(hew_string_drop)",
        "",
        "fn fs$read -> ()",
        "  drop_plans:",
        "    return[bb0] ->",
        "      drop _2 ty=string kind=cow_heap(hew_string_drop)",
    ]
    .join("\n");
    let per = parse_dump_for_direct_module(&dump, Some("stream$"));
    assert_eq!(per.get("pipe"), Some(&1));
    assert_eq!(
        per.get("fs$read"),
        Some(&1),
        "an imported dependency name must retain its owner prefix"
    );
    assert!(
        !per.contains_key("stream$pipe"),
        "only the direct module's canonical prefix is normalized"
    );
}

/// Every accounted-for shortfall names a real cell in the baseline and really
/// is BELOW it. Without this an entry could quietly waive a cell that no longer
/// exists, or "account for" a count that never dropped.
#[test]
fn every_accounted_shortfall_is_a_real_shortfall() {
    let baseline = load_baseline();
    for (file, function, expected, reason) in accounted_shortfalls() {
        let Some(FileCounts::Functions(per)) = baseline.get(file) else {
            panic!("{file} is accounted for but is not an accepted file in the baseline");
        };
        let before = per.get(function).unwrap_or_else(|| {
            panic!("{file}::{function} is accounted for but is not in the baseline")
        });
        assert!(
            expected < *before,
            "{file}::{function} is accounted for at {expected} but the baseline is \
             {before}; an accounted entry must describe a SHORTFALL"
        );
        assert!(
            reason.len() > 80,
            "{file}::{function}: \"without a stated reason\" means the reason has to \
             say something"
        );
    }
}

#[test]
fn source_refreshed_baseline_blocks_are_complete() {
    let baseline = load_baseline();
    for file in SOURCE_REFRESHED_BASELINE_FILES {
        let measured = match counts_for(&repo_root().join(file)) {
            FileCounts::Functions(per) => per
                .into_iter()
                .filter(|(_, count)| *count > 0)
                .collect::<BTreeMap<_, _>>(),
            FileCounts::Rejected => panic!("{file} is rejected after its baseline was refreshed"),
        };
        let Some(FileCounts::Functions(recorded)) = baseline.get(*file) else {
            panic!("{file} has no accepted block in the release-count baseline");
        };
        assert_eq!(
            recorded, &measured,
            "{file} changed after its baseline block was refreshed; recapture every \
             positive release-count cell so a new function cannot remain unguarded"
        );
    }
}
