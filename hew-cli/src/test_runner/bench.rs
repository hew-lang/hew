//! Benchmark selection and baseline reporting on the shared entry runner.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::args::{BenchArgs, BenchFormat, TestArgs, TestEngine, TestFormat, TestSchedule};

pub fn cmd_bench(args: &BenchArgs) {
    for name in [args.save.as_deref(), args.compare.as_deref()]
        .into_iter()
        .flatten()
    {
        if name.is_empty()
            || !name
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'))
        {
            eprintln!("Error: baseline names use letters, digits, hyphens and underscores");
            std::process::exit(2);
        }
    }
    let options = TestArgs {
        cases: std::num::NonZeroU32::MIN,
        coverage: false,
        paths: args.paths.clone(),
        doc: false,
        list: false,
        filter: None,
        partition: None,
        format: match args.format {
            BenchFormat::Text => TestFormat::Text,
            BenchFormat::Json => TestFormat::Json,
        },
        engine: TestEngine::Native,
        timeout: "30s".into(),
        color: None,
        show_output: false,
        no_capture: false,
        trace: None,
        rerun_failed: false,
        watch: false,
        include_ignored: false,
        ignored: false,
        allow_empty: false,
        jobs: Some(std::num::NonZeroUsize::MIN),
        schedule: TestSchedule::Fifo,
        seed: None,
        schedules: 0,
        step_budget: 10_000_000,
    };
    super::cmd_entries(&options, Some(args));
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Measurement {
    median_ns: f64,
    spread_ns: f64,
    samples: usize,
}

fn baseline(root: &Path, name: &str) -> PathBuf {
    root.join(".hew/benchmarks").join(format!("{name}.json"))
}

pub(super) fn report(
    summary: &super::runner::TestSummary,
    args: &BenchArgs,
    root: &Path,
) -> Result<(), String> {
    if summary.failed > 0 {
        let format = match args.format {
            BenchFormat::Text => super::output::OutputFormat::Text,
            BenchFormat::Json => super::output::OutputFormat::Json,
        };
        for failure in &summary.compile_failures {
            super::output::output_event(
                super::runner::TestEvent::FileCompiled {
                    file: failure.file.clone(),
                    tests: failure.tests.clone(),
                    diagnostics: Some(failure.message.clone()),
                },
                format,
                root,
                false,
                false,
            );
        }
        for result in &summary.results {
            if matches!(result.outcome, super::runner::TestOutcome::Failed(_)) {
                super::output::output_event(
                    super::runner::TestEvent::TestFinished(Box::new(result.clone())),
                    format,
                    root,
                    false,
                    false,
                );
            }
        }
        super::output::output_results(
            summary,
            false,
            match args.format {
                BenchFormat::Text => super::output::OutputFormat::Text,
                BenchFormat::Json => super::output::OutputFormat::Json,
            },
            root,
        );
        return Ok(());
    }
    let previous: BTreeMap<String, Measurement> = if let Some(name) = &args.compare {
        let path = baseline(root, name);
        let bytes = std::fs::read(&path)
            .map_err(|error| format!("cannot read baseline {}: {error}", path.display()))?;
        serde_json::from_slice(&bytes)
            .map_err(|error| format!("invalid baseline {}: {error}", path.display()))?
    } else {
        BTreeMap::new()
    };
    let mut current = BTreeMap::new();
    for result in &summary.results {
        if matches!(result.outcome, super::runner::TestOutcome::Ignored(_)) {
            continue;
        }
        let identity = super::test_identity(&result.test, root);
        let samples = result
            .report
            .as_ref()
            .map(|report| report.benchmarks.as_slice())
            .unwrap_or_default();
        if samples.is_empty() {
            return Err(format!("benchmark {identity} did not call Bencher.iter"));
        }
        #[expect(
            clippy::cast_precision_loss,
            reason = "benchmark reports nanoseconds per iteration as floating point"
        )]
        let mut times = samples
            .iter()
            .map(|(elapsed, iterations)| *elapsed as f64 / *iterations as f64)
            .collect::<Vec<_>>();
        times.sort_by(f64::total_cmp);
        let middle = times.len() / 2;
        let median_ns = if times.len() % 2 == 0 {
            (times[middle - 1] + times[middle]) / 2.0
        } else {
            times[middle]
        };
        // The interquartile range is insensitive to one interrupted sample.
        let spread_ns = times[times.len() * 3 / 4] - times[times.len() / 4];
        let measured = Measurement {
            median_ns,
            spread_ns,
            samples: times.len(),
        };
        let change = previous
            .get(&identity)
            .filter(|old| old.median_ns > 0.0)
            .map(|old| 100.0 * (median_ns / old.median_ns - 1.0));
        match args.format {
            BenchFormat::Json => println!(
                "{}",
                serde_json::json!({
                    "event": "benchmark", "identity": identity, "median_ns": median_ns,
                    "spread_ns": spread_ns, "spread": "interquartile_range", "samples": times.len(), "change_percent": change,
                })
            ),
            BenchFormat::Text => {
                print!("{identity}: {median_ns:.2} ns/iter, IQR {spread_ns:.2} ns");
                if let Some(change) = change {
                    print!(", {change:+.2}%");
                }
                println!();
            }
        }
        current.insert(identity, measured);
    }
    if let Some(name) = &args.save {
        let path = baseline(root, name);
        std::fs::create_dir_all(path.parent().ok_or("baseline path has no directory")?)
            .map_err(|error| error.to_string())?;
        let bytes = serde_json::to_vec_pretty(&current).map_err(|error| error.to_string())?;
        std::fs::write(&path, bytes)
            .map_err(|error| format!("cannot save baseline {}: {error}", path.display()))?;
    }
    Ok(())
}
