//! Reproducible authoring and retained-component measurements, without tuning.
//! Run `make bench-live-loop LIVE_BENCH_SDK=/path/to/matched/sdk`.
//! The SDK must match this host library; `HEW_WASI_SDK` selects the installed SDK.

use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{mpsc, Arc, Mutex};
use std::time::{Duration, Instant};

use clap::Parser;
use hew_live::component::build::{self, Toolchain};
use hew_live::component::host::{
    Delivery, Error, Event, Host, Observer, Prepared, ServiceCall, Services, Snapshot,
};
use hew_live::component::network::format_digest;
use serde_json::{json, Value};
use sha2::{Digest, Sha256};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;
const ROUTER: &str = include_str!("live_router.hew");

#[derive(Parser)]
struct Options {
    #[arg(long)]
    sdk: PathBuf,
    #[arg(long, default_value_t = 20, value_parser = clap::value_parser!(u32).range(2..=100))]
    samples: u32,
    #[arg(long, default_value_t = 4, value_parser = clap::value_parser!(u32).range(2..=16))]
    generations: u32,
}

#[derive(Default)]
struct Timings(BTreeMap<&'static str, Vec<f64>>);
impl Timings {
    fn add(&mut self, name: &'static str, start: Instant) {
        self.0
            .entry(name)
            .or_default()
            .push(start.elapsed().as_secs_f64() * 1000.0);
    }
    fn report(self) -> Value {
        let metrics: BTreeMap<_, _> = self.0.into_iter().map(|(name, samples)| {
            let mut sorted = samples.clone();
            sorted.sort_by(f64::total_cmp);
            let p95 = sorted[(sorted.len() * 95).div_ceil(100) - 1];
            let median = f64::midpoint(sorted[(sorted.len()-1)/2], sorted[sorted.len()/2]);
            (name, json!({"samples_ms": samples, "count": sorted.len(), "min_ms": sorted[0], "median_ms": median, "p95_ms": p95, "max_ms": sorted[sorted.len()-1], "p95_within_250ms": p95 <= 250.0}))
        }).collect();
        json!(metrics)
    }
}

fn output(command: &mut Command) -> Result<String> {
    let result = command.output()?;
    if !result.status.success() {
        return Err(format!(
            "{command:?}: {}{}",
            String::from_utf8_lossy(&result.stdout),
            String::from_utf8_lossy(&result.stderr)
        )
        .into());
    }
    Ok(String::from_utf8(result.stdout)?)
}

fn file_digest(path: &Path) -> Result<String> {
    let mut file = std::fs::File::open(path)?;
    let mut hash = Sha256::new();
    let mut buffer = [0; 16384];
    loop {
        let length = file.read(&mut buffer)?;
        if length == 0 {
            break;
        }
        hash.update(&buffer[..length]);
    }
    Ok(format_digest(hash.finalize().into()))
}

struct Process(Child);
impl Drop for Process {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn authoring(options: &Options, directory: &Path, timings: &mut Timings) -> Result<Value> {
    eprintln!(
        "Measuring eval and retained native authoring ({} samples).",
        options.samples
    );
    let compiler = options.sdk.join("bin").join(executable("hew"));
    for sample in 0..=options.samples {
        let start = Instant::now();
        let result: Value = serde_json::from_str(&output(
            Command::new(&compiler)
                .args(["eval", "--json", "40 + 2"])
                .current_dir(directory),
        )?)?;
        if result["status"] != "ok" || result["stdout"].as_str().map(str::trim) != Some("42") {
            return Err(format!("eval returned unexpected result: {result}").into());
        }
        timings.add(
            if sample == 0 {
                "eval_first_process"
            } else {
                "eval_warm_filesystem_new_process"
            },
            start,
        );
    }
    let start = Instant::now();
    let runtime = options.sdk.join("lib/hew").join(format!(
        "{}hew{}",
        std::env::consts::DLL_PREFIX,
        std::env::consts::DLL_SUFFIX
    ));
    let mut process = Process(
        Command::new(&compiler)
            .args(["tool", "live"])
            .arg("--runtime")
            .arg(&runtime)
            .current_dir(directory)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()?,
    );
    let stdout = process.0.stdout.take().ok_or("missing live stdout")?;
    let (sender, receiver) = mpsc::channel();
    std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines() {
            if sender.send(line).is_err() {
                break;
            }
        }
    });
    let mut submit = |value: u32| -> Result<()> {
        let input = process.0.stdin.as_mut().ok_or("missing live stdin")?;
        // Two ordinary source lines replace a declaration and observe its result.
        writeln!(
            input,
            "fn answer() -> i32 {{ {value} }}\nprintln(answer());"
        )?;
        input.flush()?;
        let line = receiver.recv_timeout(Duration::from_mins(2))??;
        if line.trim() != value.to_string() {
            return Err(format!("unexpected live result: {line}").into());
        }
        Ok(())
    };
    submit(42)?;
    timings.add("live_start_to_first_result", start);
    for sample in 0..options.samples {
        let start = Instant::now();
        submit(43 + sample)?;
        timings.add("live_two_line_edit_to_first_result", start);
    }
    drop(process.0.stdin.take());
    let until = Instant::now() + Duration::from_secs(20);
    loop {
        if let Some(status) = process.0.try_wait()? {
            if !status.success() {
                return Err(format!("live shutdown: {status}").into());
            }
            break;
        }
        if Instant::now() >= until {
            return Err("live shutdown timeout".into());
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    Ok(json!({
        "compiler_version": output(Command::new(&compiler).arg("--version"))?.trim(),
        "compiler_sha256": file_digest(&compiler)?,
        "native_runtime_sha256": file_digest(&runtime)?,
        "wasi_runtime_sha256": file_digest(&options.sdk.join("lib/wasm32-wasip1/libhew_runtime.a"))?,
        "wasi_std_sha256": file_digest(&options.sdk.join("lib/wasm32-wasip1/libhew_std.a"))?,
        "benchmark_sha256": file_digest(&std::env::current_exe()?)?,
        "benchmark_source_sha256": format_digest(Sha256::digest(include_bytes!("live_loop.rs")).into()),
        "fixture_sha256": format_digest(Sha256::digest(ROUTER.as_bytes()).into()),
        "source_revision": std::env::var("HEW_LIVE_BENCH_REVISION").ok()
    }))
}

fn executable(name: &str) -> String {
    format!("{name}{}", std::env::consts::EXE_SUFFIX)
}

#[derive(Default)]
struct Providers {
    started: Arc<tokio::sync::Notify>,
    release: Arc<tokio::sync::Notify>,
    deliveries: Arc<Mutex<Option<Vec<Delivery>>>>,
}
impl Services for Providers {
    fn destination(&self, source: String) -> ServiceCall<Option<String>> {
        let started = Arc::clone(&self.started);
        let release = Arc::clone(&self.release);
        Box::pin(async move {
            tokio::task::yield_now().await;
            if source == "held" {
                started.notify_one();
                release.notified().await;
            }
            if source == "denied" {
                return Err("destination grant denied".into());
            }
            Ok(Some(source))
        })
    }
    fn emit(&self, message: Delivery) -> ServiceCall<()> {
        let deliveries = Arc::clone(&self.deliveries);
        Box::pin(async move {
            tokio::task::yield_now().await;
            if let Some(deliveries) = deliveries.lock().expect("deliveries").as_mut() {
                deliveries.push(message);
            }
            Ok(())
        })
    }
}

#[derive(Default)]
struct Pauses {
    samples: Mutex<(Option<Instant>, Vec<f64>)>,
    waiting: tokio::sync::Notify,
    events: Mutex<Option<Vec<hew_observe_protocol::LiveExecution>>>,
}
impl Observer for Pauses {
    fn record(&self, event: &hew_observe_protocol::LiveExecution) {
        if let Some(events) = self.events.lock().expect("observations").as_mut() {
            events.push(event.clone());
        }
        if event.phase == "waiting" {
            self.waiting.notify_one();
        }
        let mut state = self.samples.lock().expect("pause measurements");
        if event.phase == "snapshot" {
            state.0 = Some(Instant::now());
        }
        if event.phase == "committed" {
            if let Some(start) = state.0.take() {
                state.1.push(start.elapsed().as_secs_f64() * 1000.0);
            }
        }
    }
}

fn resident_bytes() -> Option<u64> {
    // Current RSS, not cumulative peak; unavailable on other platforms.
    let status = std::fs::read_to_string("/proc/self/status").ok()?;
    status.lines().find_map(|line| {
        line.strip_prefix("VmRSS:")
            .and_then(|v| v.split_whitespace().next())?
            .parse::<u64>()
            .ok()
            .map(|kb| kb * 1024)
    })
}

async fn runtime_loop(options: &Options, directory: &Path, timings: &mut Timings) -> Result<Value> {
    eprintln!("Measuring component preparation and cached runtime calls.");
    let toolchain = Toolchain {
        compiler: options.sdk.join("bin").join(executable("hew")),
        wasi_sdk: std::env::var_os("HEW_WASI_SDK")
            .ok_or("set HEW_WASI_SDK")?
            .into(),
        wasm_lib_dir: options.sdk.join("lib/wasm32-wasip1"),
    };
    let source = directory.join("router.hew");
    std::fs::write(&source, ROUTER)?;
    let start = Instant::now();
    let bytes = build::build(&source, &toolchain)?;
    timings.add("component_source_build", start);
    let providers = Arc::new(Providers::default());
    let pauses = Arc::new(Pauses::default());
    let host = Arc::new(Host::new(providers.clone())?.with_observer(pauses.clone()));
    let rss_before_prepare = resident_bytes();
    let start = Instant::now();
    let prepared = host.prepare(&bytes)?;
    timings.add("component_prepare", start);
    let rss_one_prepared = resident_bytes();
    let (changed, workload) = replacement_workload(
        &source, &toolchain, &host, &prepared, &providers, &pauses, timings,
    )
    .await?;
    let generations = vec![prepared, changed];
    cached_calls(options, &host, &generations, &providers, &pauses, timings).await?;
    let memory = retained_memory(options, &source, &toolchain, &host, generations).await?;
    Ok(
        json!({"component_bytes": bytes.len(), "component_sha256": format_digest(Sha256::digest(&bytes).into()), "rss_before_prepare": rss_before_prepare, "rss_one_prepared": rss_one_prepared, "replacement_under_work": workload, "retained_generations": memory}),
    )
}

async fn replacement_workload(
    source: &Path,
    toolchain: &Toolchain,
    host: &Arc<Host>,
    original: &Prepared,
    providers: &Providers,
    pauses: &Pauses,
    timings: &mut Timings,
) -> Result<(Prepared, Value)> {
    eprintln!("Editing and replacing a service with admitted work retained.");
    *providers.deliveries.lock().expect("deliveries") = Some(Vec::new());
    *pauses.events.lock().expect("observations") = Some(Vec::new());
    let deployment = Arc::new(
        host.activate(original.clone(), Snapshot { accepted: 0 })
            .await?,
    );
    if !deployment.route(event("before")).await? {
        return Err("initial service call refused".into());
    }
    if !matches!(
        deployment.route(event("denied")).await,
        Err(Error::Guest(_))
    ) {
        return Err("denied service call did not report a guest failure".into());
    }
    let mut caller = {
        let deployment = Arc::clone(&deployment);
        tokio::spawn(async move { deployment.route(event("held")).await })
    };
    tokio::select! {
        () = providers.started.notified() => {},
        result = &mut caller => {
            return Err(format!("held call ended before reaching its service: {result:?}").into());
        }
    }
    caller.abort();
    if !caller.await.is_err_and(|error| error.is_cancelled()) {
        return Err("held caller did not cancel".into());
    }

    // The admitted call stays blocked in its service while source is edited,
    // built and prepared. No activation work belongs to edit-to-ready latency.
    let edit = Instant::now();
    std::fs::write(source, ROUTER.replace("skip0", "skip1"))?;
    let start = Instant::now();
    let changed_bytes = build::build(source, toolchain)?;
    timings.add("component_source_build", start);
    let start = Instant::now();
    let changed = host.prepare(&changed_bytes)?;
    timings.add("component_prepare", start);
    timings.add("component_edit_to_ready", edit);
    if original.digest() == changed.digest() {
        return Err("edited source did not produce a distinct candidate".into());
    }

    let epoch = deployment.epoch().await;
    let start = Instant::now();
    let mut replacement = {
        let deployment = Arc::clone(&deployment);
        let changed = changed.clone();
        tokio::spawn(async move {
            deployment
                .replace(changed, epoch, Duration::from_secs(10))
                .await
        })
    };
    tokio::select! {
        () = pauses.waiting.notified() => {},
        result = &mut replacement => {
            return Err(format!("replacement ended before reaching its admission fence: {result:?}").into());
        }
    }
    // Replacement has reached the admission fence. Releasing the guest's
    // service, rather than dropping its caller, permits drain and state capture.
    providers.release.notify_one();
    let committed = replacement.await??;
    timings.add("prepared_replace_under_work_total", start);
    if committed.snapshot.accepted != 2
        || committed.digest != changed.digest()
        || committed.epoch != epoch + 1
    {
        return Err("replacement lost cancelled caller work or selected the wrong code".into());
    }
    committed.retirement.await??;
    if !deployment.route(event("skip0")).await? || deployment.route(event("skip1")).await? {
        return Err("edited service did not apply its changed route policy".into());
    }
    let restored = deployment
        .replace(original.clone(), epoch + 1, Duration::from_secs(10))
        .await?;
    if restored.snapshot.accepted != 3
        || restored.digest != original.digest()
        || restored.epoch != epoch + 2
    {
        return Err("subsequent replacement lost retained service state".into());
    }
    restored.retirement.await??;
    Arc::try_unwrap(deployment)
        .map_err(|_| "service deployment still borrowed")?
        .dispose()
        .await??;
    let result = workload_observations(providers, pauses, timings)?;
    Ok((changed, result))
}

fn event(source: &str) -> Event {
    Event {
        source: source.into(),
        body: "retained work".into(),
    }
}

fn workload_observations(
    providers: &Providers,
    pauses: &Pauses,
    timings: &mut Timings,
) -> Result<Value> {
    let events = pauses
        .events
        .lock()
        .expect("observations")
        .take()
        .ok_or("service workload observations were not retained")?;
    for admitted in events.iter().filter(|event| event.phase == "admitted") {
        let terminal: Vec<_> = events
            .iter()
            .filter(|event| {
                event.deployment_id == admitted.deployment_id
                    && event.invocation_id == admitted.invocation_id
                    && matches!(event.phase.as_str(), "completed" | "faulted")
            })
            .collect();
        if terminal.len() != 1
            || terminal[0].code_id != admitted.code_id
            || terminal[0].activation_epoch != admitted.activation_epoch
        {
            return Err("admitted work lost its terminal code identity".into());
        }
    }
    let failed = events
        .iter()
        .filter(|event| event.phase == "faulted")
        .count();
    let completed = events
        .iter()
        .filter(|event| event.phase == "completed")
        .count();
    let destinations: Vec<_> = providers
        .deliveries
        .lock()
        .expect("deliveries")
        .take()
        .ok_or("service workload deliveries were not retained")?
        .into_iter()
        .map(|delivery| delivery.destination)
        .collect();
    if failed != 1 || completed != 4 || destinations != ["before", "held", "skip0"] {
        return Err("service work was lost, duplicated or performed after refusal".into());
    }
    let samples = std::mem::take(&mut pauses.samples.lock().expect("pause measurements").1);
    if samples.len() != 2 {
        return Err("service replacement pause observations were incomplete".into());
    }
    timings
        .0
        .insert("replacement_under_work_fenced_pause", vec![samples[0]]);
    Ok(
        json!({"admitted_calls": completed + failed, "completed_calls": completed,
        "failed_calls": failed, "delivered_destinations": destinations,
        "accepted_after_edit": 2, "accepted_after_following_call": 3,
        "cancelled_caller_work_completed": true}),
    )
}

async fn cached_calls(
    options: &Options,
    host: &Arc<Host>,
    generations: &[Prepared],
    providers: &Providers,
    pauses: &Pauses,
    timings: &mut Timings,
) -> Result<()> {
    let prepared = &generations[0];
    // From here through replacement, all calls use Prepared handles: no source
    // build, cache restore or component preparation occurs inside these samples.
    for _ in 0..options.samples {
        let start = Instant::now();
        let deployment = host
            .activate(prepared.clone(), Snapshot { accepted: 0 })
            .await?;
        timings.add("prepared_activation", start);
        deployment.dispose().await??;
    }
    let deployment = host
        .activate(prepared.clone(), Snapshot { accepted: 0 })
        .await?;
    for _ in 0..options.samples {
        let start = Instant::now();
        if deployment
            .route(Event {
                source: "skip0".into(),
                body: String::new(),
            })
            .await?
        {
            return Err("filtered route was accepted".into());
        }
        timings.add("cached_invocation_no_services", start);
        let start = Instant::now();
        if !deployment
            .route(Event {
                source: "input".into(),
                body: "body".into(),
            })
            .await?
        {
            return Err("service route refused".into());
        }
        timings.add("cached_invocation_two_async_services", start);
        let start = Instant::now();
        let target = providers
            .destination("input".into())
            .await?
            .ok_or("missing destination")?;
        providers
            .emit(Delivery {
                destination: target,
                body: "body".into(),
            })
            .await?;
        timings.add("direct_two_async_services", start);
        let routed = *timings.0["cached_invocation_two_async_services"]
            .last()
            .ok_or("missing routed sample")?;
        let direct = *timings.0["direct_two_async_services"]
            .last()
            .ok_or("missing direct sample")?;
        timings
            .0
            .entry("guest_and_bridge_above_direct_services")
            .or_default()
            .push(routed - direct);
    }
    for candidate in generations
        .iter()
        .cycle()
        .skip(1)
        .take(usize::try_from(options.samples)?)
    {
        let epoch = deployment.epoch().await;
        let start = Instant::now();
        let committed = deployment
            .replace(candidate.clone(), epoch, Duration::from_secs(10))
            .await?;
        timings.add("prepared_replace_total", start);
        if committed.snapshot.accepted != u64::from(options.samples) {
            return Err("replacement lost accepted state".into());
        }
        if committed.digest != candidate.digest() || committed.epoch != epoch + 1 {
            return Err("replacement did not activate the edited candidate".into());
        }
        committed.retirement.await??;
    }
    let recorded = std::mem::take(&mut pauses.samples.lock().expect("pause measurements").1);
    if recorded.len() != usize::try_from(options.samples)? {
        return Err("replacement pause observations were incomplete".into());
    }
    timings
        .0
        .insert("replacement_admission_fenced_pause", recorded);
    deployment.dispose().await??;
    Ok(())
}

async fn retained_memory(
    options: &Options,
    source: &Path,
    toolchain: &Toolchain,
    host: &Arc<Host>,
    mut generations: Vec<Prepared>,
) -> Result<Value> {
    eprintln!(
        "Measuring RSS with {} retained generations.",
        options.generations
    );
    for generation in 2..options.generations {
        std::fs::write(
            source,
            ROUTER.replace("skip0", &format!("skip{generation}")),
        )?;
        generations.push(host.prepare(&build::build(source, toolchain)?)?);
    }
    let rss_retained_prepared = resident_bytes();
    let mut instances = Vec::new();
    for generation in &generations {
        instances.push(
            host.activate(generation.clone(), Snapshot { accepted: 0 })
                .await?,
        );
    }
    let rss_retained_instances = resident_bytes();
    for instance in instances {
        instance.dispose().await??;
    }
    let rss_disposed_instances = resident_bytes();
    drop(generations);
    let rss_released_prepared = resident_bytes();
    Ok(
        json!({"generation_count": options.generations, "rss_bytes": {"retained_prepared": rss_retained_prepared, "retained_instances": rss_retained_instances, "disposed_instances_prepared_retained": rss_disposed_instances, "released_prepared": rss_released_prepared}}),
    )
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    let mut options = Options::parse();
    options.sdk = options.sdk.canonicalize()?;
    let directory = tempfile::tempdir()?;
    let load_before = std::fs::read_to_string("/proc/loadavg").ok();
    let mut timings = Timings::default();
    let provenance = authoring(&options, directory.path(), &mut timings)?;
    let runtime = runtime_loop(&options, directory.path(), &mut timings).await?;
    let cpu = std::fs::read_to_string("/proc/cpuinfo")
        .ok()
        .and_then(|text| {
            text.lines().find_map(|line| {
                line.strip_prefix("model name")
                    .and_then(|v| v.split_once(':'))
                    .map(|(_, v)| v.trim().to_owned())
            })
        });
    println!(
        "{}",
        serde_json::to_string_pretty(&json!({
            "schema": 1, "sdk": options.sdk, "provenance": provenance,
            "machine": {"os": std::env::consts::OS, "arch": std::env::consts::ARCH, "cpu": cpu, "parallelism": std::thread::available_parallelism()?.get(), "load_before": load_before, "load_after": std::fs::read_to_string("/proc/loadavg").ok(), "benchmark_debug_assertions": cfg!(debug_assertions)},
            "definitions": {
                "cold": "first new eval process; OS caches are not flushed",
                "warm": "new eval processes after the first sample; filesystem/tool caches may be warm",
                "edit": "two source lines replace answer() then print its result in one retained hew tool live process",
                "cached": "in-memory Prepared handle; no compiler or Component::new in activation/invocation/replacement samples",
                "async_difference": "paired routed minus direct service time; includes guest actor work, marshalling and scheduling, not just import dispatch",
                "pause": "snapshot event through committed event while host admission is fenced; no concurrent ingress in this probe",
                "memory": "Linux process RSS at ownership boundaries; includes engine/allocator retention, not allocation traffic or proof of leaks",
                "target_ms": 250,
                "target": "informative nearest-rank p95 comparison, not a pass/fail gate"
            },
            "metrics": timings.report(), "runtime": runtime
        }))?
    );
    Ok(())
}
