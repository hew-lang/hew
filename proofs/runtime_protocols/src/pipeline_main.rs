//! Execute compiler-produced histories, then check their ownership certificates.
use hew_runtime_protocol_verification::pipeline_ledger::{Event, Kind, Ledger};
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
const WASI: &str = r"
const fs=require('node:fs');const{WASI}=require('node:wasi');
(async()=>{const env={};if(process.env.HEW_DETERMINISTIC!==undefined)env.HEW_DETERMINISTIC=process.env.HEW_DETERMINISTIC;
const wasi=new WASI({version:'preview1',args:[],env,preopens:{},returnOnExit:true});
const module=await WebAssembly.compile(fs.readFileSync(process.argv[1]));
const instance=await WebAssembly.instantiate(module,{wasi_snapshot_preview1:wasi.wasiImport});
process.exitCode=wasi.start(instance);})().catch(e=>{console.error(e);process.exitCode=125;});
";

fn parse(line: &str) -> Result<Event> {
    let fields: Vec<_> = line.split_ascii_whitespace().collect();
    let n = |index: usize| -> Result<u8> { Ok(fields[index].parse()?) };
    Ok(match fields.as_slice() {
        ["I", _, kind, _, owner] => Event::Issue {
            id: n(1)?,
            kind: match *kind {
                "0" => Kind::Lease,
                "1" => Kind::Timer,
                _ => return Err("unknown resource kind".into()),
            },
            case: n(3)?,
            owner: if *owner == "-1" { 0 } else { n(4)? },
        },
        ["S", _, _] => Event::Start {
            id: n(1)?,
            owner: n(2)?,
        },
        ["C", _, _] => Event::Close {
            id: n(1)?,
            owner: n(2)?,
        },
        ["F", _, _, _] => Event::Fault {
            owner: n(1)?,
            case: n(2)?,
            kind: n(3)?,
        },
        ["Q", _] => Event::Quiescent { case: n(1)? },
        ["END"] => Event::End,
        _ => return Err(format!("unknown trace event: {line:?}").into()),
    })
}

fn certificate(events: &[Event]) -> Result<()> {
    let mut ledger = Ledger::default();
    for (index, event) in events.iter().enumerate() {
        ledger
            .apply(*event)
            .map_err(|reason| format!("event {} {event:?}: {reason}", index + 1))?;
    }
    if !ledger.ended {
        return Err("trace ended without a complete certificate".into());
    }
    Ok(())
}

fn reject_defects(events: &[Event]) -> Result<usize> {
    let close = events
        .iter()
        .position(|e| matches!(e, Event::Close { .. }))
        .ok_or("empty cleanup coverage")?;
    let start = events
        .iter()
        .position(|e| matches!(e, Event::Start { .. }))
        .ok_or("empty dispatch coverage")?;
    let fault = events
        .iter()
        .position(|e| matches!(e, Event::Fault { .. }))
        .ok_or("empty fault coverage")?;
    let quiet = events
        .iter()
        .position(|e| matches!(e, Event::Quiescent { .. }))
        .ok_or("empty quiescence coverage")?;
    let mut defects = Vec::new();
    let mut trace = events.to_vec();
    trace.insert(close, events[close]);
    defects.push(("double retirement", trace));
    let mut trace = events.to_vec();
    trace.remove(close);
    defects.push(("missing cleanup", trace));
    let mut trace = events.to_vec();
    trace.insert(start, events[start]);
    defects.push(("duplicate handler", trace));
    let mut trace = events.to_vec();
    if let Event::Start { id, .. } = trace[start] {
        trace[start] = Event::Start { id, owner: 0 };
    }
    defects.push(("wrong incarnation", trace));
    let mut trace = events.to_vec();
    if let Event::Fault { owner, case, kind } = trace[fault] {
        trace[fault] = Event::Fault {
            owner,
            case,
            kind: 1 - kind,
        };
    }
    defects.push(("wrong failure classification", trace));
    let mut trace = events.to_vec();
    trace.remove(fault);
    defects.push(("lost fault", trace));
    let mut trace = events.to_vec();
    trace.remove(quiet);
    defects.push(("lost quiescence", trace));
    let mut trace = events.to_vec();
    trace.insert(0, events[quiet]);
    defects.push(("premature quiescence", trace));
    let mut trace = events.to_vec();
    trace.insert(0, Event::End);
    defects.push(("premature success", trace));
    let mut trace = events.to_vec();
    trace.push(events[0]);
    defects.push(("work after final success", trace));
    let mut trace = events.to_vec();
    trace.remove(0);
    defects.push(("missing allocation identity", trace));
    for (name, trace) in &defects {
        if certificate(trace).is_ok() {
            return Err(format!("validator accepted {name}").into());
        }
        println!("Rejected pipeline trace defect: {name}");
    }
    Ok(defects.len())
}

fn run(command: &mut Command, logs: &Path, name: &str) -> Result<String> {
    let stdout = logs.join(format!("{name}.stdout.log"));
    let stderr = logs.join(format!("{name}.stderr.log"));
    fs::write(
        logs.join(format!("{name}.command.log")),
        format!("{command:?}\n"),
    )?;
    let mut child = command
        .stdout(Stdio::from(File::create(&stdout)?))
        .stderr(Stdio::from(File::create(&stderr)?))
        .spawn()?;
    let deadline = Instant::now() + Duration::from_secs(120);
    loop {
        if let Some(status) = child.try_wait()? {
            let diagnostic = fs::read_to_string(&stderr)?;
            if !status.success()
                || [
                    "AddressSanitizer",
                    "LeakSanitizer",
                    "UndefinedBehaviorSanitizer",
                ]
                .iter()
                .any(|finding| diagnostic.contains(finding))
            {
                return Err(format!(
                    "pipeline {name}: execution failed ({status}); see {}",
                    logs.display()
                )
                .into());
            }
            return Ok(fs::read_to_string(stdout)?);
        }
        if Instant::now() >= deadline {
            child.kill()?;
            child.wait()?;
            return Err(format!(
                "pipeline {name}: process did not finish; see {}",
                logs.display()
            )
            .into());
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let first = args
        .next()
        .ok_or("usage: pipeline /path/to/hew [seed-count] | --check-trace FILE")?;
    if first == "--check-trace" {
        let path = PathBuf::from(args.next().ok_or("missing trace file")?);
        if args.next().is_some() {
            return Err("unexpected arguments after trace file".into());
        }
        let events = fs::read_to_string(&path)?
            .lines()
            .map(parse)
            .collect::<Result<Vec<_>>>()?;
        certificate(&events)?;
        if reject_defects(&events)? != 11 {
            return Err("empty defect control coverage".into());
        }
        println!("Verified pipeline certificate {}", path.display());
        return Ok(());
    }
    let compiler = PathBuf::from(first).canonicalize()?;
    let seeds: u32 = args.next().map_or(Ok(16), |text| text.parse())?;
    if seeds == 0 || args.next().is_some() {
        return Err("nonzero seed count and one compiler path required".into());
    }
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("cases/timer-pipeline.hew");
    let logs = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/proof-logs/pipeline");
    fs::create_dir_all(&logs)?;
    let mut checked = 0;
    let mut controls = 0;
    let asan = std::env::var_os("HEW_PIPELINE_ASAN").as_deref() == Some(std::ffi::OsStr::new("1"));
    if asan && !cfg!(target_os = "linux") {
        return Err("generated-code ASan campaign currently requires Linux".into());
    }
    let mut backends = vec![(false, false), (true, false)];
    if asan {
        backends.push((false, true));
    }
    for (wasm, sanitized) in backends {
        for level in [0, 2] {
            let name = format!(
                "{}-O{level}",
                if wasm {
                    "wasi"
                } else if sanitized {
                    "native-asan"
                } else {
                    "native"
                }
            );
            let executable = logs.join(format!("{name}{}", if wasm { ".wasm" } else { "" }));
            let mut build = Command::new(&compiler);
            build
                .current_dir(root)
                .args(["build"])
                .arg(&source)
                .args(["--opt-level", &level.to_string(), "-o"])
                .arg(&executable);
            build.env_remove("HEW_SANITIZE_ADDRESS");
            if sanitized {
                build
                    .env("HEW_SANITIZE_ADDRESS", "1")
                    .env("HEW_CC", "clang");
            }
            if wasm {
                build.args(["--target", "wasm32-wasip1"]);
            }
            run(&mut build, &logs, &format!("{name}-build"))?;
            let mut schedules =
                vec![("fifo".into(), Some("schedule=fifo,budget=100000".into()), 1)];
            let seed_count = if sanitized { seeds.min(2) } else { seeds };
            for seed in 0..seed_count {
                schedules.push((
                    format!("seed-{seed}"),
                    Some(format!("schedule=random,seed={seed},budget=100000")),
                    1,
                ));
            }
            for workers in if wasm { &[1][..] } else { &[1, 2, 4][..] } {
                schedules.push((format!("clock-workers-{workers}"), None, *workers));
            }
            for (schedule, config, workers) in schedules {
                let label = format!("{name}-{schedule}");
                let mut command = if wasm {
                    let mut node = Command::new("node");
                    node.args(["--no-warnings", "-e", WASI]).arg(&executable);
                    node
                } else {
                    Command::new(&executable)
                };
                command
                    .env_remove("HEW_DETERMINISTIC")
                    .env("HEW_WORKERS", workers.to_string());
                if sanitized {
                    command.env("ASAN_OPTIONS", "detect_leaks=1:halt_on_error=1:exitcode=99");
                }
                if let Some(config) = config {
                    command.env("HEW_DETERMINISTIC", config);
                }
                let output = run(&mut command, &logs, &label)?;
                let events = output.lines().map(parse).collect::<Result<Vec<_>>>()?;
                certificate(&events).map_err(|error| {
                    format!("pipeline {label}: {error}; trace in {}", logs.display())
                })?;
                if controls == 0 {
                    controls = reject_defects(&events)?;
                }
                checked += 1;
                println!("Verified pipeline {label} (32 histories, 80 owned identities)");
            }
        }
    }
    let expected =
        (seeds + 4) * 2 + (seeds + 2) * 2 + if asan { (seeds.min(2) + 4) * 2 } else { 0 };
    if checked != expected || controls != 11 {
        return Err("pipeline campaign was incomplete".into());
    }
    println!(
        "Verified {checked} executions, {} histories, {} owned lifetimes",
        checked * 32,
        checked * 80
    );
    Ok(())
}
