//! Compile the real language and execute failure paths on both backends.
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
const WASI: &str = r"
const fs=require('node:fs');const{WASI}=require('node:wasi');
(async()=>{const wasi=new WASI({version:'preview1',args:[],env:{},preopens:{},returnOnExit:true});
const module=await WebAssembly.compile(fs.readFileSync(process.argv[1]));
const instance=await WebAssembly.instantiate(module,{wasi_snapshot_preview1:wasi.wasiImport});
process.exitCode=wasi.start(instance);})().catch(e=>{console.error(e);process.exitCode=125;});
";

fn run(
    command: &mut Command,
    folder: &Path,
    name: &str,
    timeout: u64,
) -> Result<(ExitStatus, String)> {
    let stdout = folder.join(format!("{name}.stdout.log"));
    let stderr = folder.join(format!("{name}.stderr.log"));
    let mut child = command
        .stdout(Stdio::from(File::create(&stdout)?))
        .stderr(Stdio::from(File::create(&stderr)?))
        .spawn()?;
    let deadline = Instant::now() + Duration::from_secs(timeout);
    loop {
        if let Some(status) = child.try_wait()? {
            let diagnostic = fs::read_to_string(&stderr)?;
            if [
                "AddressSanitizer",
                "LeakSanitizer",
                "UndefinedBehaviorSanitizer",
            ]
            .iter()
            .any(|finding| diagnostic.contains(finding))
            {
                return Err(format!("{name}: sanitizer finding; see {}", stderr.display()).into());
            }
            return Ok((status, fs::read_to_string(stdout)?));
        }
        if Instant::now() >= deadline {
            child.kill()?;
            child.wait()?;
            return Err(format!("{name}: timeout; see {}", stderr.display()).into());
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

fn main() -> Result<()> {
    let compiler = PathBuf::from(
        std::env::args()
            .nth(1)
            .ok_or("usage: failure-corpus /absolute/path/to/hew")?,
    )
    .canonicalize()?;
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let logs = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/proof-logs/failure-corpus");
    fs::create_dir_all(&logs)?;
    let mut verified = 0;
    for (case, expected, exit) in [
        ("failure-pipe-owned-cancellation", "FIRST\nSUCCESSOR\n", 0),
        ("failure-supervisor-wide-budget", "budget spent\n", 1),
    ] {
        let source = root
            .join("tests/core-acceptance/cases")
            .join(format!("{case}.hew"));
        for wasm in [false, true] {
            for level in [0, 2] {
                let name = format!("{case}-{}-O{level}", if wasm { "wasm32" } else { "native" });
                let executable = logs.join(format!("{name}{}", if wasm { ".wasm" } else { "" }));
                let mut build = Command::new(&compiler);
                build
                    .current_dir(root)
                    .args(["build"])
                    .arg(&source)
                    .args(["--opt-level", &level.to_string(), "-o"])
                    .arg(&executable);
                if wasm {
                    build.args(["--target", "wasm32-wasip1"]);
                }
                if !run(&mut build, &logs, &format!("{name}-build"), 120)?
                    .0
                    .success()
                {
                    return Err(format!("{name}: compiler failure; see {}", logs.display()).into());
                }
                for workers in if wasm { &[1][..] } else { &[1, 2, 4][..] } {
                    let run_name = format!("{name}-workers{workers}");
                    let mut command = if wasm {
                        let mut node = Command::new("node");
                        node.args(["--no-warnings", "-e", WASI]).arg(&executable);
                        node
                    } else {
                        Command::new(&executable)
                    };
                    command.env("HEW_WORKERS", workers.to_string());
                    let (status, stdout) = run(&mut command, &logs, &run_name, 30)?;
                    if status.code() != Some(exit) || stdout != expected {
                        return Err(format!("{run_name}: expected exit {exit}, stdout {expected:?}; got {status}, {stdout:?}; see {}", logs.display()).into());
                    }
                    if case == "failure-supervisor-wide-budget" {
                        let stderr =
                            fs::read_to_string(logs.join(format!("{run_name}.stderr.log")))?;
                        if stderr.matches("actor crash in ").count() != 41
                            || stderr.matches("restart budget probe").count() != 41
                        {
                            return Err(format!(
                                "{run_name}: expected exactly 41 requested actor failures; see {}",
                                logs.display()
                            )
                            .into());
                        }
                    }
                    verified += 1;
                    println!("Verified compiled failure {run_name}");
                }
            }
        }
    }
    if verified != 16 {
        return Err("failure corpus was incomplete".into());
    }
    println!("Verified compiled failure corpus: {verified} executions");
    Ok(())
}
