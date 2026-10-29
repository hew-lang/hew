//! Metamorphic execution complements the AST proofs: each real machine program
//! must keep its observable outcome across optimization and word width.
use std::error::Error;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use serde::Deserialize;

type Result<T> = std::result::Result<T, Box<dyn Error>>;
const WASI: &str = r"const fs=require('node:fs');const{WASI}=require('node:wasi');
(async()=>{const wasi=new WASI({version:'preview1',args:[],env:{},preopens:{},returnOnExit:true});
const module=await WebAssembly.compile(fs.readFileSync(process.argv[1]));
const instance=await WebAssembly.instantiate(module,{wasi_snapshot_preview1:wasi.wasiImport});
process.exitCode=wasi.start(instance);})().catch(e=>{console.error(e);process.exitCode=125;});";

#[derive(Debug, PartialEq, Eq)]
struct Outcome {
    status: Option<i32>,
    stdout: Vec<u8>,
}
#[derive(Deserialize)]
struct Manifest {
    case: Vec<Case>,
}

#[derive(Deserialize)]
struct Case {
    source: PathBuf,
    expected: Expected,
}

#[derive(Deserialize)]
struct Expected {
    stdout: Option<String>,
    stdout_file: Option<PathBuf>,
    exit: Option<i32>,
}

fn expected_outcome(source: &Path, root: &Path) -> Result<Outcome> {
    let manifest_path = source.with_extension("toml");
    let manifest: Manifest = toml::from_str(&fs::read_to_string(&manifest_path)?)?;
    let acceptance = root.join("tests/core-acceptance");
    let source = source.canonicalize()?;
    let matches: Vec<_> = manifest
        .case
        .into_iter()
        .filter(|case| acceptance.join(&case.source).canonicalize().ok().as_ref() == Some(&source))
        .collect();
    let [case] = matches.as_slice() else {
        return Err(format!(
            "{} must contain exactly one case for {}",
            manifest_path.display(),
            source.display()
        )
        .into());
    };
    let stdout = match (&case.expected.stdout, &case.expected.stdout_file) {
        (Some(text), None) => text.as_bytes().to_vec(),
        (None, Some(path)) => fs::read(acceptance.join(path))?,
        _ => {
            return Err(format!(
                "{} must declare exactly one of stdout and stdout_file",
                manifest_path.display()
            )
            .into());
        }
    };
    let exit = case
        .expected
        .exit
        .ok_or_else(|| format!("{} must declare an exit status", manifest_path.display()))?;
    Ok(Outcome {
        status: Some(exit),
        stdout,
    })
}

fn check_expected(label: &str, expected: &Outcome, observed: &Outcome) -> Result<()> {
    if expected != observed {
        return Err(format!(
            "machine outcome incorrect for {label}: expected {expected:?}, observed {observed:?}"
        )
        .into());
    }
    Ok(())
}
fn run(mut command: Command, prefix: &Path, timeout: Duration) -> Result<Outcome> {
    fs::write(
        prefix.with_extension("command.log"),
        format!("{command:?}\n"),
    )?;
    let stdout = prefix.with_extension("stdout.log");
    let stderr = prefix.with_extension("stderr.log");
    let mut child = command
        .stdout(Stdio::from(File::create(&stdout)?))
        .stderr(Stdio::from(File::create(&stderr)?))
        .spawn()?;
    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait()? {
            return Ok(Outcome {
                status: status.code(),
                stdout: fs::read(&stdout)?,
            });
        }
        if start.elapsed() > timeout {
            child.kill()?;
            child.wait()?;
            return Err(format!("machine campaign timed out: {}", prefix.display()).into());
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

pub(super) fn verify(compiler: &Path, files: &[PathBuf], root: &Path) -> Result<()> {
    let folder = std::env::var_os("HEW_MACHINE_CODEGEN_DIR")
        .map_or_else(|| root.join("proofs/target/machine-codegen"), PathBuf::from);
    fs::create_dir_all(&folder)?;
    let scratch = folder.join(format!("binaries-{}", std::process::id()));
    fs::create_dir_all(&scratch)?;
    let mut executions = 0;
    for source in files {
        let name = source
            .file_stem()
            .ok_or("machine file has no name")?
            .to_string_lossy();
        let original = fs::read(source)?;
        let expected = expected_outcome(source, root)?;
        let mut baseline = None;
        for (wasm, opt) in [(false, 0), (false, 2), (true, 0), (true, 2)] {
            let label = format!("{name}-{}-O{opt}", if wasm { "wasi" } else { "native" });
            let binary = scratch.join(if wasm { "machine.wasm" } else { "machine" });
            let mut command = Command::new(compiler);
            command
                .args(["build"])
                .arg(source)
                .args(["--opt-level", &opt.to_string(), "-o"])
                .arg(&binary);
            if wasm {
                command.args(["--target", "wasm32-wasip1"]);
            }
            let outcome = run(
                command,
                &folder.join(format!("{label}-compile")),
                Duration::from_mins(2),
            )?;
            if outcome.status != Some(0) {
                return Err(format!(
                    "machine compilation failed: {label}; see {}",
                    folder.display()
                )
                .into());
            }
            let mut command = if wasm {
                let mut cmd = Command::new("node");
                cmd.args(["--no-warnings", "-e", WASI]).arg(&binary);
                cmd
            } else {
                Command::new(&binary)
            };
            command.env("HEW_WORKERS", "1");
            let result = run(command, &folder.join(&label), Duration::from_secs(30))?;
            check_expected(&label, &expected, &result)?;
            if let Some(expected) = &baseline {
                if *expected != result {
                    return Err(format!("machine outcome changed for {label}: expected {expected:?}, observed {result:?}; see {}",folder.display()).into());
                }
            } else {
                baseline = Some(result);
            }
            executions += 1;
            fs::remove_file(binary)?;
        }
        if original != fs::read(source)? {
            return Err(format!(
                "machine source changed during validation: {}",
                source.display()
            )
            .into());
        }
        println!("Verified compiled machine {name}: native/WASI at O0/O2");
    }
    if executions != files.len() * 4 || executions == 0 {
        return Err("compiled machine campaign was incomplete".into());
    }
    println!(
        "Verified {executions} compiled machine executions across {} source programs",
        files.len()
    );
    Ok(())
}
