//! `hew` — the Hew programming language compiler driver.
//!
//! ```text
//! hew build file.hew [-o output]   # Compile to executable
//! hew run file.hew [-- args...]    # Compile and run
//! hew debug file.hew [-- args...]  # Build with debug info + launch gdb/lldb
//! hew check file.hew               # Parse + typecheck only
//! hew watch file_or_dir [options]  # Watch for changes and re-check
//! hew eval                         # Interactive REPL
//! hew eval "<expression>"          # Evaluate expression
//! hew eval -f file.hew             # Execute file in REPL context
//! hew wire check file.hew --against baseline.hew
//!                                  # Validate wire compatibility
//! hew fmt file.hew                 # Format source file in-place
//! hew fmt --check file.hew         # Check formatting (CI mode)
//! hew init [name]                  # Scaffold a new project
//! hew completions <shell>          # Print shell completion script
//! hew version                      # Print version info
//! ```

mod args;
mod compile;
mod diagnostic;
mod doc;
mod eval;
mod link;
mod machine;
mod manifest;
mod platform;
#[cfg(unix)]
mod signal;
mod target;
mod test_runner;
mod util;
mod watch;
mod wire;

use std::path::Path;

use args::{Cli, Command};
use clap::Parser;

fn main() {
    // Spawn the real entry point on a thread with a large stack so deeply
    // nested ASTs (e.g. thousands of chained binary operators) don't cause
    // a stack overflow in the parser, type checker, or serializer.
    const STACK_SIZE: usize = 64 * 1024 * 1024; // 64 MiB
    let builder = std::thread::Builder::new()
        .name("hew-main".into())
        .stack_size(STACK_SIZE);
    let handler = builder
        .spawn(hew_main)
        .expect("failed to spawn main thread");
    if let Err(e) = handler.join() {
        std::panic::resume_unwind(e);
    }
}

fn hew_main() {
    // Try normal clap parse first; if it fails and the first arg looks like
    // a .hew file, re-parse as `hew build <file> [rest...]`.
    let cli = match Cli::try_parse() {
        Ok(cli) => cli,
        Err(e) => {
            let raw_args: Vec<String> = std::env::args().collect();
            if raw_args.len() >= 2
                && Path::new(&raw_args[1])
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("hew"))
            {
                let mut new_args = vec![raw_args[0].clone(), "build".into()];
                new_args.extend_from_slice(&raw_args[1..]);
                Cli::parse_from(new_args)
            } else {
                e.exit();
            }
        }
    };

    match cli.command {
        Some(Command::Build(ref a)) => cmd_build(a),
        Some(Command::Run(ref a)) => cmd_run(a),
        Some(Command::Debug(ref a)) => cmd_debug(a),
        Some(Command::Check(ref a)) => cmd_check(a),
        Some(Command::Doc(ref a)) => doc::cmd_doc(a),
        Some(Command::Eval(ref a)) => eval::cmd_eval(a),
        Some(Command::Test(ref a)) => test_runner::cmd_test(a),
        Some(Command::Watch(ref a)) => watch::cmd_watch(a),
        Some(Command::Wire(ref a)) => wire::cmd_wire(a),
        Some(Command::Machine(ref a)) => machine::cmd_machine(a),
        Some(Command::Fmt(ref a)) => cmd_fmt(a),
        Some(Command::Init(ref a)) => cmd_init(a),
        Some(Command::Completions(ref a)) => cmd_completions(a),
        Some(Command::Version) => cmd_version(),
        None => {
            // No subcommand — shouldn't normally happen since clap shows help,
            // but handle gracefully.
            let _ = <Cli as clap::CommandFactory>::command().print_help();
            eprintln!();
            std::process::exit(1);
        }
    }
}

// ---------------------------------------------------------------------------
// Sub-commands
// ---------------------------------------------------------------------------

fn cmd_build(a: &args::BuildArgs) {
    let input = a.input.display().to_string();
    let output = a.output.as_ref().map(|p| p.display().to_string());
    let options = a.to_compile_options();
    match compile::compile(&input, output.as_deref(), false, &options) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

fn cmd_run(a: &args::RunArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target_spec =
        target::TargetSpec::from_requested(options.target.as_deref()).unwrap_or_else(|e| {
            eprintln!("{e}");
            std::process::exit(1);
        });

    if !target_spec.can_run_on_host() {
        eprintln!("{}", target_spec.cross_target_run_error("run"));
        std::process::exit(1);
    }

    // Compile to a temporary binary
    let tmp_path = tempfile::Builder::new()
        .prefix("hew_run_")
        .suffix(target_spec.executable_suffix())
        .tempfile()
        .unwrap_or_else(|e| {
            eprintln!("Error: cannot create temp file: {e}");
            std::process::exit(1);
        })
        .into_temp_path();
    let tmp_bin = tmp_path.display().to_string();

    match compile::compile(&input, Some(&tmp_bin), false, &options) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{e}");
            drop(tmp_path);
            // Exit 125 = compile failure (sentinel used by the playground to
            // distinguish compile errors from program exit codes).
            std::process::exit(125);
        }
    }

    // Run the compiled binary, holding a Child handle so signals sent directly
    // to `hew run` also terminate the compiled program instead of orphaning it.
    let mut child = match std::process::Command::new(&tmp_bin)
        .args(&a.program_args)
        .spawn()
    {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: cannot run compiled binary: {e}");
            drop(tmp_path);
            std::process::exit(1);
        }
    };

    // Forward SIGTERM/SIGINT to the child so that signals sent directly to the
    // wrapper also terminate the compiled program instead of orphaning it.
    #[cfg(unix)]
    signal::forward_signals_to_child(child.id());

    let status = child.wait();

    // Drop TempPath to clean up before exit (std::process::exit skips destructors)
    drop(tmp_path);

    match status {
        Ok(s) => std::process::exit(s.code().unwrap_or(1)),
        Err(e) => {
            eprintln!("Error: cannot run compiled binary: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_check(a: &args::CheckArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    match compile::compile(&input, None, true, &options) {
        Ok(_) => {
            eprintln!("{input}: OK");
        }
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

fn cmd_debug(a: &args::DebugArgs) {
    let input = a.input.display().to_string();
    let options = a.to_compile_options();
    let target_spec =
        target::TargetSpec::from_requested(options.target.as_deref()).unwrap_or_else(|e| {
            eprintln!("{e}");
            std::process::exit(1);
        });

    if !target_spec.can_debug_on_host() {
        eprintln!("{}", target_spec.cross_target_run_error("debug"));
        std::process::exit(1);
    }

    // Compile to a temporary binary with debug info
    let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
        eprintln!("Error: cannot create temp dir: {e}");
        std::process::exit(1);
    });
    let debug_bin_name = format!("hew_debug_bin{}", target_spec.executable_suffix());
    let tmp_bin = tmp_dir.path().join(debug_bin_name);
    let tmp_bin_str = tmp_bin.display().to_string();

    match compile::compile(&input, Some(&tmp_bin_str), false, &options) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(125);
        }
    }

    // Find a debugger: prefer gdb, fall back to lldb
    let (debugger, debugger_args) = if which_exists("gdb") {
        // Load the Hew GDB helper script if it exists
        let gdb_script = find_debug_script("hew-gdb.py");
        let mut gdb_args = Vec::new();
        if let Some(script) = &gdb_script {
            gdb_args.push("-x".to_string());
            gdb_args.push(script.clone());
        }
        gdb_args.push("--args".to_string());
        gdb_args.push(tmp_bin_str.clone());
        gdb_args.extend(a.program_args.iter().cloned());
        ("gdb".to_string(), gdb_args)
    } else if which_exists("lldb") {
        let lldb_script = find_debug_script("hew_lldb.py");
        let mut lldb_args = Vec::new();
        if let Some(script) = &lldb_script {
            lldb_args.push("-o".to_string());
            lldb_args.push(format!("command script import {script}"));
        }
        lldb_args.push("--".to_string());
        lldb_args.push(tmp_bin_str.clone());
        lldb_args.extend(a.program_args.iter().cloned());
        ("lldb".to_string(), lldb_args)
    } else {
        eprintln!("Error: no debugger found. Install gdb or lldb.");
        std::process::exit(1);
    };

    eprintln!("Launching {debugger} with debug build of {input}...");

    let status = std::process::Command::new(&debugger)
        .args(&debugger_args)
        .status();

    // Clean up
    drop(tmp_dir);

    match status {
        Ok(s) => std::process::exit(s.code().unwrap_or(1)),
        Err(e) => {
            eprintln!("Error: cannot launch {debugger}: {e}");
            std::process::exit(1);
        }
    }
}

fn which_exists(name: &str) -> bool {
    std::process::Command::new(name)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}

fn find_debug_script(name: &str) -> Option<String> {
    // Check next to the hew binary first, then the repo scripts/ dir
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            let candidate = dir.join(format!("../share/hew/{name}"));
            if candidate.exists() {
                return candidate
                    .canonicalize()
                    .ok()
                    .map(|p| p.display().to_string());
            }
            // Development layout
            let candidate = dir.join(format!("../../scripts/debug/{name}"));
            if candidate.exists() {
                return candidate
                    .canonicalize()
                    .ok()
                    .map(|p| p.display().to_string());
            }
        }
    }
    None
}

fn cmd_fmt(a: &args::FmtArgs) {
    if a.files.is_empty() {
        eprintln!("Usage: hew fmt [--check] <file.hew>...");
        std::process::exit(1);
    }

    let mut had_errors = false;
    let mut needs_formatting = false;

    for file_path in &a.files {
        let file = file_path.display().to_string();
        let source = match std::fs::read_to_string(file_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: cannot read {file}: {e}");
                had_errors = true;
                continue;
            }
        };

        let result = hew_parser::parse(&source);
        let is_fatal = result
            .errors
            .iter()
            .any(|e| matches!(e.severity, hew_parser::Severity::Error));
        if is_fatal {
            for err in &result.errors {
                eprintln!("{file}: {err:?}");
            }
            had_errors = true;
            continue;
        }

        let formatted = hew_parser::fmt::format_source(&source, &result.program);

        if a.check {
            if formatted != source {
                eprintln!("{file}: needs formatting");
                needs_formatting = true;
            }
        } else if formatted != source {
            if let Err(e) = std::fs::write(file_path, &formatted) {
                eprintln!("Error: cannot write {file}: {e}");
                had_errors = true;
            } else {
                eprintln!("Formatted {file}");
            }
        }
    }

    if had_errors || needs_formatting {
        std::process::exit(1);
    }
}

fn cmd_init(a: &args::InitArgs) {
    let (project_name, project_dir) = if let Some(ref name) = a.name {
        let dir = std::path::PathBuf::from(name);
        let pname = dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("hew-project")
            .to_string();
        if dir.exists() && !a.force {
            eprintln!(
                "Error: directory '{}' already exists (use --force to overwrite)",
                dir.display()
            );
            std::process::exit(1);
        }
        if let Err(e) = std::fs::create_dir_all(&dir) {
            eprintln!("Error: cannot create directory '{}': {e}", dir.display());
            std::process::exit(1);
        }
        (pname, dir)
    } else {
        // No name given — use current directory name as project name.
        let cwd = std::env::current_dir().unwrap_or_else(|e| {
            eprintln!("Error: cannot determine current directory: {e}");
            std::process::exit(1);
        });
        let pname = cwd
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("hew-project")
            .to_string();
        (pname, cwd)
    };

    let main_hew = project_dir.join("main.hew");
    let readme = project_dir.join("README.md");

    // Guard against overwriting existing files unless --force is given.
    if !a.force {
        for path in [&main_hew, &readme] {
            if path.exists() {
                eprintln!(
                    "Error: '{}' already exists (use --force to overwrite)",
                    path.display()
                );
                std::process::exit(1);
            }
        }
    }

    let main_content = "\
fn main() {
    println(\"Hello, world!\");
}
";

    let readme_content = format!(
        "\
# {project_name}

A [Hew](https://hew.sh) project.

## Build & Run

```sh
hew build main.hew -o {project_name}
./{project_name}
```
"
    );

    if let Err(e) = std::fs::write(&main_hew, main_content) {
        eprintln!("Error: cannot write {}: {e}", main_hew.display());
        std::process::exit(1);
    }
    if let Err(e) = std::fs::write(&readme, &readme_content) {
        eprintln!("Error: cannot write {}: {e}", readme.display());
        std::process::exit(1);
    }

    println!("Created project \"{project_name}\" with main.hew");
}

fn cmd_completions(a: &args::CompletionsArgs) {
    use clap::CommandFactory;
    use clap_complete::{generate, Shell};

    let mut cmd = Cli::command();
    let shell = match a.shell {
        args::ShellChoice::Bash => Shell::Bash,
        args::ShellChoice::Zsh => Shell::Zsh,
        args::ShellChoice::Fish => Shell::Fish,
        args::ShellChoice::PowerShell => Shell::PowerShell,
    };
    generate(shell, &mut cmd, "hew", &mut std::io::stdout());
}

fn cmd_version() {
    let version = env!("CARGO_PKG_VERSION");
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    let git_hash = option_env!("HEW_GIT_HASH").unwrap_or("");
    let dirty = if option_env!("HEW_GIT_DIRTY") == Some("true") {
        "-dirty"
    } else {
        ""
    };
    if git_hash.is_empty() {
        println!("hew {version} ({profile})");
    } else {
        println!("hew {version} ({profile}, {git_hash}{dirty})");
    }
}

// ---------------------------------------------------------------------------
// Helper utilities
// ---------------------------------------------------------------------------
