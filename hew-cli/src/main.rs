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

mod compile;
mod diagnostic;
mod doc;
mod eval;
mod link;
mod manifest;
mod test_runner;
mod watch;
mod wire;

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
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        print_usage();
        std::process::exit(1);
    }

    match args[1].as_str() {
        "build" => cmd_build(&args[2..]),
        "run" => cmd_run(&args[2..]),
        "debug" => cmd_debug(&args[2..]),
        "check" => cmd_check(&args[2..]),
        "doc" => doc::cmd_doc(&args[2..]),
        "eval" => eval::cmd_eval(&args[2..]),
        "test" => test_runner::cmd_test(&args[2..]),
        "watch" => watch::cmd_watch(&args[2..]),
        "wire" => wire::cmd_wire(&args[2..]),
        "fmt" => cmd_fmt(&args[2..]),
        "init" => cmd_init(&args[2..]),
        "completions" => cmd_completions(&args[2..]),
        "version" | "--version" | "-V" => cmd_version(),
        "help" | "--help" | "-h" => {
            print_usage();
        }
        // `hew file.hew` is shorthand for `hew build file.hew`
        arg if std::path::Path::new(arg)
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("hew")) =>
        {
            cmd_build(&args[1..]);
        }
        other => {
            eprintln!("Unknown command: {other}");
            print_usage();
            std::process::exit(1);
        }
    }
}

// ---------------------------------------------------------------------------
// Sub-commands
// ---------------------------------------------------------------------------

fn cmd_build(args: &[String]) {
    let build_args = parse_build_args(args);
    match compile::compile(
        &build_args.input,
        build_args.output.as_deref(),
        false,
        &build_args.options,
    ) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

fn cmd_run(args: &[String]) {
    // Split at `--` to separate compiler args from program args.
    let (compiler_args, program_args) = match args.iter().position(|a| a == "--") {
        Some(pos) => (&args[..pos], &args[pos + 1..]),
        None => (args, [].as_slice()),
    };

    let build_args = parse_build_args(compiler_args);
    if build_args.options.codegen_mode != compile::CodegenMode::LinkExecutable {
        eprintln!("Error: run does not support --emit-* options");
        std::process::exit(1);
    }

    // Compile to a temporary binary
    let tmp_path = tempfile::Builder::new()
        .prefix("hew_run_")
        .tempfile()
        .unwrap_or_else(|e| {
            eprintln!("Error: cannot create temp file: {e}");
            std::process::exit(1);
        })
        .into_temp_path();
    let tmp_bin = tmp_path.display().to_string();

    match compile::compile(
        &build_args.input,
        Some(&tmp_bin),
        false,
        &build_args.options,
    ) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{e}");
            drop(tmp_path);
            // Exit 125 = compile failure (sentinel used by the playground to
            // distinguish compile errors from program exit codes).
            std::process::exit(125);
        }
    }

    // Run the compiled binary
    let status = std::process::Command::new(&tmp_bin)
        .args(program_args)
        .status();

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

fn cmd_check(args: &[String]) {
    let build_args = parse_build_args(args);
    match compile::compile(&build_args.input, None, true, &build_args.options) {
        Ok(_) => {
            eprintln!("{}: OK", build_args.input);
        }
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

fn cmd_debug(args: &[String]) {
    // Split at `--` to separate compiler args from program args.
    let (compiler_args, program_args) = match args.iter().position(|a| a == "--") {
        Some(pos) => (&args[..pos], &args[pos + 1..]),
        None => (args, [].as_slice()),
    };

    let mut build_args = parse_build_args(compiler_args);
    build_args.options.debug = true;

    if build_args.options.codegen_mode != compile::CodegenMode::LinkExecutable {
        eprintln!("Error: debug does not support --emit-* options");
        std::process::exit(1);
    }

    // Compile to a temporary binary with debug info
    let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
        eprintln!("Error: cannot create temp dir: {e}");
        std::process::exit(1);
    });
    let tmp_bin = tmp_dir.path().join("hew_debug_bin");
    let tmp_bin_str = tmp_bin.display().to_string();

    match compile::compile(
        &build_args.input,
        Some(&tmp_bin_str),
        false,
        &build_args.options,
    ) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(125);
        }
    }

    // Find a debugger: prefer gdb, fall back to lldb
    let (debugger, debugger_args) = if which_exists("gdb") {
        // Load the Hew GDB helper script if it exists
        let gdb_script = find_gdb_script();
        let mut gdb_args = Vec::new();
        if let Some(script) = &gdb_script {
            gdb_args.push("-x".to_string());
            gdb_args.push(script.clone());
        }
        gdb_args.push("--args".to_string());
        gdb_args.push(tmp_bin_str.clone());
        gdb_args.extend(program_args.iter().cloned());
        ("gdb".to_string(), gdb_args)
    } else if which_exists("lldb") {
        let mut lldb_args = vec!["--".to_string(), tmp_bin_str.clone()];
        lldb_args.extend(program_args.iter().cloned());
        ("lldb".to_string(), lldb_args)
    } else {
        eprintln!("Error: no debugger found. Install gdb or lldb.");
        std::process::exit(1);
    };

    eprintln!(
        "Launching {debugger} with debug build of {}...",
        build_args.input
    );

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
    std::process::Command::new("which")
        .arg(name)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}

fn find_gdb_script() -> Option<String> {
    // Check next to the hew binary first, then the repo scripts/ dir
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            let candidate = dir.join("../share/hew/hew-gdb.py");
            if candidate.exists() {
                return candidate
                    .canonicalize()
                    .ok()
                    .map(|p| p.display().to_string());
            }
            // Development layout
            let candidate = dir.join("../../scripts/debug/hew-gdb.py");
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

fn cmd_fmt(args: &[String]) {
    let mut check_mode = false;
    let mut files: Vec<String> = Vec::new();

    for arg in args {
        match arg.as_str() {
            "--check" => check_mode = true,
            a if a.starts_with('-') => {
                eprintln!("Unknown option: {a}");
                std::process::exit(1);
            }
            _ => files.push(arg.clone()),
        }
    }

    if files.is_empty() {
        eprintln!("Usage: hew fmt [--check] <file.hew>...");
        std::process::exit(1);
    }

    let mut had_errors = false;
    let mut needs_formatting = false;

    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: cannot read {file}: {e}");
                had_errors = true;
                continue;
            }
        };

        let result = hew_parser::parse(&source);
        if !result.errors.is_empty() {
            for err in &result.errors {
                eprintln!("{file}: {err:?}");
            }
            had_errors = true;
            continue;
        }

        let formatted = hew_parser::fmt::format_source(&source, &result.program);

        if check_mode {
            if formatted != source {
                eprintln!("{file}: needs formatting");
                needs_formatting = true;
            }
        } else if formatted != source {
            if let Err(e) = std::fs::write(file, &formatted) {
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

fn cmd_init(args: &[String]) {
    let force = args.iter().any(|a| a == "--force");
    let name_arg = args.iter().find(|a| !a.starts_with('-'));

    let (project_name, project_dir) = if let Some(name) = name_arg {
        let dir = std::path::PathBuf::from(name);
        let pname = dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("hew-project")
            .to_string();
        if dir.exists() && !force {
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
    if !force {
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
fn main() -> i32 {
    println(\"Hello, world!\");
    0
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

fn cmd_completions(args: &[String]) {
    if args.is_empty() {
        eprintln!("Usage: hew completions <bash|zsh|fish>");
        std::process::exit(1);
    }
    match args[0].as_str() {
        "bash" => print!("{}", include_str!("../../completions/hew.bash")),
        "zsh" => print!("{}", include_str!("../../completions/hew.zsh")),
        "fish" => print!("{}", include_str!("../../completions/hew.fish")),
        other => {
            eprintln!("Unknown shell: {other}");
            eprintln!("Supported shells: bash, zsh, fish");
            std::process::exit(1);
        }
    }
}

fn cmd_version() {
    let version = env!("CARGO_PKG_VERSION");
    println!("hew {version}");
}

// ---------------------------------------------------------------------------
// Argument parsing helpers
// ---------------------------------------------------------------------------

struct BuildArgs {
    input: String,
    output: Option<String>,
    options: compile::CompileOptions,
}

fn parse_build_args(args: &[String]) -> BuildArgs {
    let mut input = None;
    let mut output = None;
    let mut options = compile::CompileOptions::default();
    let mut i = 0;

    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: -o requires an argument");
                    std::process::exit(1);
                }
                output = Some(args[i].clone());
            }
            "--Werror" => {
                options.werror = true;
            }
            "--no-typecheck" => {
                options.no_typecheck = true;
            }
            "--debug" | "-g" => {
                options.debug = true;
            }
            "--emit-ast" => {
                set_codegen_mode(&mut options, compile::CodegenMode::EmitAst);
            }
            "--emit-json" => {
                set_codegen_mode(&mut options, compile::CodegenMode::EmitJson);
            }
            "--emit-mlir" => {
                set_codegen_mode(&mut options, compile::CodegenMode::EmitMlir);
            }
            "--emit-llvm" => {
                set_codegen_mode(&mut options, compile::CodegenMode::EmitLlvm);
            }
            "--emit-obj" => {
                set_codegen_mode(&mut options, compile::CodegenMode::EmitObj);
            }
            s if s.starts_with("--target=") => {
                options.target = Some(s.strip_prefix("--target=").unwrap().to_string());
            }
            "--pkg-path" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --pkg-path requires an argument");
                    std::process::exit(1);
                }
                options.pkg_path = Some(std::path::PathBuf::from(&args[i]));
            }
            s if s.starts_with("--pkg-path=") => {
                options.pkg_path = Some(std::path::PathBuf::from(
                    s.strip_prefix("--pkg-path=").unwrap(),
                ));
            }
            _ => {
                if input.is_none() {
                    input = Some(args[i].clone());
                } else {
                    eprintln!("Error: unexpected argument '{}'", args[i]);
                    std::process::exit(1);
                }
            }
        }
        i += 1;
    }

    let Some(input) = input else {
        eprintln!("Error: no input file specified");
        print_usage();
        std::process::exit(1);
    };

    BuildArgs {
        input,
        output,
        options,
    }
}

fn set_codegen_mode(options: &mut compile::CompileOptions, mode: compile::CodegenMode) {
    if options.codegen_mode != compile::CodegenMode::LinkExecutable {
        eprintln!("Error: only one --emit-* option may be specified");
        std::process::exit(1);
    }
    options.codegen_mode = mode;
}

fn print_usage() {
    eprintln!(
        "\
Usage: hew <command> [options]

Commands:
  build <file.hew> [-o output]    Compile to executable
  run <file.hew> [-- args...]     Compile and run
  debug <file.hew> [-- args...]   Build with debug info and launch under gdb/lldb
  check <file.hew>                Parse + typecheck only
  watch <file_or_dir> [options]   Watch for changes and re-check automatically
  doc <file_or_dir> [options]     Generate HTML documentation
  eval [expr | -f file]           Interactive REPL or evaluate expression
  test [file|dir] [options]       Run tests
  wire check <file.hew> --against <baseline.hew>
                                  Check wire schema compatibility
  fmt <file.hew>... [--check]     Format source files in-place
  init [name]                     Scaffold a new project
  completions <shell>             Print shell completion script
  version                         Print version info
  help                            Show this message

Build/check options:
  --Werror                        Accepted for spec compatibility (no-op)
  --no-typecheck                  Skip type-checking phase
  --debug, -g                     Build with debug info (no optimization, no stripping)
  --emit-ast                      Emit enriched AST as JSON
  --emit-json                     Emit full codegen IR as JSON (same as msgpack, for debugging)
  --emit-mlir                     Emit MLIR instead of linking
  --emit-llvm                     Emit LLVM IR instead of linking
  --emit-obj                      Emit object code instead of linking
  --pkg-path <dir>                Override package search directory (default: .adze/packages/)

Fmt options:
  --check                         Check formatting without writing (exit 1 if unformatted)

Doc options:
  --output-dir <dir>              Output directory (default: ./doc)
  --open                          Open docs in browser after generation
Watch options:
  --run                           Build and run on successful check
  --clear                         Clear terminal before each re-check
  --debounce <ms>                 Debounce time in milliseconds (default: 300)
Test options:
  --filter <pattern>              Run only tests matching pattern
  --no-color                      Disable colored output
  --include-ignored               Run ignored tests too

Shell completions:
  hew completions bash            Print bash completion script
  hew completions zsh             Print zsh completion script
  hew completions fish            Print fish completion script

Shorthand:
  hew file.hew                    Same as: hew build file.hew"
    );
}
