//! CLI argument definitions using `clap` derive macros.

use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

const EVAL_AFTER_HELP: &str = "\
REPL commands:
  :help, :h           Show command help
  :session, :show     Summarize remembered session state
  :items              List remembered top-level items
  :bindings           List persistent let/var bindings
  :type <expr>        Show the inferred type of an expression
  :load <file>        Evaluate a file in the current session
  :clear, :reset      Drop all remembered session state
";

/// The Hew programming language compiler.
#[derive(Debug, Parser)]
#[command(name = "hew", version, about, propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Command>,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Compile a .hew file through the v0.5 IR ladder.
    Compile(CompileArgs),
    /// Compile and run a .hew file.
    Run(RunArgs),
    /// Build with debug info and launch under gdb/lldb.
    Debug(DebugArgs),
    /// Parse and typecheck only.
    Check(CheckArgs),
    /// Generate documentation.
    Doc(DocArgs),
    /// Interactive REPL or evaluate expression.
    Eval(EvalArgs),
    /// Run tests.
    Test(TestArgs),
    /// Watch for changes and re-check automatically.
    Watch(WatchArgs),
    /// Wire schema compatibility tools.
    Wire(WireCommand),
    /// State machine tools.
    Machine(MachineCommand),
    /// Format source files in-place or from stdin.
    Fmt(FmtArgs),
    /// Scaffold a source-only project with `main.hew` + `README.md` (no `hew.toml`).
    Init(InitArgs),
    /// Curated playground example tools.
    Playground(PlaygroundCommand),
    /// Print shell completion script.
    Completions(CompletionsArgs),
    /// Print version info.
    Version,
}

#[derive(Debug, Args)]
pub struct CompileArgs {
    /// Input .hew file.
    pub input: PathBuf,
    /// Directory to write `<name>.ll`, `<name>.o`, `<name>.wasm.o`, and
    /// `<name>.wasm` artefacts into. Default: `.tmp/compile-out`.
    #[arg(long = "emit-dir", value_name = "DIR")]
    pub emit_dir: Option<PathBuf>,
    /// Emit a textual MIR dump and exit (no LLVM emission).
    /// Accepts `raw` (the lowered `RawMirFunction`), `checked` (the
    /// `CheckedMirFunction` after move/init/aliasing checks run), and
    /// `elab` (the elaborated MIR with drop entries). Useful for
    /// spot-checking the front-half lowering during development.
    #[arg(long = "dump-mir", value_name = "STAGE", value_parser = ["raw", "checked", "elab"])]
    pub dump_mir: Option<String>,
    /// Compilation target. Omit for native; pass `wasm32-unknown-unknown` for WASM.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
}

// ---------------------------------------------------------------------------
// Shared build options
// ---------------------------------------------------------------------------

#[derive(Debug, Args, Clone, Default)]
pub struct CommonBuildArgs {
    /// Treat warnings as errors.
    #[arg(long = "Werror")]
    pub werror: bool,
    /// Skip type-checking phase.
    #[arg(long)]
    pub no_typecheck: bool,
    /// Override package search directory (default: .adze/packages/).
    #[arg(long, value_name = "DIR")]
    pub pkg_path: Option<PathBuf>,
}

impl CommonBuildArgs {
    /// Build a base [`crate::compile::CompileOptions`] from the common flags.
    ///
    /// Per-command fields (`target`, `extra_libs`, `debug`)
    /// are left at their defaults; callers override with struct-update syntax:
    ///
    /// ```ignore
    /// crate::compile::CompileOptions {
    ///     debug: self.debug,
    ///     ..self.common.base_compile_options()
    /// }
    /// ```
    pub fn base_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            no_typecheck: self.no_typecheck,
            werror: self.werror,
            pkg_path: self.pkg_path.clone(),
            ..Default::default()
        }
    }
}

// ---------------------------------------------------------------------------
// Run
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct RunArgs {
    /// Input .hew file.
    pub input: PathBuf,
    /// Build with debug info (no optimization, no stripping).
    #[arg(long, short = 'g')]
    pub debug: bool,
    /// Enable the built-in runtime profiler.
    /// On Unix sets `HEW_PPROF=auto` (per-user unix socket, auto-discovered by `hew-observe`).
    /// On other platforms sets `HEW_PPROF=:6060` (TCP listener on port 6060).
    /// Has no effect if `HEW_PPROF` is already set in the environment.
    /// Override the address by setting `HEW_PPROF` directly instead of using this flag.
    #[arg(long)]
    pub profile: bool,
    /// Pass an extra library or linker argument to the native link step.
    #[arg(long = "link-lib", value_name = "PATH")]
    pub link_libs: Vec<String>,
    /// Target triple.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    /// Execution timeout (`500ms`, `30s`, `1m`; bare integers mean seconds).
    #[arg(long, value_name = "DURATION")]
    pub timeout: Option<String>,
    #[command(flatten)]
    pub common: CommonBuildArgs,
    /// Surface diagnostic-only stack-allocation hints from the type checker.
    ///
    /// When set, the checker's escape-analysis pass prints
    /// `info[HEW-PERF-001]` lines on stderr for each binding whose right-hand
    /// side resolves to a heap allocation class (`Vec`, `String`, `HashMap`,
    /// `HashSet`, `Rc`, closure environment). Hint accuracy is approximate in
    /// early phases; never affects exit code or program output.
    #[arg(long = "show-stack-hints")]
    pub show_stack_hints: bool,
    /// Arguments to pass to the compiled program (after --).
    #[arg(last = true)]
    pub program_args: Vec<String>,
}

#[allow(dead_code, reason = "dormant during v0.5 cutover")]
impl RunArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            target: self.target.clone(),
            extra_libs: self.link_libs.clone(),
            debug: self.debug,
            ..self.common.base_compile_options()
        }
    }
}

// ---------------------------------------------------------------------------
// Debug
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct DebugArgs {
    /// Input .hew file.
    pub input: PathBuf,
    /// Accepted for compatibility (debug info is always enabled).
    #[arg(long, short = 'g', hide = true)]
    pub debug: bool,
    /// Pass an extra library or linker argument to the native link step.
    #[arg(long = "link-lib", value_name = "PATH")]
    pub link_libs: Vec<String>,
    /// Target triple.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    #[command(flatten)]
    pub common: CommonBuildArgs,
    /// Arguments to pass to the debugger/program (after --).
    #[arg(last = true)]
    pub program_args: Vec<String>,
}

impl DebugArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            target: self.target.clone(),
            extra_libs: self.link_libs.clone(),
            debug: true,
            ..self.common.base_compile_options()
        }
    }
}

// ---------------------------------------------------------------------------
// Check
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct CheckArgs {
    /// Input .hew file.
    pub input: PathBuf,
    /// Print alias-vs-copy decision for every actor send site.
    ///
    /// Shows whether each `actor.method(arg)` call crossed the mailbox
    /// boundary via a refcount-bumped alias (no copy) or a deep-copy
    /// (the legacy path). Default off; opt-in for Phase α.
    #[arg(long)]
    pub explain_cow: bool,
    #[command(flatten)]
    pub common: CommonBuildArgs,
    /// Surface diagnostic-only stack-allocation hints from the type checker.
    ///
    /// When set, the checker's escape-analysis pass prints
    /// `info[HEW-PERF-001]` lines on stderr for each binding whose right-hand
    /// side resolves to a heap allocation class. Never affects exit code.
    #[arg(long = "show-stack-hints")]
    pub show_stack_hints: bool,
}

impl CheckArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        self.common.base_compile_options()
    }
}

// ---------------------------------------------------------------------------
// Doc
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum DocFormat {
    Html,
    Markdown,
    Md,
}

#[derive(Debug, Args)]
pub struct DocArgs {
    /// Input files or directories.
    pub input: Vec<PathBuf>,
    /// Output directory.
    #[arg(long, short = 'o', default_value = "./doc", value_name = "DIR")]
    pub output_dir: PathBuf,
    /// Output format.
    #[arg(long, short = 'f', value_enum, default_value = "html")]
    pub format: DocFormat,
    /// Open docs in browser after generation.
    #[arg(long)]
    pub open: bool,
}

// ---------------------------------------------------------------------------
// Eval
// ---------------------------------------------------------------------------

/// JIT execution mode for `hew eval`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum JitMode {
    /// Choose the execution mode automatically.
    ///
    /// Today this always selects `Inprocess` (the M1 LLJIT warm-path).
    /// Future work (#1227 crash counter) may downgrade to `Worker` when
    /// the host-death count exceeds a threshold.
    Auto,

    /// Run the compiled module in-process via LLJIT (fast, no subprocess).
    ///
    /// SHIM: stdout goes directly to the parent fd; no capture yet.
    /// WHY: the M1 keystone (#1235) requires in-process execution.
    /// WHEN obsolete: when #1228 introduces a long-lived JIT session.
    Inprocess,

    /// AOT-compile and spawn a child process (current default behaviour).
    ///
    /// Selecting this mode explicitly routes through the existing
    /// `run_inprocess_compiled` AOT+spawn path unchanged.
    Worker,
}

#[derive(Debug, Args)]
#[command(after_help = EVAL_AFTER_HELP)]
pub struct EvalArgs {
    /// Execute file in REPL context (`-` reads from stdin).
    #[arg(short = 'f')]
    pub file: Option<PathBuf>,
    /// Per-evaluation timeout (`500ms`, `30s`, `1m`; bare integers mean seconds).
    #[arg(long, default_value = "30", value_name = "DURATION")]
    pub timeout: String,
    /// Compilation target triple (e.g. `wasm32-wasi`).
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    /// JIT execution mode.
    ///
    /// `auto`      — choose automatically (today: selects `inprocess`).
    /// `inprocess` — compile and run in-process via LLJIT (no subprocess).
    /// `worker`    — AOT-compile and spawn a child process (default when
    ///               this flag is absent).
    #[arg(long, value_name = "MODE")]
    pub jit: Option<JitMode>,
    /// Emit a machine-readable JSON run contract on stdout instead of raw program output.
    ///
    /// The JSON object always contains:
    ///   `status`   — `"ok"`, `"compile_error"`, or `"runtime_failure"`
    ///   `stdout`   — captured program output (empty string when none)
    ///   `stderr`   — captured runtime stderr (empty string unless runtime failed)
    ///   `exit_code`— integer exit code (0 for compile errors)
    ///
    /// On `"compile_error"` the object also contains:
    ///   `diagnostics` — rendered compiler diagnostic text
    ///
    /// Incompatible with interactive REPL mode; requires `-f` or an inline
    /// expression.
    #[arg(long)]
    pub json: bool,
    /// Expression to evaluate (if no -f given).
    pub expr: Vec<String>,
}

// ---------------------------------------------------------------------------
// Test
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum TestFormat {
    Text,
    Junit,
}

#[derive(Debug, Args)]
pub struct TestArgs {
    /// Files or directories to test.
    pub paths: Vec<PathBuf>,
    /// Run only tests matching pattern.
    #[arg(long)]
    pub filter: Option<String>,
    /// Output format.
    #[arg(long, value_enum, default_value = "text")]
    pub format: TestFormat,
    /// Per-test timeout (`500ms`, `30s`, `1m`; bare integers mean seconds).
    #[arg(long, default_value = "30", value_name = "DURATION")]
    pub timeout: String,
    /// Disable coloured output.
    #[arg(long)]
    pub no_color: bool,
    /// Run ignored tests too.
    #[arg(long)]
    pub include_ignored: bool,
}

// ---------------------------------------------------------------------------
// Watch
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct WatchArgs {
    /// File or directory to watch.
    pub input: PathBuf,
    /// Build and run on successful check.
    #[arg(long)]
    pub run: bool,
    /// Clear terminal before each re-check.
    #[arg(long)]
    pub clear: bool,
    /// Debounce time in milliseconds.
    #[arg(long, default_value = "300")]
    pub debounce: u64,
    #[command(flatten)]
    pub common: CommonBuildArgs,
}

impl WatchArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        self.common.base_compile_options()
    }
}

// ---------------------------------------------------------------------------
// Wire
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct WireCommand {
    #[command(subcommand)]
    pub command: WireSubcommand,
}

#[derive(Debug, Subcommand)]
pub enum WireSubcommand {
    /// Check wire schema compatibility.
    Check(WireCheckArgs),
}

#[derive(Debug, Args)]
pub struct WireCheckArgs {
    /// Input .hew file with current schema.
    pub input: PathBuf,
    /// Baseline schema to check against.
    #[arg(long)]
    pub against: PathBuf,
}

// ---------------------------------------------------------------------------
// Machine
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct MachineCommand {
    #[command(subcommand)]
    pub command: MachineSubcommand,
}

#[derive(Debug, Subcommand)]
pub enum MachineSubcommand {
    /// Generate state diagram to stdout.
    Diagram(MachineDiagramArgs),
    /// List all machines with states and events.
    List(MachineListArgs),
}

/// Output format for `hew machine diagram`.
#[derive(Debug, Clone, PartialEq, ValueEnum)]
pub enum MachineFormat {
    /// Mermaid `stateDiagram-v2` (default).
    Mermaid,
    /// Graphviz DOT.
    Graphviz,
    /// Graphviz DOT (alias for `graphviz`).
    Dot,
    /// Stable JSON schema for tooling.
    Json,
}

#[derive(Debug, Args)]
pub struct MachineDiagramArgs {
    /// Input .hew file.
    pub input: PathBuf,
    /// Output Graphviz DOT format instead of Mermaid (shorthand for `--format graphviz`).
    #[arg(long, conflicts_with = "format")]
    pub dot: bool,
    /// Output format. Default: mermaid.
    #[arg(long, value_enum)]
    pub format: Option<MachineFormat>,
    /// Only render the named machine (useful for multi-machine files).
    #[arg(long = "machine", value_name = "NAME")]
    pub machine_name: Option<String>,
    /// Run HIR static checks before rendering and exit with an error if any
    /// check fails. Enabled by default.
    #[arg(long = "no-check", action = clap::ArgAction::SetFalse)]
    pub check: bool,
}

#[derive(Debug, Args)]
pub struct MachineListArgs {
    /// Input .hew file.
    pub input: PathBuf,
}

// ---------------------------------------------------------------------------
// Fmt
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct FmtArgs {
    /// Source files to format in-place.
    pub files: Vec<PathBuf>,
    /// Read source from stdin and write formatted output to stdout (cannot be used with FILES).
    #[arg(long, conflicts_with = "files")]
    pub stdin: bool,
    /// Check formatting without writing (files or stdin; exit 1 if unformatted).
    #[arg(long)]
    pub check: bool,
}

// ---------------------------------------------------------------------------
// Init
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct InitArgs {
    /// Project name (creates a directory with `main.hew` + `README.md`; omit to init in current dir).
    pub name: Option<String>,
    /// Overwrite existing scaffold files.
    #[arg(long)]
    pub force: bool,
}

// ---------------------------------------------------------------------------
// Completions
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum ShellChoice {
    Bash,
    Zsh,
    Fish,
    #[value(name = "powershell")]
    PowerShell,
}

#[derive(Debug, Args)]
pub struct CompletionsArgs {
    /// Shell to generate completions for.
    pub shell: ShellChoice,
}

// ---------------------------------------------------------------------------
// Playground
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct PlaygroundCommand {
    #[command(subcommand)]
    pub command: PlaygroundSubcommand,
}

#[derive(Debug, Subcommand)]
pub enum PlaygroundSubcommand {
    /// Compile and run each runnable playground example and verify its stdout
    /// against the checked-in `.expected` file.
    Verify(PlaygroundVerifyArgs),
}

#[derive(Debug, Args)]
pub struct PlaygroundVerifyArgs {
    /// Path to the playground manifest JSON.
    /// Defaults to `examples/playground/manifest.json` (relative to cwd).
    #[arg(long, value_name = "FILE")]
    pub manifest: Option<std::path::PathBuf>,
    /// Per-example execution timeout (`500ms`, `30s`, `1m`; bare integers mean seconds).
    #[arg(long, default_value = "30", value_name = "DURATION")]
    pub timeout: String,
}
