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
    /// Compile a .hew file to an executable.
    Build(BuildArgs),
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

// ---------------------------------------------------------------------------
// Shared build options
// ---------------------------------------------------------------------------

#[derive(Debug, Args, Clone, Default)]
pub struct CommonBuildArgs {
    /// Accepted for spec compatibility (no-op).
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
    /// Per-command fields (`target`, `extra_libs`, `debug`, `codegen_mode`)
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
            pkg_path: self.pkg_path.clone(),
            ..Default::default()
        }
    }
}

// ---------------------------------------------------------------------------
// Build
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "emit flags are mutually exclusive via clap group, not independent bools"
)]
pub struct BuildArgs {
    /// Input .hew file.
    pub input: PathBuf,
    /// Output path. Defaults to `<input stem><target suffix>`.
    #[arg(short = 'o', value_name = "FILE")]
    pub output: Option<PathBuf>,
    /// Build with debug info (no optimization, no stripping).
    #[arg(long, short = 'g')]
    pub debug: bool,
    /// Pass an extra library or linker argument to the native link step.
    #[arg(long = "link-lib", value_name = "PATH")]
    pub link_libs: Vec<String>,
    /// Target triple.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    /// Emit enriched AST as JSON.
    #[arg(long, group = "emit_mode")]
    pub emit_ast: bool,
    /// Emit full codegen IR as JSON (same as msgpack, for debugging).
    #[arg(long, group = "emit_mode")]
    pub emit_json: bool,
    /// Emit full codegen IR as msgpack.
    #[arg(long, group = "emit_mode")]
    pub emit_msgpack: bool,
    /// Emit MLIR instead of linking.
    #[arg(long, group = "emit_mode")]
    pub emit_mlir: bool,
    /// Emit LLVM IR instead of linking.
    #[arg(long, group = "emit_mode")]
    pub emit_llvm: bool,
    /// Emit object code instead of linking.
    #[arg(long, group = "emit_mode")]
    pub emit_obj: bool,
    #[command(flatten)]
    pub common: CommonBuildArgs,
}

impl BuildArgs {
    pub fn codegen_mode(&self) -> crate::compile::CodegenMode {
        use crate::compile::CodegenMode;
        if self.emit_ast {
            CodegenMode::EmitAst
        } else if self.emit_json {
            CodegenMode::EmitJson
        } else if self.emit_msgpack {
            CodegenMode::EmitMsgpack
        } else if self.emit_mlir {
            CodegenMode::EmitMlir
        } else if self.emit_llvm {
            CodegenMode::EmitLlvm
        } else if self.emit_obj {
            CodegenMode::EmitObj
        } else {
            CodegenMode::LinkExecutable
        }
    }

    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            codegen_mode: self.codegen_mode(),
            target: self.target.clone(),
            extra_libs: self.link_libs.clone(),
            debug: self.debug,
            ..self.common.base_compile_options()
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
    /// Execution timeout in seconds.
    #[arg(long)]
    pub timeout: Option<u64>,
    #[command(flatten)]
    pub common: CommonBuildArgs,
    /// Arguments to pass to the compiled program (after --).
    #[arg(last = true)]
    pub program_args: Vec<String>,
}

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
    #[command(flatten)]
    pub common: CommonBuildArgs,
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

#[derive(Debug, Args)]
#[command(after_help = EVAL_AFTER_HELP)]
pub struct EvalArgs {
    /// Execute file in REPL context (`-` reads from stdin).
    #[arg(short = 'f')]
    pub file: Option<PathBuf>,
    /// Per-evaluation timeout in seconds.
    #[arg(long, default_value = "30")]
    pub timeout: u64,
    /// Compilation target triple (e.g. `wasm32-wasi`).
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
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
    /// Per-test timeout in seconds.
    #[arg(long, default_value = "30")]
    pub timeout: u64,
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

#[derive(Debug, Args)]
pub struct MachineDiagramArgs {
    /// Input .hew file.
    pub input: PathBuf,
    /// Output Graphviz DOT format instead of Mermaid.
    #[arg(long)]
    pub dot: bool,
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
    /// Per-example execution timeout in seconds.
    #[arg(long, default_value = "30")]
    pub timeout: u64,
}
