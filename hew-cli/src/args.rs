//! CLI argument definitions using `clap` derive macros.

use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

const EVAL_AFTER_HELP: &str = "\
`hew eval` is a lightweight, one-shot evaluator: it runs a single expression, a
piped snippet, or a program from `-f <file>`, and offers an interactive session
for quick exploration.

Persistence model: top-level definitions (fn/struct/enum/actor/impl/trait) are
remembered across lines so you can define a function then call it on the next
line. let/var bindings and bare expression-statements are evaluated fresh each
line and do NOT carry over — their side effects (file writes, channel sends, …)
execute exactly once and never replay. For a multi-statement program with
persistent state, use `hew eval -f <file>` or `hew run`.

REPL commands:
  :help, :h           Show command help
  :session, :show     List remembered top-level definitions
  :items              List remembered top-level items
  :type <expr>        Show the inferred type of an expression
  :load <file>        Evaluate a file in the current session
  :clear, :reset      Drop all remembered definitions
";

/// The Hew programming language compiler.
#[derive(Debug, Parser)]
#[command(
    name = "hew",
    version = env!("HEW_VERSION"),
    about,
    propagate_version = true
)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Command>,
}

/// Selects how a compile-capable command uses Hew Semantic IR (SIR).
///
/// This is deliberately shared by every command that can reach native or
/// WASM code generation. Keeping the selection at the CLI boundary prevents
/// commands such as `run` and `test` from accidentally taking a different
/// compiler lane than `compile` and `build`.
#[derive(Debug, Args, Clone, Copy, Default)]
pub struct SirModeArgs {
    /// Compile a closed scalar direct-call graph through SIR → raw/checked MIR
    /// with no legacy function-body lowering. A reachable unsupported feature
    /// is an explicit error; it never falls back to established MIR.
    #[arg(long = "sir-lower")]
    pub sir_lower: bool,
}

impl SirModeArgs {
    /// Convert parsed flags into the one compiler-driver mode threaded through
    /// every compile-capable command.
    pub fn mode(self) -> crate::compile::SirMode {
        if self.sir_lower {
            crate::compile::SirMode::Lower
        } else {
            crate::compile::SirMode::Disabled
        }
    }
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Compile a .hew file through the v0.5 IR ladder.
    ///
    /// Superseded by `hew tool compile`. Kept as a hidden alias so the
    /// Makefile, release scripts, and the bare `hew <file>.hew` fallback keep
    /// working while they migrate.
    #[command(hide = true)]
    Compile(CompileArgs),
    /// Compile and run a .hew file.
    Run(RunArgs),
    /// Build with debug info and launch under gdb/lldb.
    Debug(DebugArgs),
    /// Parse and typecheck only.
    Check(CheckArgs),
    /// Compile a .hew file to a native binary on disk (like `go build`).
    ///
    /// Uses O0 by default; pass `--release` to use the O2 release pipeline.
    Build(BuildArgs),
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
    /// Format Hew source files or project directories in-place, or read stdin.
    Fmt(FmtArgs),
    /// Scaffold a manifest-first project (`hew.toml` + starter source + merged `.gitignore`).
    Init(InitArgs),
    /// Curated playground example tools.
    ///
    /// Superseded by `hew tool playground-verify`. Kept as a hidden alias so
    /// CI keeps working while it migrates.
    #[command(hide = true)]
    Playground(PlaygroundCommand),
    /// Compiler-internal tools.
    Tool(ToolCommand),
    /// Print shell completion script.
    Completions(CompletionsArgs),
    /// Print version info.
    Version,
    /// Launch the TUI actor observer (`hew-observe`).
    ///
    /// Forwards all arguments to `hew-observe`. Requires the `hew-observe`
    /// binary to be available in the same directory as `hew` or on PATH.
    ///
    /// Examples:
    ///   hew observe --demo
    ///   hew observe --addr localhost:6060
    ///   hew observe --list
    Observe(ObserveArgs),
    /// Start the Hew Language Server (`hew-lsp`).
    ///
    /// Forwards all arguments to `hew-lsp`. Requires the `hew-lsp`
    /// binary to be available in the same directory as `hew` or on PATH.
    /// Communicates via stdin/stdout using the Language Server Protocol.
    ///
    /// Examples:
    ///   hew lsp
    ///   hew lsp --version
    Lsp(LspArgs),
    #[command(flatten)]
    Pkg(hew_pkg::cli::PkgCommand),
}

#[allow(
    clippy::struct_excessive_bools,
    reason = "clap maps independent compile switches directly to booleans; collapsing them would obscure the CLI contract"
)]
#[derive(Debug, Args)]
pub struct CompileArgs {
    /// Input .hew file.
    pub input: PathBuf,
    /// Directory to write `<name>.ll`, `<name>.o`, `<name>.wasm.o`, and
    /// `<name>.wasm` artefacts into. Default: `.tmp/compile-out`.
    #[arg(long = "emit-dir", value_name = "DIR")]
    pub emit_dir: Option<PathBuf>,
    /// Retain the pre-optimization textual LLVM IR beside emitted artifacts.
    #[arg(long = "emit-llvm")]
    pub emit_llvm: bool,
    /// Emit a textual MIR dump and exit (no LLVM emission).
    /// Accepts `raw` (the lowered `RawMirFunction`), `checked` (the
    /// `CheckedMirFunction` after move/init/aliasing checks run), and
    /// `elab` (the elaborated MIR with drop entries). Useful for
    /// spot-checking the front-half lowering during development.
    #[arg(long = "dump-mir", value_name = "STAGE", value_parser = ["raw", "checked", "elab"])]
    pub dump_mir: Option<String>,
    #[command(flatten)]
    pub sir: SirModeArgs,
    /// Emit the verified Semantic IR subset and exit. The subset is what the
    /// entry reaches: lowering is demand-driven, so a compilation unit with no
    /// entry emits no bodies at all, and a declaration the entry never calls is
    /// reported as unreached rather than lowered. Every body the entry demands
    /// but SIR cannot lower is reported explicitly with its reason. No MIR or
    /// LLVM artifacts are emitted.
    #[arg(long = "dump-sir", conflicts_with_all = ["dump_mir", "sir_lower"])]
    pub dump_sir: bool,
    /// Compilation target. Omit for native; pass `wasm32-unknown-unknown` for WASM.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    /// LLVM middle-end optimization level: `0` (default, no optimization) or
    /// `2` (the `default<O2>` release pipeline — inlining, SROA, GVN, loop opts).
    #[arg(long = "opt-level", value_parser = ["0", "2"], default_value = "0", value_name = "LEVEL")]
    pub opt_level: String,
    /// Diagnostic output format: `text` (default) or `json`.
    #[arg(long, value_enum, default_value_t = DiagnosticFormat::Text, value_name = "FORMAT")]
    pub format: DiagnosticFormat,
}

// ---------------------------------------------------------------------------
// Diagnostic output format
// ---------------------------------------------------------------------------

/// Output format for compiler diagnostics on `check`, `run`, and `compile`.
///
/// `text` is the human-readable Rust/Elm-style rendering with source context
/// and `^^^` underlines. `json` emits a machine-readable JSON array of
/// structured diagnostics on stdout — one object per diagnostic with a stable
/// `code`, `severity`, `file`, `span`, `message`, `notes`, and machine-checked
/// `fixes`. The JSON shape mirrors the LSP diagnostic substrate so agentic
/// tooling can apply fixes without re-parsing prose.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, ValueEnum)]
pub enum DiagnosticFormat {
    /// Human-readable diagnostics with source context (default).
    #[default]
    Text,
    /// Machine-readable JSON array of structured diagnostics on stdout.
    Json,
}

// ---------------------------------------------------------------------------
// Lint level flags
// ---------------------------------------------------------------------------

/// One `--allow` / `--warn` / `--deny` value: a specific lint name or the
/// `all` wildcard. Parsed and validated by [`parse_lint_selector`] so an
/// unknown lint fails closed at the CLI boundary rather than being ignored.
#[derive(Debug, Clone)]
pub enum LintSelector {
    /// Every known lint (`all`).
    All,
    /// A single named lint.
    One(hew_types::LintId),
}

impl LintSelector {
    fn is_all(&self) -> bool {
        matches!(self, LintSelector::All)
    }
}

/// Parse a `--allow`/`--warn`/`--deny` value into a [`LintSelector`].
///
/// Accepts any known [`hew_types::LintId`] name or `all`; an unknown name is a
/// hard CLI error listing the known lints (fail closed — a misspelled lint must
/// never be silently dropped).
fn parse_lint_selector(value: &str) -> Result<LintSelector, String> {
    if value == "all" {
        return Ok(LintSelector::All);
    }
    hew_types::LintId::from_name(value)
        .map(LintSelector::One)
        .ok_or_else(|| {
            let known = hew_types::LintId::ALL
                .iter()
                .map(|id| id.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            format!("unknown lint `{value}` (known lints: {known}, all)")
        })
}

/// Resolve the CLI lint-level flags into a [`hew_types::LintLevels`].
///
/// Starts from every lint's default level, then applies the flags. clap
/// collects the three flags into independent lists, so their interleaved argv
/// order is not recoverable through the derive API; conflicts are therefore
/// resolved deterministically by *specificity then severity* rather than by
/// position: a specific lint name always overrides an `all` wildcard, and
/// within the same specificity a stronger level wins (`deny` > `warn` >
/// `allow`). Thus `--deny all --allow needless_range_loop` denies everything
/// except that one lint, and `--allow all --deny needless_range_loop` does the
/// reverse.
fn resolve_lint_levels(
    allow: &[LintSelector],
    warn: &[LintSelector],
    deny: &[LintSelector],
) -> hew_types::LintLevels {
    use hew_types::{LintId, LintLevel, LintLevels};
    let mut levels = LintLevels::from_defaults();
    // Ascending severity so deny overwrites warn overwrites allow when two
    // flags name the same target.
    let by_severity = [
        (LintLevel::Allow, allow),
        (LintLevel::Warn, warn),
        (LintLevel::Deny, deny),
    ];
    // Pass 1: `all` wildcards (broad); pass 2: specific names (narrow) win.
    for want_all in [true, false] {
        for (level, selectors) in &by_severity {
            for selector in selectors.iter().filter(|s| s.is_all() == want_all) {
                match selector {
                    LintSelector::All => {
                        for id in LintId::ALL.iter().copied() {
                            levels.set(id, *level);
                        }
                    }
                    LintSelector::One(id) => levels.set(*id, *level),
                }
            }
        }
    }
    levels
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
    /// Override package search directory (default: .hew/packages/).
    #[arg(long, value_name = "DIR")]
    pub pkg_path: Option<PathBuf>,
    /// Override project directory for manifest and package resolution.
    #[arg(long, value_name = "DIR")]
    pub project_dir: Option<PathBuf>,
    /// Downgrade a lint to `allow` (suppress it). Repeatable; accepts a lint
    /// name (e.g. `needless_range_loop`) or `all`.
    #[arg(long = "allow", short = 'A', value_name = "LINT", value_parser = parse_lint_selector)]
    pub allow: Vec<LintSelector>,
    /// Set a lint to `warn`. Repeatable; accepts a lint name or `all`.
    #[arg(long = "warn", short = 'W', value_name = "LINT", value_parser = parse_lint_selector)]
    pub warn: Vec<LintSelector>,
    /// Promote a lint to `deny` (a hard error). Repeatable; accepts a lint
    /// name or `all`.
    #[arg(long = "deny", short = 'D', value_name = "LINT", value_parser = parse_lint_selector)]
    pub deny: Vec<LintSelector>,
}

impl CommonBuildArgs {
    /// Build a base [`crate::compile::CompileOptions`] from the common flags.
    ///
    /// Per-command fields such as `target` are left at their defaults; callers
    /// override with struct-update syntax:
    ///
    /// ```ignore
    /// crate::compile::CompileOptions { target: self.target.clone(), ..self.common.base_compile_options() }
    /// ```
    pub fn base_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            no_typecheck: self.no_typecheck,
            werror: self.werror,
            pkg_path: self.pkg_path.clone(),
            project_dir: self.project_dir.clone(),
            lint_levels: resolve_lint_levels(&self.allow, &self.warn, &self.deny),
            ..Default::default()
        }
    }
}

// ---------------------------------------------------------------------------
// Run
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct RunArgs {
    /// Input .hew file, or a package directory. Omit it to run the package
    /// enclosing the current directory.
    pub input: Option<PathBuf>,
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
    /// Values may begin with a dash (e.g. `-lMagickWand-7.Q16`).
    #[arg(long = "link-lib", value_name = "PATH", allow_hyphen_values = true)]
    pub link_libs: Vec<String>,
    /// Target triple.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    /// Execution timeout (`500ms`, `30s`, `1m`; bare integers mean seconds).
    #[arg(long, value_name = "DURATION")]
    pub timeout: Option<String>,
    #[command(flatten)]
    pub common: CommonBuildArgs,
    #[command(flatten)]
    pub sir: SirModeArgs,
    /// Surface diagnostic-only stack-allocation hints from the type checker.
    ///
    /// When set, the checker's escape-analysis pass prints
    /// `info[HEW-PERF-001]` lines on stderr for each binding whose right-hand
    /// side resolves to a heap allocation class (`Vec`, `String`, `HashMap`,
    /// `HashSet`, `Rc`, closure environment). Hint accuracy is approximate in
    /// early phases; never affects exit code or program output.
    #[arg(long = "show-stack-hints")]
    pub show_stack_hints: bool,
    /// Diagnostic output format: `text` (default) or `json`.
    #[arg(long, value_enum, default_value_t = DiagnosticFormat::Text, value_name = "FORMAT")]
    pub format: DiagnosticFormat,
    /// Arguments to pass to the compiled program (after --).
    #[arg(last = true)]
    pub program_args: Vec<String>,
}

impl RunArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            target: self.target.clone(),
            sir_mode: self.sir.mode(),
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
    /// Values may begin with a dash (e.g. `-lMagickWand-7.Q16`).
    #[arg(long = "link-lib", value_name = "PATH", allow_hyphen_values = true)]
    pub link_libs: Vec<String>,
    /// Target triple.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    #[command(flatten)]
    pub common: CommonBuildArgs,
    #[command(flatten)]
    pub sir: SirModeArgs,
    /// Arguments to pass to the debugger/program (after --).
    #[arg(last = true)]
    pub program_args: Vec<String>,
}

impl DebugArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            target: self.target.clone(),
            sir_mode: self.sir.mode(),
            ..self.common.base_compile_options()
        }
    }
}

// ---------------------------------------------------------------------------
// Check
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct CheckArgs {
    /// Input .hew file, or a package directory. Omit it to check the package
    /// enclosing the current directory.
    pub input: Option<PathBuf>,
    /// Print alias-vs-copy decision for every actor send site.
    ///
    /// Shows whether each `actor.method(arg)` call crossed the mailbox
    /// boundary via a refcount-bumped alias (no copy) or a deep-copy
    /// (the compatibility path). Default off; enable it explicitly.
    #[arg(long)]
    pub explain_cow: bool,
    /// Target triple.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    #[command(flatten)]
    pub common: CommonBuildArgs,
    /// Surface diagnostic-only stack-allocation hints from the type checker.
    ///
    /// When set, the checker's escape-analysis pass prints
    /// `info[HEW-PERF-001]` lines on stderr for each binding whose right-hand
    /// side resolves to a heap allocation class. Never affects exit code.
    #[arg(long = "show-stack-hints")]
    pub show_stack_hints: bool,
    /// Diagnostic output format: `text` (default) or `json`.
    #[arg(long, value_enum, default_value_t = DiagnosticFormat::Text, value_name = "FORMAT")]
    pub format: DiagnosticFormat,
}

impl CheckArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            target: self.target.clone(),
            ..self.common.base_compile_options()
        }
    }
}

// ---------------------------------------------------------------------------
// Build
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
#[allow(
    clippy::struct_excessive_bools,
    reason = "clap maps these independent command-line switches directly"
)]
pub struct BuildArgs {
    /// Input .hew file, or a package directory. Omit it to build the package
    /// enclosing the current directory.
    pub input: Option<PathBuf>,
    /// Output binary path. Default: `<package-root>/<package-name>` for a
    /// package, `./<stem>` for an explicit file (no extension on Unix targets,
    /// `.exe` on Windows targets). Ignored with `--emit-obj`.
    #[arg(long, short = 'o', value_name = "PATH")]
    pub output: Option<PathBuf>,
    /// Target triple. Omit for host native; e.g. `arm64-apple-darwin`,
    /// `x86_64-unknown-linux-gnu`, `x86_64-pc-windows-gnu`,
    /// `wasm32-unknown-unknown`.
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,
    /// Emit a relocatable object file instead of a linked binary, written to
    /// `<cwd>/<stem><.o|.obj>`. Required for foreign-OS targets that cannot be
    /// linked on this host.
    #[arg(long = "emit-obj")]
    pub emit_obj: bool,
    /// Retain the pre-optimization textual LLVM IR beside the output.
    #[arg(long = "emit-llvm")]
    pub emit_llvm: bool,
    /// Use the O2 release pipeline. Explicit `--opt-level` takes precedence.
    #[arg(long, short = 'r')]
    pub release: bool,
    /// Build with debug info (no optimization, no stripping).
    ///
    /// Emits DWARF debug info into the native object. gdb and lldb read this
    /// faithfully on Linux (ELF), macOS, and FreeBSD. On `windows-msvc` the
    /// output is COFF with `CodeView` plus a sidecar `.pdb`; `windows-gnu`
    /// remains the PE/COFF GNU-target path with embedded DWARF, which no
    /// debugger check currently proves (the live-debugger suite is excluded
    /// on Windows and the PDB check covers only `windows-msvc`).
    #[arg(long, short = 'g')]
    pub debug: bool,
    /// LLVM middle-end optimization level: `0` (default, no optimization) or
    /// `2` (the `default<O2>` release pipeline). Independent of `-g`: `-g`
    /// remains O0 unless `--opt-level 2` is also passed (which produces
    /// lower-fidelity DWARF — inlined frames, optimized-out locals).
    #[arg(
        long = "opt-level",
        value_parser = ["0", "2"],
        default_value = "0",
        default_value_if("release", "true", "2"),
        value_name = "LEVEL"
    )]
    pub opt_level: String,
    /// Pass an extra library or linker argument to the native link step.
    /// Values may begin with a dash (e.g. `-lMagickWand-7.Q16`).
    #[arg(long = "link-lib", value_name = "PATH", allow_hyphen_values = true)]
    pub link_libs: Vec<String>,
    #[command(flatten)]
    pub common: CommonBuildArgs,
    #[command(flatten)]
    pub sir: SirModeArgs,
    /// Diagnostic output format: `text` (default) or `json`.
    #[arg(long, value_enum, default_value_t = DiagnosticFormat::Text, value_name = "FORMAT")]
    pub format: DiagnosticFormat,
}

impl BuildArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            target: self.target.clone(),
            sir_mode: self.sir.mode(),
            ..self.common.base_compile_options()
        }
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
    /// Choose the best available execution backend automatically.
    ///
    /// Today this is the AOT compile-and-spawn path: the in-process LLJIT
    /// backend (#1227/#1235) is not implemented yet, so `auto` falls back to
    /// AOT rather than failing. A future LLJIT backend will be selected here
    /// transparently.
    Auto,

    /// Run the compiled module in-process via LLJIT (no subprocess).
    ///
    /// Currently unavailable: the Rust v0.5 `ORCv2`/LLJIT bridge is not
    /// implemented yet (#1227/#1235), so this mode fails closed with an
    /// explanatory error. Use `auto` (or omit `--jit`) for the AOT path.
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
    /// `auto`      — choose the best available backend (today: AOT, since the
    ///               in-process LLJIT backend is not implemented yet).
    /// `inprocess` — in-process LLJIT; currently unavailable (#1227/#1235) and
    ///               fails closed with an explanatory error.
    /// `worker`    — AOT-compile and spawn a child process (default when this
    ///               flag is absent).
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
    /// Suppress the interactive REPL's startup banner and help line.
    ///
    /// The banner is already suppressed automatically when stdout is not a
    /// terminal (piped or redirected); this flag suppresses it explicitly,
    /// including in an interactive terminal.
    #[arg(long, short = 'q')]
    pub quiet: bool,
    /// Select the SIR lowering lane for every evaluation compiled by this
    /// command, including native AOT, WASM AOT, and interactive REPL input.
    #[command(flatten)]
    pub sir: SirModeArgs,
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
#[allow(
    clippy::struct_excessive_bools,
    reason = "clap maps these independent command-line switches directly"
)]
pub struct TestArgs {
    /// Files or directories to test.
    pub paths: Vec<PathBuf>,
    /// List discovered test identities without compiling or running them.
    #[arg(long)]
    pub list: bool,
    /// Run only tests matching pattern.
    #[arg(long)]
    pub filter: Option<String>,
    /// Run one stable hash partition (`hash:SHARD/TOTAL`, one-based).
    #[arg(long, value_name = "PARTITION")]
    pub partition: Option<String>,
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
    /// Exit 0 when no test files or functions are discovered.
    #[arg(long)]
    pub allow_empty: bool,
    /// Maximum number of tests to compile or execute concurrently.
    ///
    /// Defaults to the host's physical core count, capped to avoid excessive
    /// memory pressure from concurrent compilation tasks.
    #[arg(long, short = 'j', value_name = "N")]
    pub jobs: Option<std::num::NonZeroUsize>,
    #[command(flatten)]
    pub sir: SirModeArgs,
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
    /// SIR flags are accepted only with `--run`, whose build goes through the
    /// ordinary native compilation path. Check-only watches stop at the
    /// frontend and therefore cannot truthfully select an IR lane.
    #[command(flatten)]
    pub sir: WatchSirModeArgs,
}

impl WatchArgs {
    pub fn to_compile_options(&self) -> crate::compile::CompileOptions {
        crate::compile::CompileOptions {
            sir_mode: self.sir.mode(),
            ..self.common.base_compile_options()
        }
    }
}

/// SIR mode selection for `hew watch`.
///
/// A check-only watch deliberately stops before MIR/SIR realization. Requiring
/// `--run` at parse time prevents `--sir-lower` from being accepted and then
/// silently doing nothing.
#[derive(Debug, Args, Clone, Copy, Default)]
pub struct WatchSirModeArgs {
    /// Compile the watched program through the strict closed SIR direct-call
    /// lane. Reachable unsupported features are errors, never fallbacks.
    #[arg(long = "sir-lower", requires = "run")]
    pub sir_lower: bool,
}

impl WatchSirModeArgs {
    pub fn mode(self) -> crate::compile::SirMode {
        if self.sir_lower {
            crate::compile::SirMode::Lower
        } else {
            crate::compile::SirMode::Disabled
        }
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
    /// Skip HIR static checks before rendering. Checks run by default and fail
    /// closed on invalid machine declarations.
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
    /// Hew source files or directories to format in-place (use --root for directory migration).
    #[arg(value_name = "PATH")]
    pub files: Vec<PathBuf>,
    /// Read source from stdin and write formatted output to stdout (cannot be used with PATHS).
    #[arg(long, conflicts_with = "files")]
    pub stdin: bool,
    /// Check formatting without writing (paths or stdin; exit 1 if unformatted).
    #[arg(long)]
    pub check: bool,
    /// Rewrite legacy path and variant syntax before formatting.
    #[arg(long, conflicts_with = "stdin")]
    pub migrate: bool,
    /// Recursively migrate every Hew source below this directory.
    #[arg(long, value_name = "DIR", conflicts_with_all = ["stdin", "files"])]
    pub root: Option<PathBuf>,
}

// ---------------------------------------------------------------------------
// Init
// ---------------------------------------------------------------------------

#[derive(Debug, Args)]
pub struct InitArgs {
    /// Project name (creates the directory if needed; omit to init the current directory).
    pub name: Option<String>,
    /// Scaffold a library project (`<final-package-segment>.hew`).
    #[arg(long, conflicts_with = "actor")]
    pub lib: bool,
    /// Scaffold an actor project.
    #[arg(long)]
    pub actor: bool,
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

// ---------------------------------------------------------------------------
// Tool
// ---------------------------------------------------------------------------

/// The compiler-internal surface: harnesses the compiler team drives, not
/// commands a Hew program's author reaches for. Keeping them behind one
/// namespace lets the public help stay about building and shipping code.
#[derive(Debug, Args)]
pub struct ToolCommand {
    #[command(subcommand)]
    pub command: ToolSubcommand,
}

#[derive(Debug, Subcommand)]
pub enum ToolSubcommand {
    /// Compile a .hew file through the v0.5 IR ladder.
    Compile(CompileArgs),
    /// Compile and run each runnable playground example and verify its stdout
    /// against the checked-in `.expected` file.
    #[command(name = "playground-verify")]
    PlaygroundVerify(PlaygroundVerifyArgs),
    /// Inventory every root item in the given `.hew` files (directories are
    /// recursed) and report whether SIR admits it or the legacy lowerer still
    /// owns it, with a coverage total.
    #[command(name = "sir-coverage")]
    SirCoverage(SirCoverageArgs),
}

/// Arguments for `hew tool sir-coverage`.
#[derive(Debug, Args)]
pub struct SirCoverageArgs {
    /// `.hew` files or directories to inventory; directories are recursed.
    #[arg(required = true, value_name = "PATH")]
    pub paths: Vec<std::path::PathBuf>,
    /// Emit the inventory as JSON on stdout instead of one line per item.
    #[arg(long)]
    pub json: bool,
    /// Compare the coverage percentage with the one recorded in FILE and
    /// fail when it dropped.
    #[arg(long, value_name = "FILE")]
    pub ratchet: Option<std::path::PathBuf>,
}

// ---------------------------------------------------------------------------
// Observe
// ---------------------------------------------------------------------------

/// Arguments forwarded verbatim to `hew-observe`.
#[derive(Debug, Args)]
#[command(disable_help_flag = true)]
pub struct ObserveArgs {
    /// Arguments passed through to `hew-observe`.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub args: Vec<String>,
}

// ---------------------------------------------------------------------------
// Lsp
// ---------------------------------------------------------------------------

/// Arguments forwarded verbatim to `hew-lsp`.
#[derive(Debug, Args)]
#[command(disable_help_flag = true)]
pub struct LspArgs {
    /// Arguments passed through to `hew-lsp`.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub args: Vec<String>,
}

#[cfg(test)]
mod tests {
    use clap::Parser;

    use super::{Cli, Command};
    use crate::compile::SirMode;

    #[test]
    fn sir_lower_is_selectable_for_every_compile_capable_command() {
        let compile = Cli::try_parse_from(["hew", "compile", "sample.hew", "--sir-lower"])
            .expect("compile should accept --sir-lower");
        let run = Cli::try_parse_from(["hew", "run", "sample.hew", "--sir-lower"])
            .expect("run should accept --sir-lower");
        let build = Cli::try_parse_from(["hew", "build", "sample.hew", "--sir-lower"])
            .expect("build should accept --sir-lower");
        let debug = Cli::try_parse_from(["hew", "debug", "sample.hew", "--sir-lower"])
            .expect("debug should accept --sir-lower");
        let watch = Cli::try_parse_from(["hew", "watch", "sample.hew", "--run", "--sir-lower"])
            .expect("watch should accept --sir-lower");
        let test = Cli::try_parse_from(["hew", "test", "--sir-lower"])
            .expect("test should accept --sir-lower");
        let eval = Cli::try_parse_from(["hew", "eval", "--sir-lower", "let x = 1;"])
            .expect("eval should accept --sir-lower");

        for command in [compile, run, build, debug, watch, test, eval] {
            assert_eq!(
                command_sir_mode(command.command.expect("command should be present")),
                SirMode::Lower
            );
        }
    }

    #[test]
    fn sir_mode_defaults_to_disabled_and_rejects_competing_flags() {
        let defaulted = Cli::try_parse_from(["hew", "build", "sample.hew"])
            .expect("build should parse without an SIR mode");
        assert_eq!(
            command_sir_mode(defaulted.command.expect("build command should be present")),
            SirMode::Disabled
        );

        let lowered = Cli::try_parse_from(["hew", "test", "--sir-lower"])
            .expect("test should preserve --sir-lower");
        assert_eq!(
            command_sir_mode(lowered.command.expect("test command should be present")),
            SirMode::Lower
        );

        assert!(Cli::try_parse_from(
            ["hew", "compile", "sample.hew", "--dump-sir", "--sir-lower",]
        )
        .is_err());
        assert!(Cli::try_parse_from([
            "hew",
            "compile",
            "sample.hew",
            "--dump-sir",
            "--dump-mir",
            "raw",
        ])
        .is_err());
        assert!(Cli::try_parse_from(["hew", "watch", "sample.hew", "--sir-lower"]).is_err());
    }

    /// The shadow lane is retired. A compatibility alias or a hidden flag would
    /// silently resurrect a second compile path, so every command that accepts
    /// an SIR mode must reject `--sir-shadow` outright.
    ///
    /// The paired `--sir-lower` acceptance on the same argv shape is the
    /// positive control: it proves the rejection is the flag's own, not a
    /// malformed command line that clap would refuse either way.
    #[test]
    fn retired_sir_shadow_flag_is_rejected_by_every_compile_capable_command() {
        let shapes: [&[&str]; 7] = [
            &["hew", "compile", "sample.hew"],
            &["hew", "run", "sample.hew"],
            &["hew", "build", "sample.hew"],
            &["hew", "debug", "sample.hew"],
            &["hew", "watch", "sample.hew", "--run"],
            &["hew", "test"],
            &["hew", "eval", "let x = 1;"],
        ];

        for shape in shapes {
            let mut accepted = shape.to_vec();
            accepted.insert(2, "--sir-lower");
            let parsed = Cli::try_parse_from(&accepted)
                .unwrap_or_else(|e| panic!("{shape:?} should still accept --sir-lower: {e}"));
            assert_eq!(
                command_sir_mode(parsed.command.expect("command should be present")),
                SirMode::Lower,
                "{shape:?} should still select the strict lane"
            );

            let mut rejected = shape.to_vec();
            rejected.insert(2, "--sir-shadow");
            assert!(
                Cli::try_parse_from(&rejected).is_err(),
                "{shape:?} must reject the retired --sir-shadow flag"
            );
        }
    }

    fn command_sir_mode(command: Command) -> SirMode {
        match command {
            Command::Compile(args) => args.sir.mode(),
            Command::Run(args) => args.to_compile_options().sir_mode,
            Command::Debug(args) => args.to_compile_options().sir_mode,
            Command::Build(args) => args.to_compile_options().sir_mode,
            Command::Test(args) => args.sir.mode(),
            Command::Watch(args) => args.to_compile_options().sir_mode,
            Command::Eval(args) => args.sir.mode(),
            other => panic!("expected a compile-capable command, got {other:?}"),
        }
    }
}
