use std::ffi::OsString;
use std::path::Path;

use clap::FromArgMatches;

use crate::args::{self, Cli, Command, ToolSubcommand};

pub(crate) trait CommandDispatcher {
    fn compile(&mut self, args: &args::CompileArgs);
    fn expand(&mut self, args: &args::ExpandArgs);
    fn live(&mut self, args: &args::LiveArgs);
    fn generate(&mut self, args: &args::GenerateArgs);
    fn run(&mut self, args: &args::RunArgs);
    fn debug(&mut self, args: &args::DebugArgs);
    fn check(&mut self, args: &args::CheckArgs);
    fn build(&mut self, args: &args::BuildArgs);
    fn doc(&mut self, args: &args::DocArgs);
    fn eval(&mut self, args: &args::EvalArgs);
    fn test(&mut self, args: &args::TestArgs);
    fn bench(&mut self, args: &args::BenchArgs);
    fn watch(&mut self, args: &args::WatchArgs);
    fn node(&mut self, args: &args::NodeCommand);
    fn wire(&mut self, args: &args::WireCommand);
    fn machine(&mut self, args: &args::MachineCommand);
    fn fmt(&mut self, args: &args::FmtArgs);
    fn init(&mut self, args: &args::InitArgs);
    fn new_project(&mut self, args: &args::NewArgs);
    fn playground_verify(&mut self, args: &args::PlaygroundVerifyArgs);
    fn completions(&mut self, args: &args::CompletionsArgs);
    fn version(&mut self, args: args::VersionArgs);
    fn env(&mut self);
    fn help(&mut self);
    fn observe(&mut self, args: &args::ObserveArgs);
    fn symbolize(&mut self, args: &crate::symbolize::SymbolizeArgs);
    fn lsp(&mut self, args: &args::LspArgs);
    fn pkg(&mut self, cmd: &hew_pkg::cli::PkgCommand);
}

pub(crate) struct MainCommandDispatcher;

impl CommandDispatcher for MainCommandDispatcher {
    fn live(&mut self, args: &args::LiveArgs) {
        if let Err(error) = crate::live::run(args) {
            eprintln!("hew live: {error}");
            std::process::exit(1);
        }
    }
    fn generate(&mut self, args: &args::GenerateArgs) {
        crate::generate::cmd_generate(args);
    }

    fn expand(&mut self, args: &args::ExpandArgs) {
        crate::cmd_expand(args);
    }

    fn compile(&mut self, args: &args::CompileArgs) {
        crate::cmd_compile(args);
    }

    fn run(&mut self, args: &args::RunArgs) {
        crate::cmd_run(args);
    }

    fn debug(&mut self, args: &args::DebugArgs) {
        crate::cmd_debug(args);
    }

    fn check(&mut self, args: &args::CheckArgs) {
        crate::cmd_check(args);
    }

    fn build(&mut self, args: &args::BuildArgs) {
        crate::cmd_build(args);
    }

    fn doc(&mut self, args: &args::DocArgs) {
        crate::doc::cmd_doc(args);
    }

    fn eval(&mut self, args: &args::EvalArgs) {
        crate::eval::cmd_eval(args);
    }

    fn test(&mut self, args: &args::TestArgs) {
        crate::test_runner::cmd_test(args);
    }

    fn bench(&mut self, args: &args::BenchArgs) {
        crate::test_runner::bench::cmd_bench(args);
    }

    fn watch(&mut self, args: &args::WatchArgs) {
        crate::watch::cmd_watch(args);
    }

    fn node(&mut self, args: &args::NodeCommand) {
        crate::node::cmd_node(args);
    }

    fn wire(&mut self, args: &args::WireCommand) {
        crate::wire::cmd_wire(args);
    }

    fn machine(&mut self, args: &args::MachineCommand) {
        crate::machine::cmd_machine(args);
    }

    fn fmt(&mut self, args: &args::FmtArgs) {
        crate::cmd_fmt(args);
    }

    fn init(&mut self, args: &args::InitArgs) {
        crate::cmd_init(args);
    }

    fn new_project(&mut self, args: &args::NewArgs) {
        crate::cmd_new(args);
    }

    fn playground_verify(&mut self, args: &args::PlaygroundVerifyArgs) {
        crate::playground::cmd_playground_verify(args);
    }

    fn completions(&mut self, args: &args::CompletionsArgs) {
        crate::cmd_completions(args);
    }

    fn version(&mut self, args: args::VersionArgs) {
        crate::cmd_version(args);
    }

    fn env(&mut self) {
        crate::cmd_env();
    }

    fn help(&mut self) {
        // No subcommand — shouldn't normally happen since clap shows help,
        // but handle gracefully.
        let _ = crate::help::hew_command().print_help();
        eprintln!();
        std::process::exit(1);
    }

    fn observe(&mut self, args: &args::ObserveArgs) {
        crate::cmd_observe(args);
    }

    fn symbolize(&mut self, args: &crate::symbolize::SymbolizeArgs) {
        match crate::symbolize::run(args) {
            Ok(text) => print!("{text}"),
            Err(error) => {
                eprintln!("hew symbolize: {error}");
                std::process::exit(1);
            }
        }
    }

    fn lsp(&mut self, args: &args::LspArgs) {
        crate::cmd_lsp(args);
    }

    fn pkg(&mut self, cmd: &hew_pkg::cli::PkgCommand) {
        let requested = match cmd {
            hew_pkg::cli::PkgCommand::Native {
                action: hew_pkg::cli::NativeAction::Build { target, .. },
            } => target.as_deref(),
            _ => None,
        };
        let target = crate::target::TargetSpec::from_requested(requested).unwrap_or_else(|error| {
            eprintln!("Error: {error}");
            std::process::exit(2);
        });
        if target.is_wasm()
            && matches!(
                cmd,
                hew_pkg::cli::PkgCommand::Native {
                    action: hew_pkg::cli::NativeAction::Build { .. }
                }
            )
        {
            eprintln!("Error: native package archives require a native target");
            std::process::exit(2);
        }
        hew_pkg::cli::dispatch(
            cmd,
            target.normalized_triple(),
            crate::generate::check_package,
        );
    }
}

pub(crate) fn parse_cli_or_exit() -> Cli {
    try_parse_cli(std::env::args_os()).unwrap_or_else(|error| error.exit())
}

pub(crate) fn dispatch_command(command: Option<&Command>, dispatcher: &mut impl CommandDispatcher) {
    match command {
        Some(Command::Run(args)) => dispatcher.run(args),
        Some(Command::Debug(args)) => dispatcher.debug(args),
        Some(Command::Check(args)) => dispatcher.check(args),
        Some(Command::Build(args)) => dispatcher.build(args),
        Some(Command::Generate(args)) => dispatcher.generate(args),
        Some(Command::Doc(args)) => dispatcher.doc(args),
        Some(Command::Eval(args)) => dispatcher.eval(args),
        Some(Command::Test(args)) => dispatcher.test(args),
        Some(Command::Bench(args)) => dispatcher.bench(args),
        Some(Command::Watch(args)) => dispatcher.watch(args),
        Some(Command::Wire(args)) => dispatcher.wire(args),
        Some(Command::Node(args)) => dispatcher.node(args),
        Some(Command::Machine(args)) => dispatcher.machine(args),
        Some(Command::Fmt(args)) => dispatcher.fmt(args),
        Some(Command::Init(args)) => dispatcher.init(args),
        Some(Command::New(args)) => dispatcher.new_project(args),
        Some(Command::Tool(tool)) => match &tool.command {
            ToolSubcommand::Live(args) => dispatcher.live(args),
            ToolSubcommand::Compile(args) => dispatcher.compile(args),
            ToolSubcommand::Expand(args) => dispatcher.expand(args),
            ToolSubcommand::PlaygroundVerify(args) => dispatcher.playground_verify(args),
        },
        Some(Command::Completions(args)) => dispatcher.completions(args),
        Some(Command::Version(args)) => dispatcher.version(*args),
        Some(Command::Env) => dispatcher.env(),
        Some(Command::Observe(args)) => dispatcher.observe(args),
        Some(Command::Symbolize(args)) => dispatcher.symbolize(args),
        Some(Command::Lsp(args)) => dispatcher.lsp(args),
        Some(Command::Pkg(cmd)) => dispatcher.pkg(cmd),
        None => dispatcher.help(),
    }
}

fn try_parse_cli<I>(args: I) -> Result<Cli, clap::Error>
where
    I: IntoIterator<Item = OsString>,
{
    let mut raw_args: Vec<OsString> = args.into_iter().collect();
    // `hew --version --verbose` is `hew version --verbose`; clap answers a
    // bare `--version` itself before it would see `--verbose`.
    if raw_args.len() == 3
        && raw_args[1..].contains(&OsString::from("--verbose"))
        && (raw_args[1..].contains(&OsString::from("--version"))
            || raw_args[1..].contains(&OsString::from("-V")))
    {
        raw_args.truncate(1);
        raw_args.extend([OsString::from("version"), OsString::from("--verbose")]);
    }
    if raw_args
        .get(1)
        .and_then(|arg| Path::new(arg).extension())
        .is_some_and(|extension| extension.eq_ignore_ascii_case("hew"))
    {
        raw_args.splice(1..1, [OsString::from("tool"), OsString::from("compile")]);
    }

    let matches = crate::help::hew_command().try_get_matches_from(raw_args)?;
    Cli::from_arg_matches(&matches)
}

#[cfg(test)]
mod tests {
    use std::ffi::OsString;
    use std::path::PathBuf;

    use crate::args::{
        CompileArgs, CompletionsArgs, ShellChoice, WireCheckArgs, WireCommand, WireSubcommand,
    };

    use super::{dispatch_command, try_parse_cli, CommandDispatcher};

    #[test]
    fn parse_cli_inserts_compile_for_top_level_hew_file() {
        let cli = try_parse_cli(
            ["hew", "sample.hew", "--emit-dir", "out"]
                .into_iter()
                .map(OsString::from),
        )
        .expect("fallback should parse as compile");

        let Some(crate::args::Command::Tool(tool)) = cli.command else {
            panic!("expected compiler tool");
        };
        let crate::args::ToolSubcommand::Compile(compile) = tool.command else {
            panic!("expected compile");
        };
        assert_eq!(compile.input, PathBuf::from("sample.hew"));
        assert_eq!(compile.emit_dir, Some(PathBuf::from("out")));
    }

    #[test]
    fn parse_cli_keeps_explicit_subcommand_intact() {
        let cli = try_parse_cli(["hew", "version"].into_iter().map(OsString::from))
            .expect("explicit subcommand should parse");

        assert!(matches!(
            cli.command,
            Some(crate::args::Command::Version(_))
        ));
    }

    #[test]
    fn version_flag_parses_as_the_version_command_with_its_verbosity() {
        for (argv, verbose) in [
            (&["hew", "version"][..], false),
            (&["hew", "--version", "--verbose"][..], true),
            (&["hew", "--verbose", "-V"][..], true),
            (&["hew", "version", "--verbose"][..], true),
        ] {
            let cli = try_parse_cli(argv.iter().map(OsString::from))
                .unwrap_or_else(|error| panic!("{argv:?}: {error}"));
            match cli.command {
                Some(crate::args::Command::Version(args)) => {
                    assert_eq!(args.verbose, verbose, "{argv:?}");
                }
                other => panic!("{argv:?}: expected version, got {other:?}"),
            }
        }
        assert!(
            try_parse_cli(["hew", "--verbose"].into_iter().map(OsString::from)).is_err(),
            "--verbose alone is not a command"
        );
        // Every subcommand still answers clap's own `--version`.
        let error =
            try_parse_cli(["hew", "lsp", "--version"].into_iter().map(OsString::from)).unwrap_err();
        assert_eq!(error.kind(), clap::error::ErrorKind::DisplayVersion);
    }

    #[test]
    fn parse_cli_returns_original_error_when_no_compile_fallback_applies() {
        assert!(try_parse_cli(
            ["hew", "missing-subcommand"]
                .into_iter()
                .map(OsString::from)
        )
        .is_err());
    }

    #[test]
    fn retired_entry_points_report_an_unknown_command() {
        for command in ["compile", "playground"] {
            let error = try_parse_cli(["hew", command].map(OsString::from)).unwrap_err();
            assert_eq!(error.kind(), clap::error::ErrorKind::InvalidSubcommand);
        }
    }

    #[test]
    fn build_release_selects_o2_by_default() {
        let cli = try_parse_cli(
            ["hew", "build", "sample.hew", "--release"]
                .into_iter()
                .map(OsString::from),
        )
        .expect("release build should parse");

        match cli.command {
            Some(crate::args::Command::Build(args)) => {
                assert!(args.release);
                assert_eq!(args.opt_level, "2");
            }
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn explicit_build_opt_level_overrides_release_default() {
        let cli = try_parse_cli(
            [
                "hew",
                "build",
                "sample.hew",
                "--release",
                "--opt-level",
                "0",
            ]
            .into_iter()
            .map(OsString::from),
        )
        .expect("explicit optimization level should parse");

        match cli.command {
            Some(crate::args::Command::Build(args)) => assert_eq!(args.opt_level, "0"),
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn dispatch_command_routes_build_to_dispatcher() {
        let command = crate::args::Command::Build(crate::args::BuildArgs {
            input: Some(PathBuf::from("sample.hew")),
            output: None,
            target: None,
            emit_obj: false,
            export_c: Vec::new(),
            export_c_async: Vec::new(),
            emit_llvm: false,
            release: false,
            trim_paths: false,
            no_trim_paths: false,
            remap_path_prefix: Vec::new(),
            debug: false,
            split_debug: false,
            opt_level: "0".to_string(),
            link_libs: Vec::new(),
            linking: crate::args::LinkArgs::default(),
            emit_deps: None,
            common: crate::args::CommonBuildArgs::default(),
            format: crate::args::DiagnosticFormat::Text,
        });
        let mut dispatcher = RecordingDispatcher::default();

        dispatch_command(Some(&command), &mut dispatcher);

        assert_eq!(dispatcher.calls, vec!["build:sample.hew"]);
    }

    #[test]
    fn dispatch_command_routes_nested_subcommands_to_dispatcher() {
        let wire = WireCommand {
            command: WireSubcommand::Check(WireCheckArgs {
                input: PathBuf::from("current.hew"),
                against: PathBuf::from("baseline.hew"),
            }),
        };
        let command = crate::args::Command::Wire(wire);
        let mut dispatcher = RecordingDispatcher::default();

        dispatch_command(Some(&command), &mut dispatcher);

        assert_eq!(dispatcher.calls, vec!["wire"]);
    }

    #[test]
    fn dispatch_command_routes_the_compiler_tool() {
        let command = crate::args::Command::Tool(crate::args::ToolCommand {
            command: crate::args::ToolSubcommand::Compile(CompileArgs {
                input: PathBuf::from("sample.hew"),
                emit_dir: None,
                emit_llvm: false,
                dump_ast: false,
                dump_hir: false,
                dump_mir: None,
                dump_sir: false,
                dump_defs: false,
                target: None,
                opt_level: "0".to_string(),
                format: crate::args::DiagnosticFormat::Text,
            }),
        });
        let mut dispatcher = RecordingDispatcher::default();

        dispatch_command(Some(&command), &mut dispatcher);

        assert_eq!(dispatcher.calls, vec!["compile:sample.hew"]);
    }

    #[test]
    fn dispatch_command_routes_zero_arg_subcommands_to_dispatcher() {
        let command = crate::args::Command::Version(crate::args::VersionArgs::default());
        let mut dispatcher = RecordingDispatcher::default();

        dispatch_command(Some(&command), &mut dispatcher);

        assert_eq!(dispatcher.calls, vec!["version"]);
    }

    #[test]
    fn dispatch_command_routes_missing_command_to_help() {
        let mut dispatcher = RecordingDispatcher::default();

        dispatch_command(None, &mut dispatcher);

        assert_eq!(dispatcher.calls, vec!["help"]);
    }

    #[derive(Default)]
    struct RecordingDispatcher {
        calls: Vec<String>,
    }

    impl CommandDispatcher for RecordingDispatcher {
        fn live(&mut self, _args: &crate::args::LiveArgs) {
            self.calls.push("live".to_string());
        }
        fn generate(&mut self, _args: &crate::args::GenerateArgs) {
            self.calls.push("generate".to_string());
        }

        fn compile(&mut self, args: &CompileArgs) {
            self.calls.push(format!("compile:{}", args.input.display()));
        }

        fn run(&mut self, _args: &crate::args::RunArgs) {
            self.calls.push("run".to_string());
        }

        fn debug(&mut self, _args: &crate::args::DebugArgs) {
            self.calls.push("debug".to_string());
        }

        fn check(&mut self, _args: &crate::args::CheckArgs) {
            self.calls.push("check".to_string());
        }

        fn build(&mut self, args: &crate::args::BuildArgs) {
            let input = args
                .input
                .as_deref()
                .map_or_else(|| "<none>".to_string(), |p| p.display().to_string());
            self.calls.push(format!("build:{input}"));
        }

        fn doc(&mut self, _args: &crate::args::DocArgs) {
            self.calls.push("doc".to_string());
        }

        fn eval(&mut self, _args: &crate::args::EvalArgs) {
            self.calls.push("eval".to_string());
        }

        fn test(&mut self, _args: &crate::args::TestArgs) {
            self.calls.push("test".to_string());
        }

        fn bench(&mut self, _args: &crate::args::BenchArgs) {
            self.calls.push("bench".to_string());
        }

        fn watch(&mut self, _args: &crate::args::WatchArgs) {
            self.calls.push("watch".to_string());
        }

        fn node(&mut self, _args: &crate::args::NodeCommand) {
            self.calls.push("node".to_string());
        }

        fn wire(&mut self, _args: &WireCommand) {
            self.calls.push("wire".to_string());
        }

        fn machine(&mut self, _args: &crate::args::MachineCommand) {
            self.calls.push("machine".to_string());
        }

        fn fmt(&mut self, _args: &crate::args::FmtArgs) {
            self.calls.push("fmt".to_string());
        }

        fn init(&mut self, _args: &crate::args::InitArgs) {
            self.calls.push("init".to_string());
        }

        fn new_project(&mut self, _args: &crate::args::NewArgs) {
            self.calls.push("new".to_string());
        }

        fn expand(&mut self, _args: &crate::args::ExpandArgs) {
            self.calls.push("expand".to_string());
        }

        fn playground_verify(&mut self, _args: &crate::args::PlaygroundVerifyArgs) {
            self.calls.push("playground-verify".to_string());
        }

        fn completions(&mut self, args: &CompletionsArgs) {
            self.calls.push(
                match args.shell {
                    ShellChoice::Bash => "completions:bash",
                    ShellChoice::Zsh => "completions:zsh",
                    ShellChoice::Fish => "completions:fish",
                    ShellChoice::PowerShell => "completions:powershell",
                }
                .to_string(),
            );
        }

        fn version(&mut self, _args: crate::args::VersionArgs) {
            self.calls.push("version".to_string());
        }

        fn env(&mut self) {
            self.calls.push("env".to_string());
        }

        fn help(&mut self) {
            self.calls.push("help".to_string());
        }

        fn observe(&mut self, _args: &crate::args::ObserveArgs) {
            self.calls.push("observe".to_string());
        }

        fn symbolize(&mut self, _args: &crate::symbolize::SymbolizeArgs) {
            self.calls.push("symbolize".to_string());
        }

        fn lsp(&mut self, _args: &crate::args::LspArgs) {
            self.calls.push("lsp".to_string());
        }

        fn pkg(&mut self, _cmd: &hew_pkg::cli::PkgCommand) {
            self.calls.push("pkg".to_string());
        }
    }
}
