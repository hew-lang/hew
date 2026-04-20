use std::ffi::OsString;
use std::path::Path;

use clap::Parser;

use crate::args::{self, Cli, Command};

pub(crate) trait CommandDispatcher {
    fn build(&mut self, args: &args::BuildArgs);
    fn run(&mut self, args: &args::RunArgs);
    fn debug(&mut self, args: &args::DebugArgs);
    fn check(&mut self, args: &args::CheckArgs);
    fn doc(&mut self, args: &args::DocArgs);
    fn eval(&mut self, args: &args::EvalArgs);
    fn test(&mut self, args: &args::TestArgs);
    fn watch(&mut self, args: &args::WatchArgs);
    fn wire(&mut self, args: &args::WireCommand);
    fn machine(&mut self, args: &args::MachineCommand);
    fn fmt(&mut self, args: &args::FmtArgs);
    fn init(&mut self, args: &args::InitArgs);
    fn playground(&mut self, args: &args::PlaygroundCommand);
    fn completions(&mut self, args: &args::CompletionsArgs);
    fn version(&mut self);
    fn help(&mut self);
}

pub(crate) struct MainCommandDispatcher;

impl CommandDispatcher for MainCommandDispatcher {
    fn build(&mut self, args: &args::BuildArgs) {
        crate::cmd_build(args);
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

    fn doc(&mut self, args: &args::DocArgs) {
        crate::doc::cmd_doc(args);
    }

    fn eval(&mut self, args: &args::EvalArgs) {
        crate::eval::cmd_eval(args);
    }

    fn test(&mut self, args: &args::TestArgs) {
        crate::test_runner::cmd_test(args);
    }

    fn watch(&mut self, args: &args::WatchArgs) {
        crate::watch::cmd_watch(args);
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

    fn playground(&mut self, args: &args::PlaygroundCommand) {
        crate::playground::cmd_playground(args);
    }

    fn completions(&mut self, args: &args::CompletionsArgs) {
        crate::cmd_completions(args);
    }

    fn version(&mut self) {
        crate::cmd_version();
    }

    fn help(&mut self) {
        // No subcommand — shouldn't normally happen since clap shows help,
        // but handle gracefully.
        let _ = <Cli as clap::CommandFactory>::command().print_help();
        eprintln!();
        std::process::exit(1);
    }
}

pub(crate) fn parse_cli_or_exit() -> Cli {
    try_parse_cli_with_build_fallback(std::env::args_os()).unwrap_or_else(|error| error.exit())
}

pub(crate) fn dispatch_command(command: Option<&Command>, dispatcher: &mut impl CommandDispatcher) {
    match command {
        Some(Command::Build(args)) => dispatcher.build(args),
        Some(Command::Run(args)) => dispatcher.run(args),
        Some(Command::Debug(args)) => dispatcher.debug(args),
        Some(Command::Check(args)) => dispatcher.check(args),
        Some(Command::Doc(args)) => dispatcher.doc(args),
        Some(Command::Eval(args)) => dispatcher.eval(args),
        Some(Command::Test(args)) => dispatcher.test(args),
        Some(Command::Watch(args)) => dispatcher.watch(args),
        Some(Command::Wire(args)) => dispatcher.wire(args),
        Some(Command::Machine(args)) => dispatcher.machine(args),
        Some(Command::Fmt(args)) => dispatcher.fmt(args),
        Some(Command::Init(args)) => dispatcher.init(args),
        Some(Command::Playground(args)) => dispatcher.playground(args),
        Some(Command::Completions(args)) => dispatcher.completions(args),
        Some(Command::Version) => dispatcher.version(),
        None => dispatcher.help(),
    }
}

fn try_parse_cli_with_build_fallback<I>(args: I) -> Result<Cli, clap::Error>
where
    I: IntoIterator<Item = OsString>,
{
    let raw_args: Vec<OsString> = args.into_iter().collect();
    match Cli::try_parse_from(raw_args.clone()) {
        Ok(cli) => Ok(cli),
        Err(error) => match build_fallback_args(&raw_args) {
            Some(fallback_args) => Cli::try_parse_from(fallback_args),
            None => Err(error),
        },
    }
}

fn build_fallback_args(raw_args: &[OsString]) -> Option<Vec<OsString>> {
    let input = raw_args.get(1)?.to_str()?;
    if !looks_like_hew_source_path(input) {
        return None;
    }

    let mut fallback_args = Vec::with_capacity(raw_args.len() + 1);
    fallback_args.push(raw_args[0].clone());
    fallback_args.push(OsString::from("build"));
    fallback_args.extend(raw_args[1..].iter().cloned());
    Some(fallback_args)
}

fn looks_like_hew_source_path(arg: &str) -> bool {
    Path::new(arg)
        .extension()
        .is_some_and(|ext| ext.eq_ignore_ascii_case("hew"))
}

#[cfg(test)]
mod tests {
    use std::ffi::OsString;
    use std::path::PathBuf;

    #[cfg(unix)]
    use std::os::unix::ffi::OsStringExt;

    use crate::args::{
        BuildArgs, CommonBuildArgs, CompletionsArgs, ShellChoice, WireCheckArgs, WireCommand,
        WireSubcommand,
    };

    use super::{
        build_fallback_args, dispatch_command, try_parse_cli_with_build_fallback, CommandDispatcher,
    };

    #[test]
    fn parse_cli_inserts_build_for_top_level_hew_file() {
        let cli = try_parse_cli_with_build_fallback(
            ["hew", "sample.hew", "-o", "sample"]
                .into_iter()
                .map(OsString::from),
        )
        .expect("fallback should parse as build");

        match cli.command {
            Some(crate::args::Command::Build(args)) => {
                assert_eq!(args.input, PathBuf::from("sample.hew"));
                assert_eq!(args.output, Some(PathBuf::from("sample")));
            }
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn parse_cli_keeps_explicit_subcommand_intact() {
        let cli =
            try_parse_cli_with_build_fallback(["hew", "version"].into_iter().map(OsString::from))
                .expect("explicit subcommand should parse");

        assert!(matches!(cli.command, Some(crate::args::Command::Version)));
    }

    #[test]
    fn parse_cli_returns_original_error_when_no_build_fallback_applies() {
        assert!(try_parse_cli_with_build_fallback(
            ["hew", "missing-subcommand"]
                .into_iter()
                .map(OsString::from)
        )
        .is_err());
    }

    #[test]
    fn build_fallback_args_only_rewrites_top_level_hew_paths() {
        assert_eq!(
            build_fallback_args(&[
                OsString::from("hew"),
                OsString::from("sample.hew"),
                OsString::from("--emit-mlir"),
            ]),
            Some(vec![
                OsString::from("hew"),
                OsString::from("build"),
                OsString::from("sample.hew"),
                OsString::from("--emit-mlir"),
            ])
        );
        assert_eq!(
            build_fallback_args(&[OsString::from("hew"), OsString::from("version")]),
            None
        );
    }

    #[cfg(unix)]
    #[test]
    fn parse_cli_preserves_non_utf8_args_until_fallback_decision() {
        let cli = try_parse_cli_with_build_fallback([
            OsString::from("hew"),
            OsString::from("build"),
            OsString::from_vec(vec![0xFF]),
        ])
        .expect("clap should accept non-utf8 build paths");

        match cli.command {
            Some(crate::args::Command::Build(args)) => {
                assert_eq!(args.input, PathBuf::from(OsString::from_vec(vec![0xFF])));
            }
            other => panic!("expected build command, got {other:?}"),
        }
    }

    #[test]
    fn dispatch_command_routes_build_to_dispatcher() {
        let build = BuildArgs {
            input: PathBuf::from("sample.hew"),
            output: None,
            debug: false,
            link_libs: Vec::new(),
            target: None,
            emit_ast: false,
            emit_json: false,
            emit_msgpack: false,
            emit_mlir: false,
            emit_llvm: false,
            emit_obj: false,
            common: CommonBuildArgs::default(),
        };
        let command = crate::args::Command::Build(build);
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
    fn dispatch_command_routes_zero_arg_subcommands_to_dispatcher() {
        let command = crate::args::Command::Version;
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
        fn build(&mut self, args: &BuildArgs) {
            self.calls.push(format!("build:{}", args.input.display()));
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

        fn doc(&mut self, _args: &crate::args::DocArgs) {
            self.calls.push("doc".to_string());
        }

        fn eval(&mut self, _args: &crate::args::EvalArgs) {
            self.calls.push("eval".to_string());
        }

        fn test(&mut self, _args: &crate::args::TestArgs) {
            self.calls.push("test".to_string());
        }

        fn watch(&mut self, _args: &crate::args::WatchArgs) {
            self.calls.push("watch".to_string());
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

        fn playground(&mut self, _args: &crate::args::PlaygroundCommand) {
            self.calls.push("playground".to_string());
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

        fn version(&mut self) {
            self.calls.push("version".to_string());
        }

        fn help(&mut self) {
            self.calls.push("help".to_string());
        }
    }
}
