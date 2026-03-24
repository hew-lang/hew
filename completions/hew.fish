# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_hew_global_optspecs
	string join \n h/help V/version
end

function __fish_hew_needs_command
	# Figure out if the current invocation already has a command.
	set -l cmd (commandline -opc)
	set -e cmd[1]
	argparse -s (__fish_hew_global_optspecs) -- $cmd 2>/dev/null
	or return
	if set -q argv[1]
		# Also print the command, so this can be used to figure out what it is.
		echo $argv[1]
		return 1
	end
	return 0
end

function __fish_hew_using_subcommand
	set -l cmd (__fish_hew_needs_command)
	test -z "$cmd"
	and return 1
	contains -- $cmd[1] $argv
end

complete -c hew -n "__fish_hew_needs_command" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_needs_command" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_needs_command" -f -a "build" -d 'Compile a .hew file to an executable'
complete -c hew -n "__fish_hew_needs_command" -f -a "run" -d 'Compile and run a .hew file'
complete -c hew -n "__fish_hew_needs_command" -f -a "debug" -d 'Build with debug info and launch under gdb/lldb'
complete -c hew -n "__fish_hew_needs_command" -f -a "check" -d 'Parse and typecheck only'
complete -c hew -n "__fish_hew_needs_command" -f -a "doc" -d 'Generate documentation'
complete -c hew -n "__fish_hew_needs_command" -f -a "eval" -d 'Interactive REPL or evaluate expression'
complete -c hew -n "__fish_hew_needs_command" -f -a "test" -d 'Run tests'
complete -c hew -n "__fish_hew_needs_command" -f -a "watch" -d 'Watch for changes and re-check automatically'
complete -c hew -n "__fish_hew_needs_command" -f -a "wire" -d 'Wire schema compatibility tools'
complete -c hew -n "__fish_hew_needs_command" -f -a "machine" -d 'State machine tools'
complete -c hew -n "__fish_hew_needs_command" -f -a "fmt" -d 'Format source files in-place'
complete -c hew -n "__fish_hew_needs_command" -f -a "init" -d 'Scaffold a new project'
complete -c hew -n "__fish_hew_needs_command" -f -a "completions" -d 'Print shell completion script'
complete -c hew -n "__fish_hew_needs_command" -f -a "version" -d 'Print version info'
complete -c hew -n "__fish_hew_needs_command" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c hew -n "__fish_hew_using_subcommand build" -s o -d 'Output executable path' -r -F
complete -c hew -n "__fish_hew_using_subcommand build" -l link-lib -d 'Pass an extra library or linker argument to the native link step' -r
complete -c hew -n "__fish_hew_using_subcommand build" -l target -d 'Target triple' -r
complete -c hew -n "__fish_hew_using_subcommand build" -l pkg-path -d 'Override package search directory (default: .adze/packages/)' -r -F
complete -c hew -n "__fish_hew_using_subcommand build" -s g -l debug -d 'Build with debug info (no optimization, no stripping)'
complete -c hew -n "__fish_hew_using_subcommand build" -l emit-ast -d 'Emit enriched AST as JSON'
complete -c hew -n "__fish_hew_using_subcommand build" -l emit-json -d 'Emit full codegen IR as JSON (same as msgpack, for debugging)'
complete -c hew -n "__fish_hew_using_subcommand build" -l emit-msgpack -d 'Emit full codegen IR as msgpack'
complete -c hew -n "__fish_hew_using_subcommand build" -l emit-mlir -d 'Emit MLIR instead of linking'
complete -c hew -n "__fish_hew_using_subcommand build" -l emit-llvm -d 'Emit LLVM IR instead of linking'
complete -c hew -n "__fish_hew_using_subcommand build" -l emit-obj -d 'Emit object code instead of linking'
complete -c hew -n "__fish_hew_using_subcommand build" -l Werror -d 'Accepted for spec compatibility (no-op)'
complete -c hew -n "__fish_hew_using_subcommand build" -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n "__fish_hew_using_subcommand build" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand build" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand run" -l link-lib -d 'Pass an extra library or linker argument to the native link step' -r
complete -c hew -n "__fish_hew_using_subcommand run" -l target -d 'Target triple' -r
complete -c hew -n "__fish_hew_using_subcommand run" -l pkg-path -d 'Override package search directory (default: .adze/packages/)' -r -F
complete -c hew -n "__fish_hew_using_subcommand run" -s g -l debug -d 'Build with debug info (no optimization, no stripping)'
complete -c hew -n "__fish_hew_using_subcommand run" -l Werror -d 'Accepted for spec compatibility (no-op)'
complete -c hew -n "__fish_hew_using_subcommand run" -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n "__fish_hew_using_subcommand run" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand run" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand debug" -l link-lib -d 'Pass an extra library or linker argument to the native link step' -r
complete -c hew -n "__fish_hew_using_subcommand debug" -l target -d 'Target triple' -r
complete -c hew -n "__fish_hew_using_subcommand debug" -l pkg-path -d 'Override package search directory (default: .adze/packages/)' -r -F
complete -c hew -n "__fish_hew_using_subcommand debug" -s g -l debug -d 'Accepted for compatibility (debug info is always enabled)'
complete -c hew -n "__fish_hew_using_subcommand debug" -l Werror -d 'Accepted for spec compatibility (no-op)'
complete -c hew -n "__fish_hew_using_subcommand debug" -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n "__fish_hew_using_subcommand debug" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand debug" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand check" -l pkg-path -d 'Override package search directory (default: .adze/packages/)' -r -F
complete -c hew -n "__fish_hew_using_subcommand check" -l Werror -d 'Accepted for spec compatibility (no-op)'
complete -c hew -n "__fish_hew_using_subcommand check" -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n "__fish_hew_using_subcommand check" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand check" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand doc" -s o -l output-dir -d 'Output directory' -r -F
complete -c hew -n "__fish_hew_using_subcommand doc" -s f -l format -d 'Output format' -r -f -a "html\t''
markdown\t''
md\t''"
complete -c hew -n "__fish_hew_using_subcommand doc" -l open -d 'Open docs in browser after generation'
complete -c hew -n "__fish_hew_using_subcommand doc" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand doc" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand eval" -s f -d 'Execute file in REPL context' -r -F
complete -c hew -n "__fish_hew_using_subcommand eval" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand eval" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand test" -l filter -d 'Run only tests matching pattern' -r
complete -c hew -n "__fish_hew_using_subcommand test" -l format -d 'Output format' -r -f -a "text\t''
junit\t''"
complete -c hew -n "__fish_hew_using_subcommand test" -l timeout -d 'Per-test timeout in seconds' -r
complete -c hew -n "__fish_hew_using_subcommand test" -l no-color -d 'Disable coloured output'
complete -c hew -n "__fish_hew_using_subcommand test" -l include-ignored -d 'Run ignored tests too'
complete -c hew -n "__fish_hew_using_subcommand test" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand test" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand watch" -l debounce -d 'Debounce time in milliseconds' -r
complete -c hew -n "__fish_hew_using_subcommand watch" -l pkg-path -d 'Override package search directory (default: .adze/packages/)' -r -F
complete -c hew -n "__fish_hew_using_subcommand watch" -l run -d 'Build and run on successful check'
complete -c hew -n "__fish_hew_using_subcommand watch" -l clear -d 'Clear terminal before each re-check'
complete -c hew -n "__fish_hew_using_subcommand watch" -l Werror -d 'Accepted for spec compatibility (no-op)'
complete -c hew -n "__fish_hew_using_subcommand watch" -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n "__fish_hew_using_subcommand watch" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand watch" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand wire; and not __fish_seen_subcommand_from check help" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand wire; and not __fish_seen_subcommand_from check help" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand wire; and not __fish_seen_subcommand_from check help" -f -a "check" -d 'Check wire schema compatibility'
complete -c hew -n "__fish_hew_using_subcommand wire; and not __fish_seen_subcommand_from check help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c hew -n "__fish_hew_using_subcommand wire; and __fish_seen_subcommand_from check" -l against -d 'Baseline schema to check against' -r -F
complete -c hew -n "__fish_hew_using_subcommand wire; and __fish_seen_subcommand_from check" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand wire; and __fish_seen_subcommand_from check" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand wire; and __fish_seen_subcommand_from help" -f -a "check" -d 'Check wire schema compatibility'
complete -c hew -n "__fish_hew_using_subcommand wire; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c hew -n "__fish_hew_using_subcommand machine; and not __fish_seen_subcommand_from diagram list help" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand machine; and not __fish_seen_subcommand_from diagram list help" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand machine; and not __fish_seen_subcommand_from diagram list help" -f -a "diagram" -d 'Generate state diagram to stdout'
complete -c hew -n "__fish_hew_using_subcommand machine; and not __fish_seen_subcommand_from diagram list help" -f -a "list" -d 'List all machines with states and events'
complete -c hew -n "__fish_hew_using_subcommand machine; and not __fish_seen_subcommand_from diagram list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c hew -n "__fish_hew_using_subcommand machine; and __fish_seen_subcommand_from diagram" -l dot -d 'Output Graphviz DOT format instead of Mermaid'
complete -c hew -n "__fish_hew_using_subcommand machine; and __fish_seen_subcommand_from diagram" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand machine; and __fish_seen_subcommand_from diagram" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand machine; and __fish_seen_subcommand_from list" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand machine; and __fish_seen_subcommand_from list" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand machine; and __fish_seen_subcommand_from help" -f -a "diagram" -d 'Generate state diagram to stdout'
complete -c hew -n "__fish_hew_using_subcommand machine; and __fish_seen_subcommand_from help" -f -a "list" -d 'List all machines with states and events'
complete -c hew -n "__fish_hew_using_subcommand machine; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c hew -n "__fish_hew_using_subcommand fmt" -l check -d 'Check formatting without writing (exit 1 if unformatted)'
complete -c hew -n "__fish_hew_using_subcommand fmt" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand fmt" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand init" -l force -d 'Overwrite existing files'
complete -c hew -n "__fish_hew_using_subcommand init" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand init" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand completions" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand completions" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand version" -s h -l help -d 'Print help'
complete -c hew -n "__fish_hew_using_subcommand version" -s V -l version -d 'Print version'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "build" -d 'Compile a .hew file to an executable'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "run" -d 'Compile and run a .hew file'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "debug" -d 'Build with debug info and launch under gdb/lldb'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "check" -d 'Parse and typecheck only'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "doc" -d 'Generate documentation'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "eval" -d 'Interactive REPL or evaluate expression'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "test" -d 'Run tests'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "watch" -d 'Watch for changes and re-check automatically'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "wire" -d 'Wire schema compatibility tools'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "machine" -d 'State machine tools'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "fmt" -d 'Format source files in-place'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "init" -d 'Scaffold a new project'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "completions" -d 'Print shell completion script'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "version" -d 'Print version info'
complete -c hew -n "__fish_hew_using_subcommand help; and not __fish_seen_subcommand_from build run debug check doc eval test watch wire machine fmt init completions version help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c hew -n "__fish_hew_using_subcommand help; and __fish_seen_subcommand_from wire" -f -a "check" -d 'Check wire schema compatibility'
complete -c hew -n "__fish_hew_using_subcommand help; and __fish_seen_subcommand_from machine" -f -a "diagram" -d 'Generate state diagram to stdout'
complete -c hew -n "__fish_hew_using_subcommand help; and __fish_seen_subcommand_from machine" -f -a "list" -d 'List all machines with states and events'
