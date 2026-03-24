#compdef hew

autoload -U is-at-least

_hew() {
    typeset -A opt_args
    typeset -a _arguments_options
    local ret=1

    if is-at-least 5.2; then
        _arguments_options=(-s -S -C)
    else
        _arguments_options=(-s -C)
    fi

    local context curcontext="$curcontext" state line
    _arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
":: :_hew_commands" \
"*::: :->hew" \
&& ret=0
    case $state in
    (hew)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:hew-command-$line[1]:"
        case $line[1] in
            (build)
_arguments "${_arguments_options[@]}" : \
'-o+[Output executable path]:FILE:_files' \
'*--link-lib=[Pass an extra library or linker argument to the native link step]:PATH:_default' \
'--target=[Target triple]:TRIPLE:_default' \
'--pkg-path=[Override package search directory (default\: .adze/packages/)]:DIR:_files' \
'-g[Build with debug info (no optimization, no stripping)]' \
'--debug[Build with debug info (no optimization, no stripping)]' \
'--emit-ast[Emit enriched AST as JSON]' \
'--emit-json[Emit full codegen IR as JSON (same as msgpack, for debugging)]' \
'--emit-msgpack[Emit full codegen IR as msgpack]' \
'--emit-mlir[Emit MLIR instead of linking]' \
'--emit-llvm[Emit LLVM IR instead of linking]' \
'--emit-obj[Emit object code instead of linking]' \
'--Werror[Accepted for spec compatibility (no-op)]' \
'--no-typecheck[Skip type-checking phase]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':input -- Input .hew file:_files' \
&& ret=0
;;
(run)
_arguments "${_arguments_options[@]}" : \
'*--link-lib=[Pass an extra library or linker argument to the native link step]:PATH:_default' \
'--target=[Target triple]:TRIPLE:_default' \
'--pkg-path=[Override package search directory (default\: .adze/packages/)]:DIR:_files' \
'-g[Build with debug info (no optimization, no stripping)]' \
'--debug[Build with debug info (no optimization, no stripping)]' \
'--Werror[Accepted for spec compatibility (no-op)]' \
'--no-typecheck[Skip type-checking phase]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':input -- Input .hew file:_files' \
'*::program_args -- Arguments to pass to the compiled program (after --):_default' \
&& ret=0
;;
(debug)
_arguments "${_arguments_options[@]}" : \
'*--link-lib=[Pass an extra library or linker argument to the native link step]:PATH:_default' \
'--target=[Target triple]:TRIPLE:_default' \
'--pkg-path=[Override package search directory (default\: .adze/packages/)]:DIR:_files' \
'-g[Accepted for compatibility (debug info is always enabled)]' \
'--debug[Accepted for compatibility (debug info is always enabled)]' \
'--Werror[Accepted for spec compatibility (no-op)]' \
'--no-typecheck[Skip type-checking phase]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':input -- Input .hew file:_files' \
'*::program_args -- Arguments to pass to the debugger/program (after --):_default' \
&& ret=0
;;
(check)
_arguments "${_arguments_options[@]}" : \
'--pkg-path=[Override package search directory (default\: .adze/packages/)]:DIR:_files' \
'--Werror[Accepted for spec compatibility (no-op)]' \
'--no-typecheck[Skip type-checking phase]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':input -- Input .hew file:_files' \
&& ret=0
;;
(doc)
_arguments "${_arguments_options[@]}" : \
'-o+[Output directory]:DIR:_files' \
'--output-dir=[Output directory]:DIR:_files' \
'-f+[Output format]:FORMAT:(html markdown md)' \
'--format=[Output format]:FORMAT:(html markdown md)' \
'--open[Open docs in browser after generation]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
'*::input -- Input files or directories:_files' \
&& ret=0
;;
(eval)
_arguments "${_arguments_options[@]}" : \
'-f+[Execute file in REPL context]:FILE:_files' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
'*::expr -- Expression to evaluate (if no -f given):_default' \
&& ret=0
;;
(test)
_arguments "${_arguments_options[@]}" : \
'--filter=[Run only tests matching pattern]:FILTER:_default' \
'--format=[Output format]:FORMAT:(text junit)' \
'--timeout=[Per-test timeout in seconds]:TIMEOUT:_default' \
'--no-color[Disable coloured output]' \
'--include-ignored[Run ignored tests too]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
'*::paths -- Files or directories to test:_files' \
&& ret=0
;;
(watch)
_arguments "${_arguments_options[@]}" : \
'--debounce=[Debounce time in milliseconds]:DEBOUNCE:_default' \
'--pkg-path=[Override package search directory (default\: .adze/packages/)]:DIR:_files' \
'--run[Build and run on successful check]' \
'--clear[Clear terminal before each re-check]' \
'--Werror[Accepted for spec compatibility (no-op)]' \
'--no-typecheck[Skip type-checking phase]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':input -- File or directory to watch:_files' \
&& ret=0
;;
(wire)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
":: :_hew__wire_commands" \
"*::: :->wire" \
&& ret=0

    case $state in
    (wire)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:hew-wire-command-$line[1]:"
        case $line[1] in
            (check)
_arguments "${_arguments_options[@]}" : \
'--against=[Baseline schema to check against]:AGAINST:_files' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':input -- Input .hew file with current schema:_files' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_hew__wire__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:hew-wire-help-command-$line[1]:"
        case $line[1] in
            (check)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
        esac
    ;;
esac
;;
(machine)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
":: :_hew__machine_commands" \
"*::: :->machine" \
&& ret=0

    case $state in
    (machine)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:hew-machine-command-$line[1]:"
        case $line[1] in
            (diagram)
_arguments "${_arguments_options[@]}" : \
'--dot[Output Graphviz DOT format instead of Mermaid]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':input -- Input .hew file:_files' \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':input -- Input .hew file:_files' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_hew__machine__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:hew-machine-help-command-$line[1]:"
        case $line[1] in
            (diagram)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
        esac
    ;;
esac
;;
(fmt)
_arguments "${_arguments_options[@]}" : \
'--check[Check formatting without writing (exit 1 if unformatted)]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
'*::files -- Source files to format:_files' \
&& ret=0
;;
(init)
_arguments "${_arguments_options[@]}" : \
'--force[Overwrite existing files]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
'::name -- Project name (creates a directory; omit to init in current dir):_default' \
&& ret=0
;;
(completions)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
':shell -- Shell to generate completions for:(bash zsh fish powershell)' \
&& ret=0
;;
(version)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_hew__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:hew-help-command-$line[1]:"
        case $line[1] in
            (build)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(run)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(debug)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(check)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(doc)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(eval)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(test)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(watch)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(wire)
_arguments "${_arguments_options[@]}" : \
":: :_hew__help__wire_commands" \
"*::: :->wire" \
&& ret=0

    case $state in
    (wire)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:hew-help-wire-command-$line[1]:"
        case $line[1] in
            (check)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
(machine)
_arguments "${_arguments_options[@]}" : \
":: :_hew__help__machine_commands" \
"*::: :->machine" \
&& ret=0

    case $state in
    (machine)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:hew-help-machine-command-$line[1]:"
        case $line[1] in
            (diagram)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
(fmt)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(init)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(completions)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(version)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
        esac
    ;;
esac
}

(( $+functions[_hew_commands] )) ||
_hew_commands() {
    local commands; commands=(
'build:Compile a .hew file to an executable' \
'run:Compile and run a .hew file' \
'debug:Build with debug info and launch under gdb/lldb' \
'check:Parse and typecheck only' \
'doc:Generate documentation' \
'eval:Interactive REPL or evaluate expression' \
'test:Run tests' \
'watch:Watch for changes and re-check automatically' \
'wire:Wire schema compatibility tools' \
'machine:State machine tools' \
'fmt:Format source files in-place' \
'init:Scaffold a new project' \
'completions:Print shell completion script' \
'version:Print version info' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'hew commands' commands "$@"
}
(( $+functions[_hew__build_commands] )) ||
_hew__build_commands() {
    local commands; commands=()
    _describe -t commands 'hew build commands' commands "$@"
}
(( $+functions[_hew__check_commands] )) ||
_hew__check_commands() {
    local commands; commands=()
    _describe -t commands 'hew check commands' commands "$@"
}
(( $+functions[_hew__completions_commands] )) ||
_hew__completions_commands() {
    local commands; commands=()
    _describe -t commands 'hew completions commands' commands "$@"
}
(( $+functions[_hew__debug_commands] )) ||
_hew__debug_commands() {
    local commands; commands=()
    _describe -t commands 'hew debug commands' commands "$@"
}
(( $+functions[_hew__doc_commands] )) ||
_hew__doc_commands() {
    local commands; commands=()
    _describe -t commands 'hew doc commands' commands "$@"
}
(( $+functions[_hew__eval_commands] )) ||
_hew__eval_commands() {
    local commands; commands=()
    _describe -t commands 'hew eval commands' commands "$@"
}
(( $+functions[_hew__fmt_commands] )) ||
_hew__fmt_commands() {
    local commands; commands=()
    _describe -t commands 'hew fmt commands' commands "$@"
}
(( $+functions[_hew__help_commands] )) ||
_hew__help_commands() {
    local commands; commands=(
'build:Compile a .hew file to an executable' \
'run:Compile and run a .hew file' \
'debug:Build with debug info and launch under gdb/lldb' \
'check:Parse and typecheck only' \
'doc:Generate documentation' \
'eval:Interactive REPL or evaluate expression' \
'test:Run tests' \
'watch:Watch for changes and re-check automatically' \
'wire:Wire schema compatibility tools' \
'machine:State machine tools' \
'fmt:Format source files in-place' \
'init:Scaffold a new project' \
'completions:Print shell completion script' \
'version:Print version info' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'hew help commands' commands "$@"
}
(( $+functions[_hew__help__build_commands] )) ||
_hew__help__build_commands() {
    local commands; commands=()
    _describe -t commands 'hew help build commands' commands "$@"
}
(( $+functions[_hew__help__check_commands] )) ||
_hew__help__check_commands() {
    local commands; commands=()
    _describe -t commands 'hew help check commands' commands "$@"
}
(( $+functions[_hew__help__completions_commands] )) ||
_hew__help__completions_commands() {
    local commands; commands=()
    _describe -t commands 'hew help completions commands' commands "$@"
}
(( $+functions[_hew__help__debug_commands] )) ||
_hew__help__debug_commands() {
    local commands; commands=()
    _describe -t commands 'hew help debug commands' commands "$@"
}
(( $+functions[_hew__help__doc_commands] )) ||
_hew__help__doc_commands() {
    local commands; commands=()
    _describe -t commands 'hew help doc commands' commands "$@"
}
(( $+functions[_hew__help__eval_commands] )) ||
_hew__help__eval_commands() {
    local commands; commands=()
    _describe -t commands 'hew help eval commands' commands "$@"
}
(( $+functions[_hew__help__fmt_commands] )) ||
_hew__help__fmt_commands() {
    local commands; commands=()
    _describe -t commands 'hew help fmt commands' commands "$@"
}
(( $+functions[_hew__help__help_commands] )) ||
_hew__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'hew help help commands' commands "$@"
}
(( $+functions[_hew__help__init_commands] )) ||
_hew__help__init_commands() {
    local commands; commands=()
    _describe -t commands 'hew help init commands' commands "$@"
}
(( $+functions[_hew__help__machine_commands] )) ||
_hew__help__machine_commands() {
    local commands; commands=(
'diagram:Generate state diagram to stdout' \
'list:List all machines with states and events' \
    )
    _describe -t commands 'hew help machine commands' commands "$@"
}
(( $+functions[_hew__help__machine__diagram_commands] )) ||
_hew__help__machine__diagram_commands() {
    local commands; commands=()
    _describe -t commands 'hew help machine diagram commands' commands "$@"
}
(( $+functions[_hew__help__machine__list_commands] )) ||
_hew__help__machine__list_commands() {
    local commands; commands=()
    _describe -t commands 'hew help machine list commands' commands "$@"
}
(( $+functions[_hew__help__run_commands] )) ||
_hew__help__run_commands() {
    local commands; commands=()
    _describe -t commands 'hew help run commands' commands "$@"
}
(( $+functions[_hew__help__test_commands] )) ||
_hew__help__test_commands() {
    local commands; commands=()
    _describe -t commands 'hew help test commands' commands "$@"
}
(( $+functions[_hew__help__version_commands] )) ||
_hew__help__version_commands() {
    local commands; commands=()
    _describe -t commands 'hew help version commands' commands "$@"
}
(( $+functions[_hew__help__watch_commands] )) ||
_hew__help__watch_commands() {
    local commands; commands=()
    _describe -t commands 'hew help watch commands' commands "$@"
}
(( $+functions[_hew__help__wire_commands] )) ||
_hew__help__wire_commands() {
    local commands; commands=(
'check:Check wire schema compatibility' \
    )
    _describe -t commands 'hew help wire commands' commands "$@"
}
(( $+functions[_hew__help__wire__check_commands] )) ||
_hew__help__wire__check_commands() {
    local commands; commands=()
    _describe -t commands 'hew help wire check commands' commands "$@"
}
(( $+functions[_hew__init_commands] )) ||
_hew__init_commands() {
    local commands; commands=()
    _describe -t commands 'hew init commands' commands "$@"
}
(( $+functions[_hew__machine_commands] )) ||
_hew__machine_commands() {
    local commands; commands=(
'diagram:Generate state diagram to stdout' \
'list:List all machines with states and events' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'hew machine commands' commands "$@"
}
(( $+functions[_hew__machine__diagram_commands] )) ||
_hew__machine__diagram_commands() {
    local commands; commands=()
    _describe -t commands 'hew machine diagram commands' commands "$@"
}
(( $+functions[_hew__machine__help_commands] )) ||
_hew__machine__help_commands() {
    local commands; commands=(
'diagram:Generate state diagram to stdout' \
'list:List all machines with states and events' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'hew machine help commands' commands "$@"
}
(( $+functions[_hew__machine__help__diagram_commands] )) ||
_hew__machine__help__diagram_commands() {
    local commands; commands=()
    _describe -t commands 'hew machine help diagram commands' commands "$@"
}
(( $+functions[_hew__machine__help__help_commands] )) ||
_hew__machine__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'hew machine help help commands' commands "$@"
}
(( $+functions[_hew__machine__help__list_commands] )) ||
_hew__machine__help__list_commands() {
    local commands; commands=()
    _describe -t commands 'hew machine help list commands' commands "$@"
}
(( $+functions[_hew__machine__list_commands] )) ||
_hew__machine__list_commands() {
    local commands; commands=()
    _describe -t commands 'hew machine list commands' commands "$@"
}
(( $+functions[_hew__run_commands] )) ||
_hew__run_commands() {
    local commands; commands=()
    _describe -t commands 'hew run commands' commands "$@"
}
(( $+functions[_hew__test_commands] )) ||
_hew__test_commands() {
    local commands; commands=()
    _describe -t commands 'hew test commands' commands "$@"
}
(( $+functions[_hew__version_commands] )) ||
_hew__version_commands() {
    local commands; commands=()
    _describe -t commands 'hew version commands' commands "$@"
}
(( $+functions[_hew__watch_commands] )) ||
_hew__watch_commands() {
    local commands; commands=()
    _describe -t commands 'hew watch commands' commands "$@"
}
(( $+functions[_hew__wire_commands] )) ||
_hew__wire_commands() {
    local commands; commands=(
'check:Check wire schema compatibility' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'hew wire commands' commands "$@"
}
(( $+functions[_hew__wire__check_commands] )) ||
_hew__wire__check_commands() {
    local commands; commands=()
    _describe -t commands 'hew wire check commands' commands "$@"
}
(( $+functions[_hew__wire__help_commands] )) ||
_hew__wire__help_commands() {
    local commands; commands=(
'check:Check wire schema compatibility' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'hew wire help commands' commands "$@"
}
(( $+functions[_hew__wire__help__check_commands] )) ||
_hew__wire__help__check_commands() {
    local commands; commands=()
    _describe -t commands 'hew wire help check commands' commands "$@"
}
(( $+functions[_hew__wire__help__help_commands] )) ||
_hew__wire__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'hew wire help help commands' commands "$@"
}

if [ "$funcstack[1]" = "_hew" ]; then
    _hew "$@"
else
    compdef _hew hew
fi
