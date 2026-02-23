#compdef hew
# Zsh completion for the hew compiler
# Install: place in a directory in your $fpath (e.g. /usr/local/share/zsh/site-functions/_hew)

_hew_files() {
    _files -g '*.hew'
}

_hew_build_opts() {
    local -a opts=(
        '-o[Output file path]:output file:_files'
        '--Werror[Treat warnings as errors]'
        '--no-typecheck[Skip type-checking phase]'
        '--emit-mlir[Emit MLIR instead of linking]'
        '--emit-llvm[Emit LLVM IR instead of linking]'
        '--emit-obj[Emit object code instead of linking]'
        '--target=[Target triple]:target:'
        '--pkg-path[Override package search directory]:package dir:_directories'
    )
    _arguments -S : $opts '*:source file:_hew_files'
}

_hew() {
    local -a commands=(
        'build:Compile to executable'
        'run:Compile and run'
        'check:Parse and typecheck only'
        'watch:Watch for changes and re-check automatically'
        'doc:Generate documentation'
        'eval:Interactive REPL or evaluate expression'
        'test:Run tests'
        'wire:Wire schema compatibility'
        'fmt:Format source file'
        'init:Initialize a new project'
        'completions:Print shell completion script'
        'version:Print version info'
        'help:Show help message'
    )

    _arguments -C \
        '1:command:->command' \
        '*::args:->args'

    case "$state" in
        command)
            _describe -t commands 'hew command' commands
            _hew_files
            ;;
        args)
            case "${words[1]}" in
                build|check)
                    _hew_build_opts
                    ;;
                run)
                    local -a run_opts=(
                        '-o[Output file path]:output file:_files'
                        '--Werror[Treat warnings as errors]'
                        '--no-typecheck[Skip type-checking phase]'
                        '--pkg-path[Override package search directory]:package dir:_directories'
                    )
                    _arguments -S : $run_opts '*:source file:_hew_files'
                    ;;
                doc)
                    local -a doc_opts=(
                        '--output-dir[Output directory]:output dir:_directories'
                        '-o[Output directory]:output dir:_directories'
                        '--format[Output format]:format:(html markdown)'
                        '-f[Output format]:format:(html markdown)'
                        '--open[Open docs in browser after generation]'
                    )
                    _arguments -S : $doc_opts '*:source file or directory:_files'
                    ;;
                eval)
                    _arguments \
                        '-f[Execute file in REPL context]:source file:_hew_files' \
                        '*:expression:'
                    ;;
                test)
                    local -a test_opts=(
                        '--filter[Run only tests matching pattern]:pattern:'
                        '--no-color[Disable colored output]'
                        '--include-ignored[Run ignored tests too]'
                    )
                    _arguments -S : $test_opts '*:file or directory:_files'
                    ;;
                fmt)
                    _arguments '*:source file:_hew_files'
                    ;;
                watch)
                    local -a watch_opts=(
                        '--run[Build and run on successful check]'
                        '--clear[Clear terminal before each re-check]'
                        '--debounce[Debounce time in milliseconds]:milliseconds:'
                        '--Werror[Treat warnings as errors]'
                        '--no-typecheck[Skip type-checking phase]'
                        '--pkg-path[Override package search directory]:package dir:_directories'
                    )
                    _arguments -S : $watch_opts '*:source file or directory:_files'
                    ;;
                init)
                    _arguments \
                        '--force[Overwrite existing files]' \
                        '1:project name:'
                    ;;
                wire)
                    local -a wire_commands=(
                        'check:Check wire schema compatibility'
                    )
                    _arguments -C \
                        '1:wire command:->wire_command' \
                        '*::wire_args:->wire_args'

                    case "$state" in
                        wire_command)
                            _describe -t wire_commands 'wire subcommand' wire_commands
                            ;;
                        wire_args)
                            _arguments \
                                '--against[Baseline file to compare against]:baseline file:_hew_files' \
                                '*:source file:_hew_files'
                            ;;
                    esac
                    ;;
                completions)
                    _arguments '1:shell:(bash zsh fish)'
                    ;;
            esac
            ;;
    esac
}

compdef _hew hew
