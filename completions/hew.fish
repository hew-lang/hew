# Fish completion for the hew compiler
# Install: copy to ~/.config/fish/completions/hew.fish

# Disable file completions by default (we add them explicitly where needed)
complete -c hew -f

# Helper: check if a subcommand has been given
function __hew_no_subcommand
    set -l cmd (commandline -opc)
    for word in $cmd[2..]
        switch $word
            case build run check watch doc eval test wire fmt init completions version help
                return 1
        end
    end
    return 0
end

# Helper: check if current subcommand matches
function __hew_using_subcommand
    set -l cmd (commandline -opc)
    test (count $cmd) -ge 2; and test "$cmd[2]" = "$argv[1]"
end

# Helper: check wire check subcommand
function __hew_wire_check
    set -l cmd (commandline -opc)
    test (count $cmd) -ge 3; and test "$cmd[2]" = wire; and test "$cmd[3]" = check
end

# --- Top-level subcommands ---
complete -c hew -n __hew_no_subcommand -a build -d 'Compile to executable'
complete -c hew -n __hew_no_subcommand -a run -d 'Compile and run'
complete -c hew -n __hew_no_subcommand -a check -d 'Parse and typecheck only'
complete -c hew -n __hew_no_subcommand -a watch -d 'Watch for changes and re-check automatically'
complete -c hew -n __hew_no_subcommand -a doc -d 'Generate documentation'
complete -c hew -n __hew_no_subcommand -a eval -d 'Interactive REPL or evaluate expression'
complete -c hew -n __hew_no_subcommand -a test -d 'Run tests'
complete -c hew -n __hew_no_subcommand -a wire -d 'Wire schema compatibility'
complete -c hew -n __hew_no_subcommand -a fmt -d 'Format source file'
complete -c hew -n __hew_no_subcommand -a init -d 'Initialize a new project'
complete -c hew -n __hew_no_subcommand -a completions -d 'Print shell completion script'
complete -c hew -n __hew_no_subcommand -a version -d 'Print version info'
complete -c hew -n __hew_no_subcommand -a help -d 'Show help message'
# Also complete .hew files at top level (shorthand for build)
complete -c hew -n __hew_no_subcommand -F -a '(complete -C "echo *.hew")'

# --- build/check options ---
complete -c hew -n '__hew_using_subcommand build' -s o -r -d 'Output file path'
complete -c hew -n '__hew_using_subcommand build' -l Werror -d 'Treat warnings as errors'
complete -c hew -n '__hew_using_subcommand build' -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n '__hew_using_subcommand build' -l emit-mlir -d 'Emit MLIR instead of linking'
complete -c hew -n '__hew_using_subcommand build' -l emit-llvm -d 'Emit LLVM IR instead of linking'
complete -c hew -n '__hew_using_subcommand build' -l emit-obj -d 'Emit object code instead of linking'
complete -c hew -n '__hew_using_subcommand build' -l target -x -d 'Target triple'
complete -c hew -n '__hew_using_subcommand build' -l pkg-path -r -d 'Override package search directory'
complete -c hew -n '__hew_using_subcommand build' -F -a '(__fish_complete_suffix .hew)'

complete -c hew -n '__hew_using_subcommand check' -l Werror -d 'Treat warnings as errors'
complete -c hew -n '__hew_using_subcommand check' -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n '__hew_using_subcommand check' -l emit-mlir -d 'Emit MLIR instead of linking'
complete -c hew -n '__hew_using_subcommand check' -l emit-llvm -d 'Emit LLVM IR instead of linking'
complete -c hew -n '__hew_using_subcommand check' -l emit-obj -d 'Emit object code instead of linking'
complete -c hew -n '__hew_using_subcommand check' -l pkg-path -r -d 'Override package search directory'
complete -c hew -n '__hew_using_subcommand check' -F -a '(__fish_complete_suffix .hew)'

# --- run options (no --emit-* as run rejects them) ---
complete -c hew -n '__hew_using_subcommand run' -s o -r -d 'Output file path'
complete -c hew -n '__hew_using_subcommand run' -l Werror -d 'Treat warnings as errors'
complete -c hew -n '__hew_using_subcommand run' -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n '__hew_using_subcommand run' -l pkg-path -r -d 'Override package search directory'
complete -c hew -n '__hew_using_subcommand run' -F -a '(__fish_complete_suffix .hew)'

# --- doc options ---
complete -c hew -n '__hew_using_subcommand doc' -l output-dir -s o -r -d 'Output directory'
complete -c hew -n '__hew_using_subcommand doc' -l format -s f -x -a 'html markdown' -d 'Output format'
complete -c hew -n '__hew_using_subcommand doc' -l open -d 'Open docs in browser after generation'
complete -c hew -n '__hew_using_subcommand doc' -F -a '(__fish_complete_suffix .hew)'

# --- eval options ---
complete -c hew -n '__hew_using_subcommand eval' -s f -r -d 'Execute file in REPL context'

# --- test options ---
complete -c hew -n '__hew_using_subcommand test' -l filter -x -d 'Run only tests matching pattern'
complete -c hew -n '__hew_using_subcommand test' -l no-color -d 'Disable colored output'
complete -c hew -n '__hew_using_subcommand test' -l include-ignored -d 'Run ignored tests too'
complete -c hew -n '__hew_using_subcommand test' -F -a '(__fish_complete_suffix .hew)'

# --- fmt ---
complete -c hew -n '__hew_using_subcommand fmt' -F -a '(__fish_complete_suffix .hew)'

# --- watch options ---
complete -c hew -n '__hew_using_subcommand watch' -l run -d 'Build and run on successful check'
complete -c hew -n '__hew_using_subcommand watch' -l clear -d 'Clear terminal before each re-check'
complete -c hew -n '__hew_using_subcommand watch' -l debounce -x -d 'Debounce time in milliseconds'
complete -c hew -n '__hew_using_subcommand watch' -l Werror -d 'Treat warnings as errors'
complete -c hew -n '__hew_using_subcommand watch' -l no-typecheck -d 'Skip type-checking phase'
complete -c hew -n '__hew_using_subcommand watch' -l pkg-path -r -d 'Override package search directory'
complete -c hew -n '__hew_using_subcommand watch' -F -a '(__fish_complete_suffix .hew)'

# --- init ---
complete -c hew -n '__hew_using_subcommand init' -l force -d 'Overwrite existing files'

# --- wire subcommands ---
complete -c hew -n '__hew_using_subcommand wire' -a check -d 'Check wire schema compatibility'
complete -c hew -n __hew_wire_check -l against -r -d 'Baseline file to compare against'
complete -c hew -n __hew_wire_check -F -a '(__fish_complete_suffix .hew)'

# --- completions ---
complete -c hew -n '__hew_using_subcommand completions' -a 'bash zsh fish' -d 'Shell type'
