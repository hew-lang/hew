
using namespace System.Management.Automation
using namespace System.Management.Automation.Language

Register-ArgumentCompleter -Native -CommandName 'hew' -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)

    $commandElements = $commandAst.CommandElements
    $command = @(
        'hew'
        for ($i = 1; $i -lt $commandElements.Count; $i++) {
            $element = $commandElements[$i]
            if ($element -isnot [StringConstantExpressionAst] -or
                $element.StringConstantType -ne [StringConstantType]::BareWord -or
                $element.Value.StartsWith('-') -or
                $element.Value -eq $wordToComplete) {
                break
        }
        $element.Value
    }) -join ';'

    $completions = @(switch ($command) {
        'hew' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'Compile a .hew file to an executable')
            [CompletionResult]::new('run', 'run', [CompletionResultType]::ParameterValue, 'Compile and run a .hew file')
            [CompletionResult]::new('debug', 'debug', [CompletionResultType]::ParameterValue, 'Build with debug info and launch under gdb/lldb')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Parse and typecheck only')
            [CompletionResult]::new('doc', 'doc', [CompletionResultType]::ParameterValue, 'Generate documentation')
            [CompletionResult]::new('eval', 'eval', [CompletionResultType]::ParameterValue, 'Interactive REPL or evaluate expression')
            [CompletionResult]::new('test', 'test', [CompletionResultType]::ParameterValue, 'Run tests')
            [CompletionResult]::new('watch', 'watch', [CompletionResultType]::ParameterValue, 'Watch for changes and re-check automatically')
            [CompletionResult]::new('wire', 'wire', [CompletionResultType]::ParameterValue, 'Wire schema compatibility tools')
            [CompletionResult]::new('machine', 'machine', [CompletionResultType]::ParameterValue, 'State machine tools')
            [CompletionResult]::new('fmt', 'fmt', [CompletionResultType]::ParameterValue, 'Format source files in-place')
            [CompletionResult]::new('init', 'init', [CompletionResultType]::ParameterValue, 'Scaffold a new project')
            [CompletionResult]::new('completions', 'completions', [CompletionResultType]::ParameterValue, 'Print shell completion script')
            [CompletionResult]::new('version', 'version', [CompletionResultType]::ParameterValue, 'Print version info')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'hew;build' {
            [CompletionResult]::new('-o', '-o', [CompletionResultType]::ParameterName, 'Output executable path')
            [CompletionResult]::new('--link-lib', '--link-lib', [CompletionResultType]::ParameterName, 'Pass an extra library or linker argument to the native link step')
            [CompletionResult]::new('--target', '--target', [CompletionResultType]::ParameterName, 'Target triple')
            [CompletionResult]::new('--pkg-path', '--pkg-path', [CompletionResultType]::ParameterName, 'Override package search directory (default: .adze/packages/)')
            [CompletionResult]::new('-g', '-g', [CompletionResultType]::ParameterName, 'Build with debug info (no optimization, no stripping)')
            [CompletionResult]::new('--debug', '--debug', [CompletionResultType]::ParameterName, 'Build with debug info (no optimization, no stripping)')
            [CompletionResult]::new('--emit-ast', '--emit-ast', [CompletionResultType]::ParameterName, 'Emit enriched AST as JSON')
            [CompletionResult]::new('--emit-json', '--emit-json', [CompletionResultType]::ParameterName, 'Emit full codegen IR as JSON (same as msgpack, for debugging)')
            [CompletionResult]::new('--emit-msgpack', '--emit-msgpack', [CompletionResultType]::ParameterName, 'Emit full codegen IR as msgpack')
            [CompletionResult]::new('--emit-mlir', '--emit-mlir', [CompletionResultType]::ParameterName, 'Emit MLIR instead of linking')
            [CompletionResult]::new('--emit-llvm', '--emit-llvm', [CompletionResultType]::ParameterName, 'Emit LLVM IR instead of linking')
            [CompletionResult]::new('--emit-obj', '--emit-obj', [CompletionResultType]::ParameterName, 'Emit object code instead of linking')
            [CompletionResult]::new('--Werror', '--Werror', [CompletionResultType]::ParameterName, 'Accepted for spec compatibility (no-op)')
            [CompletionResult]::new('--no-typecheck', '--no-typecheck', [CompletionResultType]::ParameterName, 'Skip type-checking phase')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;run' {
            [CompletionResult]::new('--link-lib', '--link-lib', [CompletionResultType]::ParameterName, 'Pass an extra library or linker argument to the native link step')
            [CompletionResult]::new('--target', '--target', [CompletionResultType]::ParameterName, 'Target triple')
            [CompletionResult]::new('--pkg-path', '--pkg-path', [CompletionResultType]::ParameterName, 'Override package search directory (default: .adze/packages/)')
            [CompletionResult]::new('-g', '-g', [CompletionResultType]::ParameterName, 'Build with debug info (no optimization, no stripping)')
            [CompletionResult]::new('--debug', '--debug', [CompletionResultType]::ParameterName, 'Build with debug info (no optimization, no stripping)')
            [CompletionResult]::new('--Werror', '--Werror', [CompletionResultType]::ParameterName, 'Accepted for spec compatibility (no-op)')
            [CompletionResult]::new('--no-typecheck', '--no-typecheck', [CompletionResultType]::ParameterName, 'Skip type-checking phase')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;debug' {
            [CompletionResult]::new('--link-lib', '--link-lib', [CompletionResultType]::ParameterName, 'Pass an extra library or linker argument to the native link step')
            [CompletionResult]::new('--target', '--target', [CompletionResultType]::ParameterName, 'Target triple')
            [CompletionResult]::new('--pkg-path', '--pkg-path', [CompletionResultType]::ParameterName, 'Override package search directory (default: .adze/packages/)')
            [CompletionResult]::new('-g', '-g', [CompletionResultType]::ParameterName, 'Accepted for compatibility (debug info is always enabled)')
            [CompletionResult]::new('--debug', '--debug', [CompletionResultType]::ParameterName, 'Accepted for compatibility (debug info is always enabled)')
            [CompletionResult]::new('--Werror', '--Werror', [CompletionResultType]::ParameterName, 'Accepted for spec compatibility (no-op)')
            [CompletionResult]::new('--no-typecheck', '--no-typecheck', [CompletionResultType]::ParameterName, 'Skip type-checking phase')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;check' {
            [CompletionResult]::new('--pkg-path', '--pkg-path', [CompletionResultType]::ParameterName, 'Override package search directory (default: .adze/packages/)')
            [CompletionResult]::new('--Werror', '--Werror', [CompletionResultType]::ParameterName, 'Accepted for spec compatibility (no-op)')
            [CompletionResult]::new('--no-typecheck', '--no-typecheck', [CompletionResultType]::ParameterName, 'Skip type-checking phase')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;doc' {
            [CompletionResult]::new('-o', '-o', [CompletionResultType]::ParameterName, 'Output directory')
            [CompletionResult]::new('--output-dir', '--output-dir', [CompletionResultType]::ParameterName, 'Output directory')
            [CompletionResult]::new('-f', '-f', [CompletionResultType]::ParameterName, 'Output format')
            [CompletionResult]::new('--format', '--format', [CompletionResultType]::ParameterName, 'Output format')
            [CompletionResult]::new('--open', '--open', [CompletionResultType]::ParameterName, 'Open docs in browser after generation')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;eval' {
            [CompletionResult]::new('-f', '-f', [CompletionResultType]::ParameterName, 'Execute file in REPL context')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;test' {
            [CompletionResult]::new('--filter', '--filter', [CompletionResultType]::ParameterName, 'Run only tests matching pattern')
            [CompletionResult]::new('--format', '--format', [CompletionResultType]::ParameterName, 'Output format')
            [CompletionResult]::new('--timeout', '--timeout', [CompletionResultType]::ParameterName, 'Per-test timeout in seconds')
            [CompletionResult]::new('--no-color', '--no-color', [CompletionResultType]::ParameterName, 'Disable coloured output')
            [CompletionResult]::new('--include-ignored', '--include-ignored', [CompletionResultType]::ParameterName, 'Run ignored tests too')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;watch' {
            [CompletionResult]::new('--debounce', '--debounce', [CompletionResultType]::ParameterName, 'Debounce time in milliseconds')
            [CompletionResult]::new('--pkg-path', '--pkg-path', [CompletionResultType]::ParameterName, 'Override package search directory (default: .adze/packages/)')
            [CompletionResult]::new('--run', '--run', [CompletionResultType]::ParameterName, 'Build and run on successful check')
            [CompletionResult]::new('--clear', '--clear', [CompletionResultType]::ParameterName, 'Clear terminal before each re-check')
            [CompletionResult]::new('--Werror', '--Werror', [CompletionResultType]::ParameterName, 'Accepted for spec compatibility (no-op)')
            [CompletionResult]::new('--no-typecheck', '--no-typecheck', [CompletionResultType]::ParameterName, 'Skip type-checking phase')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;wire' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check wire schema compatibility')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'hew;wire;check' {
            [CompletionResult]::new('--against', '--against', [CompletionResultType]::ParameterName, 'Baseline schema to check against')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;wire;help' {
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check wire schema compatibility')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'hew;wire;help;check' {
            break
        }
        'hew;wire;help;help' {
            break
        }
        'hew;machine' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('diagram', 'diagram', [CompletionResultType]::ParameterValue, 'Generate state diagram to stdout')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List all machines with states and events')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'hew;machine;diagram' {
            [CompletionResult]::new('--dot', '--dot', [CompletionResultType]::ParameterName, 'Output Graphviz DOT format instead of Mermaid')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;machine;list' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;machine;help' {
            [CompletionResult]::new('diagram', 'diagram', [CompletionResultType]::ParameterValue, 'Generate state diagram to stdout')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List all machines with states and events')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'hew;machine;help;diagram' {
            break
        }
        'hew;machine;help;list' {
            break
        }
        'hew;machine;help;help' {
            break
        }
        'hew;fmt' {
            [CompletionResult]::new('--check', '--check', [CompletionResultType]::ParameterName, 'Check formatting without writing (exit 1 if unformatted)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;init' {
            [CompletionResult]::new('--force', '--force', [CompletionResultType]::ParameterName, 'Overwrite existing files')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;completions' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;version' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            break
        }
        'hew;help' {
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'Compile a .hew file to an executable')
            [CompletionResult]::new('run', 'run', [CompletionResultType]::ParameterValue, 'Compile and run a .hew file')
            [CompletionResult]::new('debug', 'debug', [CompletionResultType]::ParameterValue, 'Build with debug info and launch under gdb/lldb')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Parse and typecheck only')
            [CompletionResult]::new('doc', 'doc', [CompletionResultType]::ParameterValue, 'Generate documentation')
            [CompletionResult]::new('eval', 'eval', [CompletionResultType]::ParameterValue, 'Interactive REPL or evaluate expression')
            [CompletionResult]::new('test', 'test', [CompletionResultType]::ParameterValue, 'Run tests')
            [CompletionResult]::new('watch', 'watch', [CompletionResultType]::ParameterValue, 'Watch for changes and re-check automatically')
            [CompletionResult]::new('wire', 'wire', [CompletionResultType]::ParameterValue, 'Wire schema compatibility tools')
            [CompletionResult]::new('machine', 'machine', [CompletionResultType]::ParameterValue, 'State machine tools')
            [CompletionResult]::new('fmt', 'fmt', [CompletionResultType]::ParameterValue, 'Format source files in-place')
            [CompletionResult]::new('init', 'init', [CompletionResultType]::ParameterValue, 'Scaffold a new project')
            [CompletionResult]::new('completions', 'completions', [CompletionResultType]::ParameterValue, 'Print shell completion script')
            [CompletionResult]::new('version', 'version', [CompletionResultType]::ParameterValue, 'Print version info')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'hew;help;build' {
            break
        }
        'hew;help;run' {
            break
        }
        'hew;help;debug' {
            break
        }
        'hew;help;check' {
            break
        }
        'hew;help;doc' {
            break
        }
        'hew;help;eval' {
            break
        }
        'hew;help;test' {
            break
        }
        'hew;help;watch' {
            break
        }
        'hew;help;wire' {
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check wire schema compatibility')
            break
        }
        'hew;help;wire;check' {
            break
        }
        'hew;help;machine' {
            [CompletionResult]::new('diagram', 'diagram', [CompletionResultType]::ParameterValue, 'Generate state diagram to stdout')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List all machines with states and events')
            break
        }
        'hew;help;machine;diagram' {
            break
        }
        'hew;help;machine;list' {
            break
        }
        'hew;help;fmt' {
            break
        }
        'hew;help;init' {
            break
        }
        'hew;help;completions' {
            break
        }
        'hew;help;version' {
            break
        }
        'hew;help;help' {
            break
        }
    })

    $completions.Where{ $_.CompletionText -like "$wordToComplete*" } |
        Sort-Object -Property ListItemText
}
