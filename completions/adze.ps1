
using namespace System.Management.Automation
using namespace System.Management.Automation.Language

Register-ArgumentCompleter -Native -CommandName 'adze' -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)

    $commandElements = $commandAst.CommandElements
    $command = @(
        'adze'
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
        'adze' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('init', 'init', [CompletionResultType]::ParameterValue, 'Create a new hew.toml in the current directory')
            [CompletionResult]::new('add', 'add', [CompletionResultType]::ParameterValue, 'Add a dependency to hew.toml')
            [CompletionResult]::new('install', 'install', [CompletionResultType]::ParameterValue, 'Install dependencies into .adze/packages/')
            [CompletionResult]::new('publish', 'publish', [CompletionResultType]::ParameterValue, 'Publish this package to the registry')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List packages in the local registry')
            [CompletionResult]::new('search', 'search', [CompletionResultType]::ParameterValue, 'Search packages in the registry')
            [CompletionResult]::new('info', 'info', [CompletionResultType]::ParameterValue, 'Show package info')
            [CompletionResult]::new('tree', 'tree', [CompletionResultType]::ParameterValue, 'Show dependency tree')
            [CompletionResult]::new('update', 'update', [CompletionResultType]::ParameterValue, 'Update dependencies')
            [CompletionResult]::new('remove', 'remove', [CompletionResultType]::ParameterValue, 'Remove a dependency')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Validate manifest')
            [CompletionResult]::new('outdated', 'outdated', [CompletionResultType]::ParameterValue, 'Show outdated dependencies')
            [CompletionResult]::new('login', 'login', [CompletionResultType]::ParameterValue, 'Log in to the registry via GitHub')
            [CompletionResult]::new('logout', 'logout', [CompletionResultType]::ParameterValue, 'Log out from the registry')
            [CompletionResult]::new('key', 'key', [CompletionResultType]::ParameterValue, 'Manage signing keys')
            [CompletionResult]::new('namespace', 'namespace', [CompletionResultType]::ParameterValue, 'Manage namespace ownership')
            [CompletionResult]::new('yank', 'yank', [CompletionResultType]::ParameterValue, 'Yank a published version')
            [CompletionResult]::new('registry-key', 'registry-key', [CompletionResultType]::ParameterValue, 'Show the registry''s public signing key')
            [CompletionResult]::new('deprecate', 'deprecate', [CompletionResultType]::ParameterValue, 'Deprecate a package')
            [CompletionResult]::new('index', 'index', [CompletionResultType]::ParameterValue, 'Manage local package index')
            [CompletionResult]::new('completions', 'completions', [CompletionResultType]::ParameterValue, 'Generate shell completion scripts')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'adze;init' {
            [CompletionResult]::new('--lib', '--lib', [CompletionResultType]::ParameterName, 'Create a library project')
            [CompletionResult]::new('--actor', '--actor', [CompletionResultType]::ParameterName, 'Create an actor project')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;add' {
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Version requirement')
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('--registry', '--registry', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;install' {
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('--registry', '--registry', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('--locked', '--locked', [CompletionResultType]::ParameterName, 'Require an up-to-date lock file')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;publish' {
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('--registry', '--registry', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;list' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;search' {
            [CompletionResult]::new('--category', '--category', [CompletionResultType]::ParameterName, 'Filter by category')
            [CompletionResult]::new('--page', '--page', [CompletionResultType]::ParameterName, 'Page number (1-based)')
            [CompletionResult]::new('--per-page', '--per-page', [CompletionResultType]::ParameterName, 'Results per page')
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('--registry', '--registry', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;info' {
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('--registry', '--registry', [CompletionResultType]::ParameterName, 'Use a named registry from config')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;tree' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;update' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;remove' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;check' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;outdated' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;login' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;logout' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;key' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('generate', 'generate', [CompletionResultType]::ParameterValue, 'Generate a new Ed25519 signing keypair')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List registered signing keys')
            [CompletionResult]::new('info', 'info', [CompletionResultType]::ParameterValue, 'Look up a signing key by fingerprint')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'adze;key;generate' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;key;list' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;key;info' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;key;help' {
            [CompletionResult]::new('generate', 'generate', [CompletionResultType]::ParameterValue, 'Generate a new Ed25519 signing keypair')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List registered signing keys')
            [CompletionResult]::new('info', 'info', [CompletionResultType]::ParameterValue, 'Look up a signing key by fingerprint')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'adze;key;help;generate' {
            break
        }
        'adze;key;help;list' {
            break
        }
        'adze;key;help;info' {
            break
        }
        'adze;key;help;help' {
            break
        }
        'adze;namespace' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('register', 'register', [CompletionResultType]::ParameterValue, 'Register a custom namespace prefix')
            [CompletionResult]::new('info', 'info', [CompletionResultType]::ParameterValue, 'Show info about a namespace')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'adze;namespace;register' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;namespace;info' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;namespace;help' {
            [CompletionResult]::new('register', 'register', [CompletionResultType]::ParameterValue, 'Register a custom namespace prefix')
            [CompletionResult]::new('info', 'info', [CompletionResultType]::ParameterValue, 'Show info about a namespace')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'adze;namespace;help;register' {
            break
        }
        'adze;namespace;help;info' {
            break
        }
        'adze;namespace;help;help' {
            break
        }
        'adze;yank' {
            [CompletionResult]::new('--reason', '--reason', [CompletionResultType]::ParameterName, 'Reason for yanking')
            [CompletionResult]::new('--undo', '--undo', [CompletionResultType]::ParameterName, 'Undo a previous yank')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;registry-key' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;deprecate' {
            [CompletionResult]::new('--message', '--message', [CompletionResultType]::ParameterName, 'Deprecation message')
            [CompletionResult]::new('--successor', '--successor', [CompletionResultType]::ParameterName, 'Suggested replacement package')
            [CompletionResult]::new('--undo', '--undo', [CompletionResultType]::ParameterName, 'Undo deprecation')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;index' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('sync', 'sync', [CompletionResultType]::ParameterValue, 'Sync the local package index from the registry')
            [CompletionResult]::new('resolve', 'resolve', [CompletionResultType]::ParameterValue, 'Resolve a package version from the local index')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List all versions of a package in the local index')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'adze;index;sync' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;index;resolve' {
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Version requirement')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;index;list' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;index;help' {
            [CompletionResult]::new('sync', 'sync', [CompletionResultType]::ParameterValue, 'Sync the local package index from the registry')
            [CompletionResult]::new('resolve', 'resolve', [CompletionResultType]::ParameterValue, 'Resolve a package version from the local index')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List all versions of a package in the local index')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'adze;index;help;sync' {
            break
        }
        'adze;index;help;resolve' {
            break
        }
        'adze;index;help;list' {
            break
        }
        'adze;index;help;help' {
            break
        }
        'adze;completions' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'adze;help' {
            [CompletionResult]::new('init', 'init', [CompletionResultType]::ParameterValue, 'Create a new hew.toml in the current directory')
            [CompletionResult]::new('add', 'add', [CompletionResultType]::ParameterValue, 'Add a dependency to hew.toml')
            [CompletionResult]::new('install', 'install', [CompletionResultType]::ParameterValue, 'Install dependencies into .adze/packages/')
            [CompletionResult]::new('publish', 'publish', [CompletionResultType]::ParameterValue, 'Publish this package to the registry')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List packages in the local registry')
            [CompletionResult]::new('search', 'search', [CompletionResultType]::ParameterValue, 'Search packages in the registry')
            [CompletionResult]::new('info', 'info', [CompletionResultType]::ParameterValue, 'Show package info')
            [CompletionResult]::new('tree', 'tree', [CompletionResultType]::ParameterValue, 'Show dependency tree')
            [CompletionResult]::new('update', 'update', [CompletionResultType]::ParameterValue, 'Update dependencies')
            [CompletionResult]::new('remove', 'remove', [CompletionResultType]::ParameterValue, 'Remove a dependency')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Validate manifest')
            [CompletionResult]::new('outdated', 'outdated', [CompletionResultType]::ParameterValue, 'Show outdated dependencies')
            [CompletionResult]::new('login', 'login', [CompletionResultType]::ParameterValue, 'Log in to the registry via GitHub')
            [CompletionResult]::new('logout', 'logout', [CompletionResultType]::ParameterValue, 'Log out from the registry')
            [CompletionResult]::new('key', 'key', [CompletionResultType]::ParameterValue, 'Manage signing keys')
            [CompletionResult]::new('namespace', 'namespace', [CompletionResultType]::ParameterValue, 'Manage namespace ownership')
            [CompletionResult]::new('yank', 'yank', [CompletionResultType]::ParameterValue, 'Yank a published version')
            [CompletionResult]::new('registry-key', 'registry-key', [CompletionResultType]::ParameterValue, 'Show the registry''s public signing key')
            [CompletionResult]::new('deprecate', 'deprecate', [CompletionResultType]::ParameterValue, 'Deprecate a package')
            [CompletionResult]::new('index', 'index', [CompletionResultType]::ParameterValue, 'Manage local package index')
            [CompletionResult]::new('completions', 'completions', [CompletionResultType]::ParameterValue, 'Generate shell completion scripts')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'adze;help;init' {
            break
        }
        'adze;help;add' {
            break
        }
        'adze;help;install' {
            break
        }
        'adze;help;publish' {
            break
        }
        'adze;help;list' {
            break
        }
        'adze;help;search' {
            break
        }
        'adze;help;info' {
            break
        }
        'adze;help;tree' {
            break
        }
        'adze;help;update' {
            break
        }
        'adze;help;remove' {
            break
        }
        'adze;help;check' {
            break
        }
        'adze;help;outdated' {
            break
        }
        'adze;help;login' {
            break
        }
        'adze;help;logout' {
            break
        }
        'adze;help;key' {
            [CompletionResult]::new('generate', 'generate', [CompletionResultType]::ParameterValue, 'Generate a new Ed25519 signing keypair')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List registered signing keys')
            [CompletionResult]::new('info', 'info', [CompletionResultType]::ParameterValue, 'Look up a signing key by fingerprint')
            break
        }
        'adze;help;key;generate' {
            break
        }
        'adze;help;key;list' {
            break
        }
        'adze;help;key;info' {
            break
        }
        'adze;help;namespace' {
            [CompletionResult]::new('register', 'register', [CompletionResultType]::ParameterValue, 'Register a custom namespace prefix')
            [CompletionResult]::new('info', 'info', [CompletionResultType]::ParameterValue, 'Show info about a namespace')
            break
        }
        'adze;help;namespace;register' {
            break
        }
        'adze;help;namespace;info' {
            break
        }
        'adze;help;yank' {
            break
        }
        'adze;help;registry-key' {
            break
        }
        'adze;help;deprecate' {
            break
        }
        'adze;help;index' {
            [CompletionResult]::new('sync', 'sync', [CompletionResultType]::ParameterValue, 'Sync the local package index from the registry')
            [CompletionResult]::new('resolve', 'resolve', [CompletionResultType]::ParameterValue, 'Resolve a package version from the local index')
            [CompletionResult]::new('list', 'list', [CompletionResultType]::ParameterValue, 'List all versions of a package in the local index')
            break
        }
        'adze;help;index;sync' {
            break
        }
        'adze;help;index;resolve' {
            break
        }
        'adze;help;index;list' {
            break
        }
        'adze;help;completions' {
            break
        }
        'adze;help;help' {
            break
        }
    })

    $completions.Where{ $_.CompletionText -like "$wordToComplete*" } |
        Sort-Object -Property ListItemText
}
