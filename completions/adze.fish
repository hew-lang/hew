# Fish completion for the adze package manager
# Install: copy to ~/.config/fish/completions/adze.fish

# Disable file completions by default (we add them explicitly where needed)
complete -c adze -f

# Helper: check if a subcommand has been given
function __adze_no_subcommand
    set -l cmd (commandline -opc)
    for word in $cmd[2..]
        switch $word
            case init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions
                return 1
        end
    end
    return 0
end

# Helper: check if current subcommand matches
function __adze_using_subcommand
    set -l cmd (commandline -opc)
    test (count $cmd) -ge 2; and test "$cmd[2]" = "$argv[1]"
end

# Helper: check for nested subcommand (e.g. key generate, index sync)
function __adze_using_nested
    set -l cmd (commandline -opc)
    test (count $cmd) -ge 3; and test "$cmd[2]" = "$argv[1]"; and test "$cmd[3]" = "$argv[2]"
end

# --- Top-level subcommands ---
complete -c adze -n __adze_no_subcommand -a init -d 'Create a new hew.toml in the current directory'
complete -c adze -n __adze_no_subcommand -a add -d 'Add a dependency to hew.toml'
complete -c adze -n __adze_no_subcommand -a install -d 'Install dependencies into .adze/packages/'
complete -c adze -n __adze_no_subcommand -a publish -d 'Publish this package to the registry'
complete -c adze -n __adze_no_subcommand -a list -d 'List packages in the local registry'
complete -c adze -n __adze_no_subcommand -a search -d 'Search packages in the registry'
complete -c adze -n __adze_no_subcommand -a info -d 'Show package info'
complete -c adze -n __adze_no_subcommand -a tree -d 'Show dependency tree'
complete -c adze -n __adze_no_subcommand -a update -d 'Update dependencies'
complete -c adze -n __adze_no_subcommand -a remove -d 'Remove a dependency'
complete -c adze -n __adze_no_subcommand -a check -d 'Validate manifest'
complete -c adze -n __adze_no_subcommand -a outdated -d 'Show outdated dependencies'
complete -c adze -n __adze_no_subcommand -a login -d 'Log in to the registry via GitHub'
complete -c adze -n __adze_no_subcommand -a logout -d 'Log out from the registry'
complete -c adze -n __adze_no_subcommand -a key -d 'Manage signing keys'
complete -c adze -n __adze_no_subcommand -a namespace -d 'Manage namespace ownership'
complete -c adze -n __adze_no_subcommand -a yank -d 'Yank a published version'
complete -c adze -n __adze_no_subcommand -a registry-key -d 'Show the registry\'s public signing key'
complete -c adze -n __adze_no_subcommand -a deprecate -d 'Deprecate a package'
complete -c adze -n __adze_no_subcommand -a index -d 'Manage local package index'
complete -c adze -n __adze_no_subcommand -a completions -d 'Generate shell completion scripts'

# --- init options ---
complete -c adze -n '__adze_using_subcommand init' -l lib -d 'Create a library project'
complete -c adze -n '__adze_using_subcommand init' -l bin -d 'Create a binary project'
complete -c adze -n '__adze_using_subcommand init' -l actor -d 'Create an actor project'

# --- add options ---
complete -c adze -n '__adze_using_subcommand add' -l version -x -d 'Version requirement'
complete -c adze -n '__adze_using_subcommand add' -s r -l registry -x -d 'Use a named registry'

# --- install options ---
complete -c adze -n '__adze_using_subcommand install' -l locked -d 'Require an up-to-date lock file'
complete -c adze -n '__adze_using_subcommand install' -s r -l registry -x -d 'Use a named registry'

# --- publish options ---
complete -c adze -n '__adze_using_subcommand publish' -s r -l registry -x -d 'Use a named registry'

# --- search options ---
complete -c adze -n '__adze_using_subcommand search' -l category -x -d 'Filter by category'
complete -c adze -n '__adze_using_subcommand search' -l page -x -d 'Page number'
complete -c adze -n '__adze_using_subcommand search' -l per-page -x -d 'Results per page'
complete -c adze -n '__adze_using_subcommand search' -s r -l registry -x -d 'Use a named registry'

# --- info options ---
complete -c adze -n '__adze_using_subcommand info' -s r -l registry -x -d 'Use a named registry'

# --- yank options ---
complete -c adze -n '__adze_using_subcommand yank' -l reason -x -d 'Reason for yanking'
complete -c adze -n '__adze_using_subcommand yank' -l undo -d 'Undo a previous yank'

# --- deprecate options ---
complete -c adze -n '__adze_using_subcommand deprecate' -l message -x -d 'Deprecation message'
complete -c adze -n '__adze_using_subcommand deprecate' -l successor -x -d 'Suggested replacement package'
complete -c adze -n '__adze_using_subcommand deprecate' -l undo -d 'Undo deprecation'

# --- key subcommands ---
complete -c adze -n '__adze_using_subcommand key' -a generate -d 'Generate a new Ed25519 signing keypair'
complete -c adze -n '__adze_using_subcommand key' -a list -d 'List registered signing keys'
complete -c adze -n '__adze_using_subcommand key' -a info -d 'Look up a signing key by fingerprint'

# --- namespace subcommands ---
complete -c adze -n '__adze_using_subcommand namespace' -a register -d 'Register a custom namespace prefix'
complete -c adze -n '__adze_using_subcommand namespace' -a list -d 'List namespaces you own'
complete -c adze -n '__adze_using_subcommand namespace' -a info -d 'Show info about a namespace'

# --- index subcommands ---
complete -c adze -n '__adze_using_subcommand index' -a sync -d 'Sync the local package index from the registry'
complete -c adze -n '__adze_using_subcommand index' -a resolve -d 'Resolve a package version from the local index'
complete -c adze -n '__adze_using_subcommand index' -a list -d 'List all versions of a package in the local index'

# --- index resolve options ---
complete -c adze -n '__adze_using_nested index resolve' -l version -x -d 'Version requirement'

# --- completions ---
complete -c adze -n '__adze_using_subcommand completions' -a 'bash zsh fish' -d 'Shell type'
