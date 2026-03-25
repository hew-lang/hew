# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_adze_global_optspecs
	string join \n h/help V/version
end

function __fish_adze_needs_command
	# Figure out if the current invocation already has a command.
	set -l cmd (commandline -opc)
	set -e cmd[1]
	argparse -s (__fish_adze_global_optspecs) -- $cmd 2>/dev/null
	or return
	if set -q argv[1]
		# Also print the command, so this can be used to figure out what it is.
		echo $argv[1]
		return 1
	end
	return 0
end

function __fish_adze_using_subcommand
	set -l cmd (__fish_adze_needs_command)
	test -z "$cmd"
	and return 1
	contains -- $cmd[1] $argv
end

complete -c adze -n "__fish_adze_needs_command" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_needs_command" -s V -l version -d 'Print version'
complete -c adze -n "__fish_adze_needs_command" -f -a "init" -d 'Create a new hew.toml in the current directory'
complete -c adze -n "__fish_adze_needs_command" -f -a "add" -d 'Add a dependency to hew.toml'
complete -c adze -n "__fish_adze_needs_command" -f -a "install" -d 'Install dependencies into .adze/packages/'
complete -c adze -n "__fish_adze_needs_command" -f -a "publish" -d 'Publish this package to the registry'
complete -c adze -n "__fish_adze_needs_command" -f -a "list" -d 'List packages in the local registry'
complete -c adze -n "__fish_adze_needs_command" -f -a "search" -d 'Search packages in the registry'
complete -c adze -n "__fish_adze_needs_command" -f -a "info" -d 'Show package info'
complete -c adze -n "__fish_adze_needs_command" -f -a "tree" -d 'Show dependency tree'
complete -c adze -n "__fish_adze_needs_command" -f -a "update" -d 'Update dependencies'
complete -c adze -n "__fish_adze_needs_command" -f -a "remove" -d 'Remove a dependency'
complete -c adze -n "__fish_adze_needs_command" -f -a "check" -d 'Validate manifest'
complete -c adze -n "__fish_adze_needs_command" -f -a "outdated" -d 'Show outdated dependencies'
complete -c adze -n "__fish_adze_needs_command" -f -a "login" -d 'Log in to the registry via GitHub'
complete -c adze -n "__fish_adze_needs_command" -f -a "logout" -d 'Log out from the registry'
complete -c adze -n "__fish_adze_needs_command" -f -a "key" -d 'Manage signing keys'
complete -c adze -n "__fish_adze_needs_command" -f -a "namespace" -d 'Manage namespace ownership'
complete -c adze -n "__fish_adze_needs_command" -f -a "yank" -d 'Yank a published version'
complete -c adze -n "__fish_adze_needs_command" -f -a "registry-key" -d 'Show the registry\'s public signing key'
complete -c adze -n "__fish_adze_needs_command" -f -a "deprecate" -d 'Deprecate a package'
complete -c adze -n "__fish_adze_needs_command" -f -a "index" -d 'Manage local package index'
complete -c adze -n "__fish_adze_needs_command" -f -a "completions" -d 'Generate shell completion scripts'
complete -c adze -n "__fish_adze_needs_command" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c adze -n "__fish_adze_using_subcommand init" -l lib -d 'Create a library project'
complete -c adze -n "__fish_adze_using_subcommand init" -l actor -d 'Create an actor project'
complete -c adze -n "__fish_adze_using_subcommand init" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand add" -l version -d 'Version requirement' -r
complete -c adze -n "__fish_adze_using_subcommand add" -s r -l registry -d 'Use a named registry from config' -r
complete -c adze -n "__fish_adze_using_subcommand add" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand install" -s r -l registry -d 'Use a named registry from config' -r
complete -c adze -n "__fish_adze_using_subcommand install" -l locked -d 'Require an up-to-date lock file'
complete -c adze -n "__fish_adze_using_subcommand install" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand publish" -s r -l registry -d 'Use a named registry from config' -r
complete -c adze -n "__fish_adze_using_subcommand publish" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand list" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand search" -l category -d 'Filter by category' -r
complete -c adze -n "__fish_adze_using_subcommand search" -l page -d 'Page number (1-based)' -r
complete -c adze -n "__fish_adze_using_subcommand search" -l per-page -d 'Results per page' -r
complete -c adze -n "__fish_adze_using_subcommand search" -s r -l registry -d 'Use a named registry from config' -r
complete -c adze -n "__fish_adze_using_subcommand search" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand info" -s r -l registry -d 'Use a named registry from config' -r
complete -c adze -n "__fish_adze_using_subcommand info" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand tree" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand update" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand remove" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand check" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand outdated" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand login" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand logout" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand key; and not __fish_seen_subcommand_from generate list info help" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand key; and not __fish_seen_subcommand_from generate list info help" -f -a "generate" -d 'Generate a new Ed25519 signing keypair'
complete -c adze -n "__fish_adze_using_subcommand key; and not __fish_seen_subcommand_from generate list info help" -f -a "list" -d 'List registered signing keys'
complete -c adze -n "__fish_adze_using_subcommand key; and not __fish_seen_subcommand_from generate list info help" -f -a "info" -d 'Look up a signing key by fingerprint'
complete -c adze -n "__fish_adze_using_subcommand key; and not __fish_seen_subcommand_from generate list info help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c adze -n "__fish_adze_using_subcommand key; and __fish_seen_subcommand_from generate" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand key; and __fish_seen_subcommand_from list" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand key; and __fish_seen_subcommand_from info" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand key; and __fish_seen_subcommand_from help" -f -a "generate" -d 'Generate a new Ed25519 signing keypair'
complete -c adze -n "__fish_adze_using_subcommand key; and __fish_seen_subcommand_from help" -f -a "list" -d 'List registered signing keys'
complete -c adze -n "__fish_adze_using_subcommand key; and __fish_seen_subcommand_from help" -f -a "info" -d 'Look up a signing key by fingerprint'
complete -c adze -n "__fish_adze_using_subcommand key; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c adze -n "__fish_adze_using_subcommand namespace; and not __fish_seen_subcommand_from register info help" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand namespace; and not __fish_seen_subcommand_from register info help" -f -a "register" -d 'Register a custom namespace prefix'
complete -c adze -n "__fish_adze_using_subcommand namespace; and not __fish_seen_subcommand_from register info help" -f -a "info" -d 'Show info about a namespace'
complete -c adze -n "__fish_adze_using_subcommand namespace; and not __fish_seen_subcommand_from register info help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c adze -n "__fish_adze_using_subcommand namespace; and __fish_seen_subcommand_from register" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand namespace; and __fish_seen_subcommand_from info" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand namespace; and __fish_seen_subcommand_from help" -f -a "register" -d 'Register a custom namespace prefix'
complete -c adze -n "__fish_adze_using_subcommand namespace; and __fish_seen_subcommand_from help" -f -a "info" -d 'Show info about a namespace'
complete -c adze -n "__fish_adze_using_subcommand namespace; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c adze -n "__fish_adze_using_subcommand yank" -l reason -d 'Reason for yanking' -r
complete -c adze -n "__fish_adze_using_subcommand yank" -l undo -d 'Undo a previous yank'
complete -c adze -n "__fish_adze_using_subcommand yank" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand registry-key" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand deprecate" -l message -d 'Deprecation message' -r
complete -c adze -n "__fish_adze_using_subcommand deprecate" -l successor -d 'Suggested replacement package' -r
complete -c adze -n "__fish_adze_using_subcommand deprecate" -l undo -d 'Undo deprecation'
complete -c adze -n "__fish_adze_using_subcommand deprecate" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand index; and not __fish_seen_subcommand_from sync resolve list help" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand index; and not __fish_seen_subcommand_from sync resolve list help" -f -a "sync" -d 'Sync the local package index from the registry'
complete -c adze -n "__fish_adze_using_subcommand index; and not __fish_seen_subcommand_from sync resolve list help" -f -a "resolve" -d 'Resolve a package version from the local index'
complete -c adze -n "__fish_adze_using_subcommand index; and not __fish_seen_subcommand_from sync resolve list help" -f -a "list" -d 'List all versions of a package in the local index'
complete -c adze -n "__fish_adze_using_subcommand index; and not __fish_seen_subcommand_from sync resolve list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c adze -n "__fish_adze_using_subcommand index; and __fish_seen_subcommand_from sync" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand index; and __fish_seen_subcommand_from resolve" -l version -d 'Version requirement' -r
complete -c adze -n "__fish_adze_using_subcommand index; and __fish_seen_subcommand_from resolve" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand index; and __fish_seen_subcommand_from list" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand index; and __fish_seen_subcommand_from help" -f -a "sync" -d 'Sync the local package index from the registry'
complete -c adze -n "__fish_adze_using_subcommand index; and __fish_seen_subcommand_from help" -f -a "resolve" -d 'Resolve a package version from the local index'
complete -c adze -n "__fish_adze_using_subcommand index; and __fish_seen_subcommand_from help" -f -a "list" -d 'List all versions of a package in the local index'
complete -c adze -n "__fish_adze_using_subcommand index; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c adze -n "__fish_adze_using_subcommand completions" -s h -l help -d 'Print help'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "init" -d 'Create a new hew.toml in the current directory'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "add" -d 'Add a dependency to hew.toml'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "install" -d 'Install dependencies into .adze/packages/'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "publish" -d 'Publish this package to the registry'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "list" -d 'List packages in the local registry'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "search" -d 'Search packages in the registry'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "info" -d 'Show package info'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "tree" -d 'Show dependency tree'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "update" -d 'Update dependencies'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "remove" -d 'Remove a dependency'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "check" -d 'Validate manifest'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "outdated" -d 'Show outdated dependencies'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "login" -d 'Log in to the registry via GitHub'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "logout" -d 'Log out from the registry'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "key" -d 'Manage signing keys'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "namespace" -d 'Manage namespace ownership'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "yank" -d 'Yank a published version'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "registry-key" -d 'Show the registry\'s public signing key'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "deprecate" -d 'Deprecate a package'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "index" -d 'Manage local package index'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "completions" -d 'Generate shell completion scripts'
complete -c adze -n "__fish_adze_using_subcommand help; and not __fish_seen_subcommand_from init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c adze -n "__fish_adze_using_subcommand help; and __fish_seen_subcommand_from key" -f -a "generate" -d 'Generate a new Ed25519 signing keypair'
complete -c adze -n "__fish_adze_using_subcommand help; and __fish_seen_subcommand_from key" -f -a "list" -d 'List registered signing keys'
complete -c adze -n "__fish_adze_using_subcommand help; and __fish_seen_subcommand_from key" -f -a "info" -d 'Look up a signing key by fingerprint'
complete -c adze -n "__fish_adze_using_subcommand help; and __fish_seen_subcommand_from namespace" -f -a "register" -d 'Register a custom namespace prefix'
complete -c adze -n "__fish_adze_using_subcommand help; and __fish_seen_subcommand_from namespace" -f -a "info" -d 'Show info about a namespace'
complete -c adze -n "__fish_adze_using_subcommand help; and __fish_seen_subcommand_from index" -f -a "sync" -d 'Sync the local package index from the registry'
complete -c adze -n "__fish_adze_using_subcommand help; and __fish_seen_subcommand_from index" -f -a "resolve" -d 'Resolve a package version from the local index'
complete -c adze -n "__fish_adze_using_subcommand help; and __fish_seen_subcommand_from index" -f -a "list" -d 'List all versions of a package in the local index'
