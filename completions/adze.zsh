#compdef adze
# Zsh completion for the adze package manager
# Install: place in a directory in your $fpath (e.g. /usr/local/share/zsh/site-functions/_adze)

_adze() {
    local -a commands=(
        'init:Create a new hew.toml in the current directory'
        'add:Add a dependency to hew.toml'
        'install:Install dependencies into .adze/packages/'
        'publish:Publish this package to the registry'
        'list:List packages in the local registry'
        'search:Search packages in the registry'
        'info:Show package info'
        'tree:Show dependency tree'
        'update:Update dependencies'
        'remove:Remove a dependency'
        'check:Validate manifest'
        'outdated:Show outdated dependencies'
        'login:Log in to the registry via GitHub'
        'logout:Log out from the registry'
        'key:Manage signing keys'
        'namespace:Manage namespace ownership'
        'yank:Yank a published version'
        'registry-key:Show the registry'\''s public signing key'
        'deprecate:Deprecate a package'
        'index:Manage local package index'
        'completions:Generate shell completion scripts'
    )

    _arguments -C \
        '1:command:->command' \
        '*::args:->args'

    case "$state" in
        command)
            _describe -t commands 'adze command' commands
            ;;
        args)
            case "${words[1]}" in
                init)
                    _arguments \
                        '--lib[Create a library project]' \
                        '--bin[Create a binary project]' \
                        '--actor[Create an actor project]' \
                        '1:project name:'
                    ;;
                add)
                    _arguments \
                        '--version=[Version requirement]:version:' \
                        '(-r --registry)'{-r,--registry}'=[Use a named registry]:registry:' \
                        '1:package name:'
                    ;;
                install)
                    _arguments \
                        '--locked[Require an up-to-date lock file]' \
                        '(-r --registry)'{-r,--registry}'=[Use a named registry]:registry:'
                    ;;
                publish)
                    _arguments \
                        '(-r --registry)'{-r,--registry}'=[Use a named registry]:registry:'
                    ;;
                search)
                    _arguments \
                        '--category=[Filter by category]:category:' \
                        '--page=[Page number]:page:' \
                        '--per-page=[Results per page]:count:' \
                        '(-r --registry)'{-r,--registry}'=[Use a named registry]:registry:' \
                        '1:search query:'
                    ;;
                info)
                    _arguments \
                        '(-r --registry)'{-r,--registry}'=[Use a named registry]:registry:' \
                        '1:package name:'
                    ;;
                update)
                    _arguments '1:package name:'
                    ;;
                remove)
                    _arguments '1:package name:'
                    ;;
                yank)
                    _arguments \
                        '--reason=[Reason for yanking]:reason:' \
                        '--undo[Undo a previous yank]' \
                        '1:version:'
                    ;;
                deprecate)
                    _arguments \
                        '--message=[Deprecation message]:message:' \
                        '--successor=[Suggested replacement package]:package:' \
                        '--undo[Undo deprecation]' \
                        '1:package name:'
                    ;;
                key)
                    local -a key_commands=(
                        'generate:Generate a new Ed25519 signing keypair'
                        'list:List registered signing keys'
                        'info:Look up a signing key by fingerprint'
                    )
                    _arguments -C \
                        '1:key command:->key_command' \
                        '*::key_args:->key_args'

                    case "$state" in
                        key_command)
                            _describe -t key_commands 'key subcommand' key_commands
                            ;;
                        key_args)
                            case "${words[1]}" in
                                info)
                                    _arguments '1:fingerprint:'
                                    ;;
                            esac
                            ;;
                    esac
                    ;;
                namespace)
                    local -a namespace_commands=(
                        'register:Register a custom namespace prefix'
                        'list:List namespaces you own'
                        'info:Show info about a namespace'
                    )
                    _arguments -C \
                        '1:namespace command:->ns_command' \
                        '*::ns_args:->ns_args'

                    case "$state" in
                        ns_command)
                            _describe -t namespace_commands 'namespace subcommand' namespace_commands
                            ;;
                        ns_args)
                            case "${words[1]}" in
                                register|info)
                                    _arguments '1:namespace prefix:'
                                    ;;
                            esac
                            ;;
                    esac
                    ;;
                index)
                    local -a index_commands=(
                        'sync:Sync the local package index from the registry'
                        'resolve:Resolve a package version from the local index'
                        'list:List all versions of a package in the local index'
                    )
                    _arguments -C \
                        '1:index command:->idx_command' \
                        '*::idx_args:->idx_args'

                    case "$state" in
                        idx_command)
                            _describe -t index_commands 'index subcommand' index_commands
                            ;;
                        idx_args)
                            case "${words[1]}" in
                                resolve)
                                    _arguments \
                                        '--version=[Version requirement]:version:' \
                                        '1:package name:'
                                    ;;
                                list)
                                    _arguments '1:package name:'
                                    ;;
                            esac
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

compdef _adze adze
