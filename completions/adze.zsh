#compdef adze

autoload -U is-at-least

_adze() {
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
":: :_adze_commands" \
"*::: :->adze" \
&& ret=0
    case $state in
    (adze)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-command-$line[1]:"
        case $line[1] in
            (init)
_arguments "${_arguments_options[@]}" : \
'--lib[Create a library project]' \
'--bin[Create a binary project]' \
'--actor[Create an actor project]' \
'-h[Print help]' \
'--help[Print help]' \
'::name -- Project name (defaults to directory name):_default' \
&& ret=0
;;
(add)
_arguments "${_arguments_options[@]}" : \
'--version=[Version requirement]:VERSION:_default' \
'-r+[Use a named registry from config]:REGISTRY:_default' \
'--registry=[Use a named registry from config]:REGISTRY:_default' \
'-h[Print help]' \
'--help[Print help]' \
':package -- Package name:_default' \
&& ret=0
;;
(install)
_arguments "${_arguments_options[@]}" : \
'-r+[Use a named registry from config]:REGISTRY:_default' \
'--registry=[Use a named registry from config]:REGISTRY:_default' \
'--locked[Require an up-to-date lock file]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(publish)
_arguments "${_arguments_options[@]}" : \
'-r+[Use a named registry from config]:REGISTRY:_default' \
'--registry=[Use a named registry from config]:REGISTRY:_default' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(search)
_arguments "${_arguments_options[@]}" : \
'--category=[Filter by category]:CATEGORY:_default' \
'--page=[Page number (1-based)]:PAGE:_default' \
'--per-page=[Results per page]:PER_PAGE:_default' \
'-r+[Use a named registry from config]:REGISTRY:_default' \
'--registry=[Use a named registry from config]:REGISTRY:_default' \
'-h[Print help]' \
'--help[Print help]' \
':query -- Search query:_default' \
&& ret=0
;;
(info)
_arguments "${_arguments_options[@]}" : \
'-r+[Use a named registry from config]:REGISTRY:_default' \
'--registry=[Use a named registry from config]:REGISTRY:_default' \
'-h[Print help]' \
'--help[Print help]' \
':package -- Package name:_default' \
&& ret=0
;;
(tree)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(update)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
'::package -- Package to update (all if omitted):_default' \
&& ret=0
;;
(remove)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
':package -- Package name:_default' \
&& ret=0
;;
(check)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(outdated)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(login)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(logout)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(key)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
":: :_adze__key_commands" \
"*::: :->key" \
&& ret=0

    case $state in
    (key)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-key-command-$line[1]:"
        case $line[1] in
            (generate)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(info)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
':fingerprint -- Key fingerprint (e.g. SHA256\:...):_default' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_adze__key__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-key-help-command-$line[1]:"
        case $line[1] in
            (generate)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(info)
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
(namespace)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
":: :_adze__namespace_commands" \
"*::: :->namespace" \
&& ret=0

    case $state in
    (namespace)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-namespace-command-$line[1]:"
        case $line[1] in
            (register)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
':prefix -- Namespace prefix to claim:_default' \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(info)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
':prefix -- Namespace prefix to look up:_default' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_adze__namespace__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-namespace-help-command-$line[1]:"
        case $line[1] in
            (register)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(info)
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
(yank)
_arguments "${_arguments_options[@]}" : \
'--reason=[Reason for yanking]:REASON:_default' \
'--undo[Undo a previous yank]' \
'-h[Print help]' \
'--help[Print help]' \
':version -- Version to yank:_default' \
&& ret=0
;;
(registry-key)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(deprecate)
_arguments "${_arguments_options[@]}" : \
'--message=[Deprecation message]:MESSAGE:_default' \
'--successor=[Suggested replacement package]:SUCCESSOR:_default' \
'--undo[Undo deprecation]' \
'-h[Print help]' \
'--help[Print help]' \
'::package -- Package to deprecate (defaults to current project):_default' \
&& ret=0
;;
(index)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
":: :_adze__index_commands" \
"*::: :->index" \
&& ret=0

    case $state in
    (index)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-index-command-$line[1]:"
        case $line[1] in
            (sync)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(resolve)
_arguments "${_arguments_options[@]}" : \
'--version=[Version requirement]:VERSION:_default' \
'-h[Print help]' \
'--help[Print help]' \
':package -- Package name:_default' \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
':package -- Package name:_default' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_adze__index__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-index-help-command-$line[1]:"
        case $line[1] in
            (sync)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(resolve)
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
(completions)
_arguments "${_arguments_options[@]}" : \
'-h[Print help]' \
'--help[Print help]' \
':shell -- Shell to generate completions for:(bash zsh fish powershell)' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_adze__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-help-command-$line[1]:"
        case $line[1] in
            (init)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(add)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(install)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(publish)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(search)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(info)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(tree)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(update)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(remove)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(check)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(outdated)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(login)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(logout)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(key)
_arguments "${_arguments_options[@]}" : \
":: :_adze__help__key_commands" \
"*::: :->key" \
&& ret=0

    case $state in
    (key)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-help-key-command-$line[1]:"
        case $line[1] in
            (generate)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(info)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
(namespace)
_arguments "${_arguments_options[@]}" : \
":: :_adze__help__namespace_commands" \
"*::: :->namespace" \
&& ret=0

    case $state in
    (namespace)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-help-namespace-command-$line[1]:"
        case $line[1] in
            (register)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(info)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
(yank)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(registry-key)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(deprecate)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(index)
_arguments "${_arguments_options[@]}" : \
":: :_adze__help__index_commands" \
"*::: :->index" \
&& ret=0

    case $state in
    (index)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:adze-help-index-command-$line[1]:"
        case $line[1] in
            (sync)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(resolve)
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
(completions)
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

(( $+functions[_adze_commands] )) ||
_adze_commands() {
    local commands; commands=(
'init:Create a new hew.toml in the current directory' \
'add:Add a dependency to hew.toml' \
'install:Install dependencies into .adze/packages/' \
'publish:Publish this package to the registry' \
'list:List packages in the local registry' \
'search:Search packages in the registry' \
'info:Show package info' \
'tree:Show dependency tree' \
'update:Update dependencies' \
'remove:Remove a dependency' \
'check:Validate manifest' \
'outdated:Show outdated dependencies' \
'login:Log in to the registry via GitHub' \
'logout:Log out from the registry' \
'key:Manage signing keys' \
'namespace:Manage namespace ownership' \
'yank:Yank a published version' \
'registry-key:Show the registry'\''s public signing key' \
'deprecate:Deprecate a package' \
'index:Manage local package index' \
'completions:Generate shell completion scripts' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'adze commands' commands "$@"
}
(( $+functions[_adze__add_commands] )) ||
_adze__add_commands() {
    local commands; commands=()
    _describe -t commands 'adze add commands' commands "$@"
}
(( $+functions[_adze__check_commands] )) ||
_adze__check_commands() {
    local commands; commands=()
    _describe -t commands 'adze check commands' commands "$@"
}
(( $+functions[_adze__completions_commands] )) ||
_adze__completions_commands() {
    local commands; commands=()
    _describe -t commands 'adze completions commands' commands "$@"
}
(( $+functions[_adze__deprecate_commands] )) ||
_adze__deprecate_commands() {
    local commands; commands=()
    _describe -t commands 'adze deprecate commands' commands "$@"
}
(( $+functions[_adze__help_commands] )) ||
_adze__help_commands() {
    local commands; commands=(
'init:Create a new hew.toml in the current directory' \
'add:Add a dependency to hew.toml' \
'install:Install dependencies into .adze/packages/' \
'publish:Publish this package to the registry' \
'list:List packages in the local registry' \
'search:Search packages in the registry' \
'info:Show package info' \
'tree:Show dependency tree' \
'update:Update dependencies' \
'remove:Remove a dependency' \
'check:Validate manifest' \
'outdated:Show outdated dependencies' \
'login:Log in to the registry via GitHub' \
'logout:Log out from the registry' \
'key:Manage signing keys' \
'namespace:Manage namespace ownership' \
'yank:Yank a published version' \
'registry-key:Show the registry'\''s public signing key' \
'deprecate:Deprecate a package' \
'index:Manage local package index' \
'completions:Generate shell completion scripts' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'adze help commands' commands "$@"
}
(( $+functions[_adze__help__add_commands] )) ||
_adze__help__add_commands() {
    local commands; commands=()
    _describe -t commands 'adze help add commands' commands "$@"
}
(( $+functions[_adze__help__check_commands] )) ||
_adze__help__check_commands() {
    local commands; commands=()
    _describe -t commands 'adze help check commands' commands "$@"
}
(( $+functions[_adze__help__completions_commands] )) ||
_adze__help__completions_commands() {
    local commands; commands=()
    _describe -t commands 'adze help completions commands' commands "$@"
}
(( $+functions[_adze__help__deprecate_commands] )) ||
_adze__help__deprecate_commands() {
    local commands; commands=()
    _describe -t commands 'adze help deprecate commands' commands "$@"
}
(( $+functions[_adze__help__help_commands] )) ||
_adze__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'adze help help commands' commands "$@"
}
(( $+functions[_adze__help__index_commands] )) ||
_adze__help__index_commands() {
    local commands; commands=(
'sync:Sync the local package index from the registry' \
'resolve:Resolve a package version from the local index' \
'list:List all versions of a package in the local index' \
    )
    _describe -t commands 'adze help index commands' commands "$@"
}
(( $+functions[_adze__help__index__list_commands] )) ||
_adze__help__index__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze help index list commands' commands "$@"
}
(( $+functions[_adze__help__index__resolve_commands] )) ||
_adze__help__index__resolve_commands() {
    local commands; commands=()
    _describe -t commands 'adze help index resolve commands' commands "$@"
}
(( $+functions[_adze__help__index__sync_commands] )) ||
_adze__help__index__sync_commands() {
    local commands; commands=()
    _describe -t commands 'adze help index sync commands' commands "$@"
}
(( $+functions[_adze__help__info_commands] )) ||
_adze__help__info_commands() {
    local commands; commands=()
    _describe -t commands 'adze help info commands' commands "$@"
}
(( $+functions[_adze__help__init_commands] )) ||
_adze__help__init_commands() {
    local commands; commands=()
    _describe -t commands 'adze help init commands' commands "$@"
}
(( $+functions[_adze__help__install_commands] )) ||
_adze__help__install_commands() {
    local commands; commands=()
    _describe -t commands 'adze help install commands' commands "$@"
}
(( $+functions[_adze__help__key_commands] )) ||
_adze__help__key_commands() {
    local commands; commands=(
'generate:Generate a new Ed25519 signing keypair' \
'list:List registered signing keys' \
'info:Look up a signing key by fingerprint' \
    )
    _describe -t commands 'adze help key commands' commands "$@"
}
(( $+functions[_adze__help__key__generate_commands] )) ||
_adze__help__key__generate_commands() {
    local commands; commands=()
    _describe -t commands 'adze help key generate commands' commands "$@"
}
(( $+functions[_adze__help__key__info_commands] )) ||
_adze__help__key__info_commands() {
    local commands; commands=()
    _describe -t commands 'adze help key info commands' commands "$@"
}
(( $+functions[_adze__help__key__list_commands] )) ||
_adze__help__key__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze help key list commands' commands "$@"
}
(( $+functions[_adze__help__list_commands] )) ||
_adze__help__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze help list commands' commands "$@"
}
(( $+functions[_adze__help__login_commands] )) ||
_adze__help__login_commands() {
    local commands; commands=()
    _describe -t commands 'adze help login commands' commands "$@"
}
(( $+functions[_adze__help__logout_commands] )) ||
_adze__help__logout_commands() {
    local commands; commands=()
    _describe -t commands 'adze help logout commands' commands "$@"
}
(( $+functions[_adze__help__namespace_commands] )) ||
_adze__help__namespace_commands() {
    local commands; commands=(
'register:Register a custom namespace prefix' \
'list:List namespaces you own' \
'info:Show info about a namespace' \
    )
    _describe -t commands 'adze help namespace commands' commands "$@"
}
(( $+functions[_adze__help__namespace__info_commands] )) ||
_adze__help__namespace__info_commands() {
    local commands; commands=()
    _describe -t commands 'adze help namespace info commands' commands "$@"
}
(( $+functions[_adze__help__namespace__list_commands] )) ||
_adze__help__namespace__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze help namespace list commands' commands "$@"
}
(( $+functions[_adze__help__namespace__register_commands] )) ||
_adze__help__namespace__register_commands() {
    local commands; commands=()
    _describe -t commands 'adze help namespace register commands' commands "$@"
}
(( $+functions[_adze__help__outdated_commands] )) ||
_adze__help__outdated_commands() {
    local commands; commands=()
    _describe -t commands 'adze help outdated commands' commands "$@"
}
(( $+functions[_adze__help__publish_commands] )) ||
_adze__help__publish_commands() {
    local commands; commands=()
    _describe -t commands 'adze help publish commands' commands "$@"
}
(( $+functions[_adze__help__registry-key_commands] )) ||
_adze__help__registry-key_commands() {
    local commands; commands=()
    _describe -t commands 'adze help registry-key commands' commands "$@"
}
(( $+functions[_adze__help__remove_commands] )) ||
_adze__help__remove_commands() {
    local commands; commands=()
    _describe -t commands 'adze help remove commands' commands "$@"
}
(( $+functions[_adze__help__search_commands] )) ||
_adze__help__search_commands() {
    local commands; commands=()
    _describe -t commands 'adze help search commands' commands "$@"
}
(( $+functions[_adze__help__tree_commands] )) ||
_adze__help__tree_commands() {
    local commands; commands=()
    _describe -t commands 'adze help tree commands' commands "$@"
}
(( $+functions[_adze__help__update_commands] )) ||
_adze__help__update_commands() {
    local commands; commands=()
    _describe -t commands 'adze help update commands' commands "$@"
}
(( $+functions[_adze__help__yank_commands] )) ||
_adze__help__yank_commands() {
    local commands; commands=()
    _describe -t commands 'adze help yank commands' commands "$@"
}
(( $+functions[_adze__index_commands] )) ||
_adze__index_commands() {
    local commands; commands=(
'sync:Sync the local package index from the registry' \
'resolve:Resolve a package version from the local index' \
'list:List all versions of a package in the local index' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'adze index commands' commands "$@"
}
(( $+functions[_adze__index__help_commands] )) ||
_adze__index__help_commands() {
    local commands; commands=(
'sync:Sync the local package index from the registry' \
'resolve:Resolve a package version from the local index' \
'list:List all versions of a package in the local index' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'adze index help commands' commands "$@"
}
(( $+functions[_adze__index__help__help_commands] )) ||
_adze__index__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'adze index help help commands' commands "$@"
}
(( $+functions[_adze__index__help__list_commands] )) ||
_adze__index__help__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze index help list commands' commands "$@"
}
(( $+functions[_adze__index__help__resolve_commands] )) ||
_adze__index__help__resolve_commands() {
    local commands; commands=()
    _describe -t commands 'adze index help resolve commands' commands "$@"
}
(( $+functions[_adze__index__help__sync_commands] )) ||
_adze__index__help__sync_commands() {
    local commands; commands=()
    _describe -t commands 'adze index help sync commands' commands "$@"
}
(( $+functions[_adze__index__list_commands] )) ||
_adze__index__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze index list commands' commands "$@"
}
(( $+functions[_adze__index__resolve_commands] )) ||
_adze__index__resolve_commands() {
    local commands; commands=()
    _describe -t commands 'adze index resolve commands' commands "$@"
}
(( $+functions[_adze__index__sync_commands] )) ||
_adze__index__sync_commands() {
    local commands; commands=()
    _describe -t commands 'adze index sync commands' commands "$@"
}
(( $+functions[_adze__info_commands] )) ||
_adze__info_commands() {
    local commands; commands=()
    _describe -t commands 'adze info commands' commands "$@"
}
(( $+functions[_adze__init_commands] )) ||
_adze__init_commands() {
    local commands; commands=()
    _describe -t commands 'adze init commands' commands "$@"
}
(( $+functions[_adze__install_commands] )) ||
_adze__install_commands() {
    local commands; commands=()
    _describe -t commands 'adze install commands' commands "$@"
}
(( $+functions[_adze__key_commands] )) ||
_adze__key_commands() {
    local commands; commands=(
'generate:Generate a new Ed25519 signing keypair' \
'list:List registered signing keys' \
'info:Look up a signing key by fingerprint' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'adze key commands' commands "$@"
}
(( $+functions[_adze__key__generate_commands] )) ||
_adze__key__generate_commands() {
    local commands; commands=()
    _describe -t commands 'adze key generate commands' commands "$@"
}
(( $+functions[_adze__key__help_commands] )) ||
_adze__key__help_commands() {
    local commands; commands=(
'generate:Generate a new Ed25519 signing keypair' \
'list:List registered signing keys' \
'info:Look up a signing key by fingerprint' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'adze key help commands' commands "$@"
}
(( $+functions[_adze__key__help__generate_commands] )) ||
_adze__key__help__generate_commands() {
    local commands; commands=()
    _describe -t commands 'adze key help generate commands' commands "$@"
}
(( $+functions[_adze__key__help__help_commands] )) ||
_adze__key__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'adze key help help commands' commands "$@"
}
(( $+functions[_adze__key__help__info_commands] )) ||
_adze__key__help__info_commands() {
    local commands; commands=()
    _describe -t commands 'adze key help info commands' commands "$@"
}
(( $+functions[_adze__key__help__list_commands] )) ||
_adze__key__help__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze key help list commands' commands "$@"
}
(( $+functions[_adze__key__info_commands] )) ||
_adze__key__info_commands() {
    local commands; commands=()
    _describe -t commands 'adze key info commands' commands "$@"
}
(( $+functions[_adze__key__list_commands] )) ||
_adze__key__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze key list commands' commands "$@"
}
(( $+functions[_adze__list_commands] )) ||
_adze__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze list commands' commands "$@"
}
(( $+functions[_adze__login_commands] )) ||
_adze__login_commands() {
    local commands; commands=()
    _describe -t commands 'adze login commands' commands "$@"
}
(( $+functions[_adze__logout_commands] )) ||
_adze__logout_commands() {
    local commands; commands=()
    _describe -t commands 'adze logout commands' commands "$@"
}
(( $+functions[_adze__namespace_commands] )) ||
_adze__namespace_commands() {
    local commands; commands=(
'register:Register a custom namespace prefix' \
'list:List namespaces you own' \
'info:Show info about a namespace' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'adze namespace commands' commands "$@"
}
(( $+functions[_adze__namespace__help_commands] )) ||
_adze__namespace__help_commands() {
    local commands; commands=(
'register:Register a custom namespace prefix' \
'list:List namespaces you own' \
'info:Show info about a namespace' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'adze namespace help commands' commands "$@"
}
(( $+functions[_adze__namespace__help__help_commands] )) ||
_adze__namespace__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'adze namespace help help commands' commands "$@"
}
(( $+functions[_adze__namespace__help__info_commands] )) ||
_adze__namespace__help__info_commands() {
    local commands; commands=()
    _describe -t commands 'adze namespace help info commands' commands "$@"
}
(( $+functions[_adze__namespace__help__list_commands] )) ||
_adze__namespace__help__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze namespace help list commands' commands "$@"
}
(( $+functions[_adze__namespace__help__register_commands] )) ||
_adze__namespace__help__register_commands() {
    local commands; commands=()
    _describe -t commands 'adze namespace help register commands' commands "$@"
}
(( $+functions[_adze__namespace__info_commands] )) ||
_adze__namespace__info_commands() {
    local commands; commands=()
    _describe -t commands 'adze namespace info commands' commands "$@"
}
(( $+functions[_adze__namespace__list_commands] )) ||
_adze__namespace__list_commands() {
    local commands; commands=()
    _describe -t commands 'adze namespace list commands' commands "$@"
}
(( $+functions[_adze__namespace__register_commands] )) ||
_adze__namespace__register_commands() {
    local commands; commands=()
    _describe -t commands 'adze namespace register commands' commands "$@"
}
(( $+functions[_adze__outdated_commands] )) ||
_adze__outdated_commands() {
    local commands; commands=()
    _describe -t commands 'adze outdated commands' commands "$@"
}
(( $+functions[_adze__publish_commands] )) ||
_adze__publish_commands() {
    local commands; commands=()
    _describe -t commands 'adze publish commands' commands "$@"
}
(( $+functions[_adze__registry-key_commands] )) ||
_adze__registry-key_commands() {
    local commands; commands=()
    _describe -t commands 'adze registry-key commands' commands "$@"
}
(( $+functions[_adze__remove_commands] )) ||
_adze__remove_commands() {
    local commands; commands=()
    _describe -t commands 'adze remove commands' commands "$@"
}
(( $+functions[_adze__search_commands] )) ||
_adze__search_commands() {
    local commands; commands=()
    _describe -t commands 'adze search commands' commands "$@"
}
(( $+functions[_adze__tree_commands] )) ||
_adze__tree_commands() {
    local commands; commands=()
    _describe -t commands 'adze tree commands' commands "$@"
}
(( $+functions[_adze__update_commands] )) ||
_adze__update_commands() {
    local commands; commands=()
    _describe -t commands 'adze update commands' commands "$@"
}
(( $+functions[_adze__yank_commands] )) ||
_adze__yank_commands() {
    local commands; commands=()
    _describe -t commands 'adze yank commands' commands "$@"
}

if [ "$funcstack[1]" = "_adze" ]; then
    _adze "$@"
else
    compdef _adze adze
fi
