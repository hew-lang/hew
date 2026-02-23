# Bash completion for the adze package manager
# Install: source this file, or copy to /etc/bash_completion.d/adze

_adze() {
    local cur prev words cword
    _init_completion || return

    local commands="init add install publish list search info tree update remove check outdated login logout key namespace yank registry-key deprecate index completions"
    local key_commands="generate list info"
    local namespace_commands="register list info"
    local index_commands="sync resolve list"

    # Complete subcommands at position 1
    if [[ $cword -eq 1 ]]; then
        COMPREPLY=($(compgen -W "$commands" -- "$cur"))
        return
    fi

    local cmd="${words[1]}"

    case "$cmd" in
        init)
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--lib --bin --actor" -- "$cur"))
                return
            fi
            ;;
        add)
            if [[ "$prev" == "--version" ]]; then
                return
            fi
            if [[ "$prev" == "-r" || "$prev" == "--registry" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--version --registry -r" -- "$cur"))
                return
            fi
            ;;
        install)
            if [[ "$prev" == "-r" || "$prev" == "--registry" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--locked --registry -r" -- "$cur"))
                return
            fi
            ;;
        publish)
            if [[ "$prev" == "-r" || "$prev" == "--registry" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--registry -r" -- "$cur"))
                return
            fi
            ;;
        search)
            if [[ "$prev" == "--category" || "$prev" == "--page" || "$prev" == "--per-page" ]]; then
                return
            fi
            if [[ "$prev" == "-r" || "$prev" == "--registry" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--category --page --per-page --registry -r" -- "$cur"))
                return
            fi
            ;;
        info)
            if [[ "$prev" == "-r" || "$prev" == "--registry" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--registry -r" -- "$cur"))
                return
            fi
            ;;
        update|remove)
            # These take a package name argument, no special completion
            ;;
        yank)
            if [[ "$prev" == "--reason" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--reason --undo" -- "$cur"))
                return
            fi
            ;;
        deprecate)
            if [[ "$prev" == "--message" || "$prev" == "--successor" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--message --successor --undo" -- "$cur"))
                return
            fi
            ;;
        key)
            if [[ $cword -eq 2 ]]; then
                COMPREPLY=($(compgen -W "$key_commands" -- "$cur"))
                return
            fi
            ;;
        namespace)
            if [[ $cword -eq 2 ]]; then
                COMPREPLY=($(compgen -W "$namespace_commands" -- "$cur"))
                return
            fi
            ;;
        index)
            if [[ $cword -eq 2 ]]; then
                COMPREPLY=($(compgen -W "$index_commands" -- "$cur"))
                return
            fi
            # index resolve --version
            if [[ "${words[2]}" == "resolve" ]]; then
                if [[ "$prev" == "--version" ]]; then
                    return
                fi
                if [[ "$cur" == -* ]]; then
                    COMPREPLY=($(compgen -W "--version" -- "$cur"))
                    return
                fi
            fi
            ;;
        completions)
            if [[ $cword -eq 2 ]]; then
                COMPREPLY=($(compgen -W "bash zsh fish" -- "$cur"))
            fi
            ;;
    esac
}

complete -F _adze adze
