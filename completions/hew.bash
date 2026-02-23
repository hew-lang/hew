# Bash completion for the hew compiler
# Install: source this file, or copy to /etc/bash_completion.d/hew

_hew() {
    local cur prev words cword
    _init_completion || return

    local commands="build run check watch doc eval test wire fmt init completions version help"
    local build_opts="-o --Werror --no-typecheck --emit-mlir --emit-llvm --emit-obj --target --pkg-path"
    local run_opts="-o --Werror --no-typecheck --pkg-path"

    # Complete subcommands at position 1
    if [[ $cword -eq 1 ]]; then
        COMPREPLY=($(compgen -W "$commands" -- "$cur"))
        # Also complete .hew files (shorthand for build)
        COMPREPLY+=($(compgen -f -X '!*.hew' -- "$cur"))
        COMPREPLY+=($(compgen -d -- "$cur"))
        return
    fi

    local cmd="${words[1]}"

    case "$cmd" in
        build|check)
            if [[ "$prev" == "-o" ]]; then
                _filedir
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "$build_opts" -- "$cur"))
                return
            fi
            COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
            COMPREPLY+=($(compgen -d -- "$cur"))
            ;;
        run)
            # After --, stop completing hew options
            local i
            for ((i = 1; i < cword; i++)); do
                if [[ "${words[i]}" == "--" ]]; then
                    _filedir
                    return
                fi
            done
            if [[ "$prev" == "-o" ]]; then
                _filedir
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "$run_opts --" -- "$cur"))
                return
            fi
            COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
            COMPREPLY+=($(compgen -d -- "$cur"))
            ;;
        fmt)
            COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
            COMPREPLY+=($(compgen -d -- "$cur"))
            ;;
        watch)
            if [[ "$prev" == "--debounce" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--run --clear --debounce --Werror --no-typecheck --pkg-path" -- "$cur"))
                return
            fi
            COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
            COMPREPLY+=($(compgen -d -- "$cur"))
            ;;
        init)
            # Project name or --force
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--force" -- "$cur"))
                return
            fi
            ;;
        wire)
            if [[ $cword -eq 2 ]]; then
                COMPREPLY=($(compgen -W "check" -- "$cur"))
                return
            fi
            # wire check <file> --against <file>
            if [[ "$prev" == "--against" ]]; then
                COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
                COMPREPLY+=($(compgen -d -- "$cur"))
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--against" -- "$cur"))
                return
            fi
            COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
            COMPREPLY+=($(compgen -d -- "$cur"))
            ;;
        doc)
            if [[ "$prev" == "--output-dir" || "$prev" == "-o" ]]; then
                _filedir -d
                return
            fi
            if [[ "$prev" == "--format" || "$prev" == "-f" ]]; then
                COMPREPLY=($(compgen -W "html markdown" -- "$cur"))
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--output-dir --format --open" -- "$cur"))
                return
            fi
            COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
            COMPREPLY+=($(compgen -d -- "$cur"))
            ;;
        eval)
            if [[ "$prev" == "-f" ]]; then
                COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
                COMPREPLY+=($(compgen -d -- "$cur"))
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "-f" -- "$cur"))
                return
            fi
            ;;
        test)
            if [[ "$prev" == "--filter" ]]; then
                return
            fi
            if [[ "$cur" == -* ]]; then
                COMPREPLY=($(compgen -W "--filter --no-color --include-ignored" -- "$cur"))
                return
            fi
            COMPREPLY=($(compgen -f -X '!*.hew' -- "$cur"))
            COMPREPLY+=($(compgen -d -- "$cur"))
            ;;
        completions)
            if [[ $cword -eq 2 ]]; then
                COMPREPLY=($(compgen -W "bash zsh fish" -- "$cur"))
            fi
            ;;
    esac
}

complete -F _hew hew
