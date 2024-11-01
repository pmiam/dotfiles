# configure interactive shell environment and features
# line editing behavior
bindkey -e
autoload -U select-word-style
select-word-style bash

# session-specific variables
export GPG_TTY=$(tty)
export EDITOR=ec

# options
unsetopt beep

# extensions
autoload -Uz compinit
compinit

if [[ -x $commands[fzf] ]]; then
    source <(fzf --zsh)
fi

# aliases
alias lgr="hledger"
alias papis='papis '
alias refgen='update -s ref.jinja2 "$(papis config -s lit --json | jq ".\"ref-format.jinja2\"")"'

if [[ -x $commands[direnv] ]]; then
    eval "$(direnv hook zsh)"
fi

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"
