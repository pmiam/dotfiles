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

if [[ -x $commands[direnv] ]]; then
    eval "$(direnv hook zsh)"
fi
