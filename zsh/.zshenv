export HISTFILE=~/.zhistory
export HIST_IGNORE_DUPS=1
export HISTSIZE=1000
export SAVEHIST=10000

declare -U PATH path
declare -U FPATH fpath

LOCALBIN="$HOME/.local/bin"
if [[ -d $LOCALBIN ]] && ((!$path[(Ie)$LOCALBIN])); then
    export PATH="$LOCALBIN:$PATH"
fi

# XDG base directory specification
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_DATA_DIRS=${XGD_DATA_DIRS:="/usr/local/share:/usr/share"}
export XDG_CONFIG_DIRS=${XGD_CONFIG_DIRS:="/etc/xdg"}

# user variables
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export LEDGER_FILE="$HOME/fin/journal.lgr"

# relocate directories not compliant with XDG spec
export GNUPGHOME=${GNUPGHOME:="$XDG_CONFIG_HOME/gnupg"}
export GOPATH=${GOPATH:="$XDG_DATA_HOME"/go}
export GOMODCACHE=${GOMODCACHE:="$XDG_CACHE_HOME"/go-mod}
export GOBIN=${GOBIN:="$HOME/.local/bin"}
